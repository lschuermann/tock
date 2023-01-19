//! Hardware-independent kernel interface for deferred calls
//!
//! This allows any struct in the kernel which implements
//! [DynamicDeferredCallClient](crate::dynamic_deferred_call::DynamicDeferredCallClient)
//! to set and receive deferred calls.
//!
//! These can be used to implement long-running in-kernel algorithms
//! or software devices that are supposed to work like hardware devices.
//! Essentially, this allows the chip to handle more important interrupts,
//! and lets a kernel component return the function call stack up to the scheduler,
//! automatically being called again.
//!
//! Usage
//! -----
//!
//! The `dynamic_deferred_call_clients` array size determines how many
//! [DeferredCallHandle](crate::dynamic_deferred_call::DeferredCallHandle)s
//! may be registered with the instance.
//! When no more slots are available,
//! `dynamic_deferred_call.register(some_client)` will return `None`.
//!
//! ```
//! # use core::cell::Cell;
//! # use kernel::utilities::cells::OptionalCell;
//! # use kernel::static_init;
//! use kernel::dynamic_deferred_call::{
//!     DynamicDeferredCall,
//!     DynamicDeferredCallClient,
//!     DynamicDeferredCallClientState,
//! };
//!
//! let dynamic_deferred_call_clients = unsafe { static_init!(
//!     [DynamicDeferredCallClientState; 2],
//!     Default::default()
//! ) };
//! let dynamic_deferred_call = unsafe { static_init!(
//!     DynamicDeferredCall,
//!     DynamicDeferredCall::new(dynamic_deferred_call_clients)
//! ) };
//! assert!(unsafe { DynamicDeferredCall::set_global_instance(dynamic_deferred_call) } == true);
//!
//! # struct SomeCapsule;
//! # impl SomeCapsule {
//! #     pub fn new(_ddc: &'static DynamicDeferredCall) -> Self { SomeCapsule }
//! #     pub fn set_deferred_call_handle(
//! #         &self,
//! #         _handle: kernel::dynamic_deferred_call::DeferredCallHandle,
//! #     ) { }
//! # }
//! # impl DynamicDeferredCallClient for SomeCapsule {
//! #     fn call(
//! #         &self,
//! #         _handle: kernel::dynamic_deferred_call::DeferredCallHandle,
//! #     ) { }
//! # }
//! #
//! // Here you can register custom capsules, etc.
//! // This could look like:
//! let some_capsule = unsafe { static_init!(
//!     SomeCapsule,
//!     SomeCapsule::new(dynamic_deferred_call)
//! ) };
//! some_capsule.set_deferred_call_handle(
//!     dynamic_deferred_call.register(some_capsule).unwrap() // Unwrap fail = no deferred call slot available
//! );
//! ```

use core::cell::Cell;
use core::marker::PhantomData;

use crate::utilities::cells::OptionalCell;

/// Kernel-global dynamic deferred call instance
///
/// This gets called by the kernel scheduler automatically and is accessible
/// through `unsafe` static functions on the `DynamicDeferredCall` struct
static mut GLOBAL_REGISTRY: Option<&'static DeferredCallRegistry> = None;

pub trait DeferredCallClient {
    fn handle_deferred_call(&self);
}

// Rather than use a trait object, which will include a 20 byte vtable per instance, we
// implement a lighter weight alternative that only stores the data and function pointer.
#[derive(Copy, Clone)]
struct DynDefCallRef<'a> {
    data: *const (),
    callback: fn(*const ()),
    _lifetime: PhantomData<&'a ()>,
}

impl<'a> DynDefCallRef<'a> {
    // SAFETY: We define the callback function as being a closure which casts
    // the passed pointer to be the appropriate type (a pointer to `T`)
    // and then calls `T::handle_deferred_call()`. In practice, the closure
    // is optimized away by LLVM when the ABI of the closure and the underlying function
    // are identical, making this zero-cost, but saving us from having to trust
    // that `fn(*const ())` and `fn handle_deferred_call(&self)` will always have the same calling
    // convention for any type.
    fn new<T: DeferredCallClient>(x: &'a T) -> Self {
        Self {
            data: x as *const _ as *const (),
            callback: |p| unsafe { T::handle_deferred_call(&*p.cast()) },
            _lifetime: PhantomData,
        }
    }
}

impl DynDefCallRef<'_> {
    // more efficient pass by `self` if we don't have to implement `DeferredCallClient` directly
    fn handle_deferred_call(self) {
        (self.callback)(self.data)
    }
}

pub struct DeferredCallToken(usize);

impl DeferredCallToken {
    pub fn register<DC: DeferredCallClient>(self, client: &'static DC) -> DeferredCall {
	// A [`DeferredCallToken`] can only be created through
	// [`DeferredCallRegistry::initialize`], which registers this registry
	// as the global instance. Hence the following unwrap is fine here.
	//
	// We further convert the passed generic client argument to a
	// `DynDefCallRef` here, to ensure that
	// DeferredCallRegistry::register_client is not monomorphized.
	(unsafe { GLOBAL_REGISTRY }).unwrap().register_client(self, DynDefCallRef::new(client))
    }
}

/// Deferred call registry
///
/// This struct manages and calls dynamically (at runtime) registered
/// deferred calls from capsules and other kernel structures.
///
/// It has a fixed number of possible clients, which
/// is determined by the `clients`-array passed in with the constructor.
pub struct DeferredCallRegistry {
    clients: [OptionalCell<DynDefCallRef<'static>>; 32],
    call_mask: Cell<u32>,
}

impl DeferredCallRegistry {
    /// Construct a new dynamic deferred call implementation
    ///
    /// This needs to be registered with the `set_global_instance` function immediately
    /// afterwards, and should not be changed anymore. Only the globally registered
    /// instance will receive calls from the kernel scheduler.
    ///
    /// The `clients` array can be initialized using the implementation of [Default]
    /// for the [DynamicDeferredCallClientState].
    pub fn new() -> Self {
        DeferredCallRegistry {
            clients: Default::default(),
	    call_mask: Cell::new(0),
        }
    }

    pub unsafe fn initialize(&'static self) -> Option<[DeferredCallToken; 32]> {
	if GLOBAL_REGISTRY.is_some() {
	    // Initialize can only be called once per running kernel
	    None
	} else {
	    GLOBAL_REGISTRY = Some(self);

	    // Now create an array of tokens which allow clients to register
	    // [`DeferredCall`]s. This is pretty ugly right now, but Rust
	    // doesn't give us any more convient option to initialize an array
	    // of non-Default things:
	    let mut tokens = [
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
		DeferredCallToken(0), DeferredCallToken(0),
	    ];

	    // Pretend as if that ugly bit of code above didn't exist and
	    // programmatically initialize the respective tokens, ensuring that
	    // their pre-defined deferred call slot index is unique:
	    for (idx, token) in tokens.iter_mut().enumerate() {
		token.0 = idx;
	    }

	    Some(tokens)
	}
    }

    fn register_client(&self, token: DeferredCallToken, client: DynDefCallRef<'static>) -> DeferredCall {
	let DeferredCallToken(client_index) = token;
	self.clients[client_index].replace(client);
	DeferredCall(client_index)
    }

    /// Call the globally registered instance
    ///
    /// Returns `true` if a global instance was registered and has been called.
    pub unsafe fn call_global_instance() -> bool {
        GLOBAL_REGISTRY.map(|ddc| ddc.call()).is_some()
    }

    /// Call the globally registered instance while the supplied predicate
    /// returns `true`.
    ///
    /// Returns `true` if a global instance was registered and has been called.
    pub unsafe fn call_global_instance_while<F: Fn() -> bool>(f: F) -> bool {
        GLOBAL_REGISTRY
            .map(move |ddc| ddc.call_while(f))
            .is_some()
    }

    /// Check if one or more dynamic deferred calls are pending in the
    /// globally registered instance
    ///
    /// Returns `None` if no global instance has been registered, or `Some(true)`
    /// if the registered instance has one or more pending deferred calls.
    pub unsafe fn global_instance_calls_pending() -> Option<bool> {
        GLOBAL_REGISTRY.map(|ddc| ddc.has_pending())
    }

    /// Schedule a deferred call to be called
    ///
    /// The handle addresses the client that will be called.
    ///
    /// If no client for the handle is found (it was unregistered), this
    /// returns `None`. If a call is already scheduled, it returns
    /// `Some(false)`.
    fn set(&self, dc: DeferredCall) {
	let DeferredCall(bitmask_idx) = dc;
	self.call_mask.set(self.call_mask.get() | (1 << bitmask_idx))
    }

    /// Check if one or more deferred calls are pending
    ///
    /// Returns `true` if one or more deferred calls are pending.
    pub fn has_pending(&self) -> bool {
        self.call_mask.get() != 0
    }

    /// Call all registered and to-be-scheduled deferred calls
    ///
    /// It may be called without holding the `DynamicDeferredCall` reference through
    /// `call_global_instance`.
    pub(self) fn call(&self) {
        self.call_while(|| true)
    }

    /// Call all registered and to-be-scheduled deferred calls while the supplied
    /// predicate returns `true`.
    ///
    /// It may be called without holding the `DynamicDeferredCall` reference through
    /// `call_global_instance_while`.
    pub(self) fn call_while<F: Fn() -> bool>(&self, _f: F) {
	// For now, we just call a single deferred call. This infrastructure
	// should be refactored to behave more like the `service_next_pending`
	// on regular deferred calls anyways:
	let mask = self.call_mask.get();
        if mask != 0 {
            let bit = mask.trailing_zeros() as usize;
            self.call_mask.set(mask & !(1 << bit));
	    // unwrap_or_panic is safe to use here, as the only way to
	    // schedule a deferred call is through a `DeferredCall`
	    // instance, which can only be created in exchange for a
	    // `DeferredCallToken` and registering a client in the process.
            self.clients[bit].unwrap_or_panic().handle_deferred_call();
        }
    }
}

/// Unique identifier for a deferred call registered with a
/// [DynamicDeferredCall](crate::dynamic_deferred_call::DynamicDeferredCall)
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct DeferredCall(usize);

impl DeferredCall {
    pub fn set(&self) {
	(unsafe { GLOBAL_REGISTRY }).map(|dc| dc.set(*self));
    }
}
