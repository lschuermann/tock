use core::cell::UnsafeCell;
use core::intrinsics;
use core::marker::Copy;
use core::marker::PhantomData;
use core::marker::Sync;

/// AtomicUsize with no CAS operations that works on targets that have "no atomic
/// support" according to their specification. This makes it work on thumbv6
/// platforms.
///
/// Borrowed from https://github.com/japaric/heapless/blob/master/src/ring_buffer/mod.rs
/// See: https://github.com/japaric/heapless/commit/37c8b5b63780ed8811173dc1ec8859cd99efa9ad
pub(crate) struct AtomicUsize {
    v: UnsafeCell<usize>,
}

impl AtomicUsize {
    pub(crate) const fn new(v: usize) -> AtomicUsize {
        AtomicUsize {
            v: UnsafeCell::new(v),
        }
    }

    pub(crate) fn load_relaxed(&self) -> usize {
        unsafe { intrinsics::atomic_load_relaxed(self.v.get()) }
    }

    pub(crate) fn store_relaxed(&self, val: usize) {
        unsafe { intrinsics::atomic_store_relaxed(self.v.get(), val) }
    }

    pub(crate) fn fetch_or_relaxed(&self, val: usize) {
        unsafe { intrinsics::atomic_store_relaxed(self.v.get(), self.load_relaxed() | val) }
    }
}

unsafe impl Sync for AtomicUsize {}

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

// This is a 256 byte array, but at least resides in .bss
static mut DEFCALLS: [Option<DynDefCallRef<'static>>; 32] = [None; 32];

static CTR: AtomicUsize = AtomicUsize::new(0);
static BITMASK: AtomicUsize = AtomicUsize::new(0);

pub struct DeferredCall {
    idx: usize,
}

impl DeferredCall {
    pub unsafe fn new() -> Self {
        let idx = CTR.load_relaxed() + 1;
        CTR.store_relaxed(idx);
        DeferredCall { idx }
    }

    pub fn register<DC: DeferredCallClient>(&self, client: &'static DC) {
        let handler = DynDefCallRef::new(client);
        unsafe {
            DEFCALLS[self.idx] = Some(handler);
        }
    }

    pub fn set(&self) {
        BITMASK.fetch_or_relaxed(1 << self.idx);
    }

    /// Services and clears the next pending `DeferredCall`, returns which index
    /// was serviced
    pub fn service_next_pending() -> Option<usize> {
        let val = BITMASK.load_relaxed();
        if val == 0 {
            return None;
        } else {
            let bit = val.trailing_zeros() as usize;
            let new_val = val & !(1 << bit);
            BITMASK.store_relaxed(new_val);
            unsafe {
                DEFCALLS[bit].map(|dc| {
                    dc.handle_deferred_call();
                    bit
                })
            }
        }
    }

    pub fn has_tasks() -> bool {
        BITMASK.load_relaxed() != 0
    }
}
