use core::cell::Cell;
use core::cell::RefCell;
use core::marker::PhantomData;
use core::mem::MaybeUninit;
use core::ops::Range;

use std::collections::LinkedList;
use std::rc::Rc;

use kernel::platform::mpu::{self, MPU};

use crate::abi::{mock::MockABI, EncapfnABI};
use crate::binary::EncapfnBinary;
use crate::branding::EFID;
use crate::types::{AccessScope, AllocScope, AllocTracker, EFAllocation};
use crate::{EFError, EncapfnRt};

pub struct EncapfnMockRt<ID: EFID> {
    _id: PhantomData<ID>,
    allocs: Rc<RefCell<LinkedList<Range<*mut u8>>>>,
}

#[derive(Clone)]
pub struct EncapfnMockRtAllocTracker(Rc<RefCell<LinkedList<Range<*mut u8>>>>);

impl AllocTracker for EncapfnMockRtAllocTracker {}

impl<ID: EFID> EncapfnMockRt<ID> {
    /// Create a new containerized service instance from a binary loaded into an
    /// accessible memory region.
    ///
    /// This method assumes that the kernel has (at least read) access to the
    /// full binary. Callers need to ensure that any memory protection systems
    /// are set up accordingly.
    ///
    // TODO: switch to raw slices
    //
    // TODO: make the loading process work with PIC binaries
    pub unsafe fn new(
        _id: ID,
    ) -> (
        Self,
        AllocScope<EncapfnMockRtAllocTracker, ID>,
        AccessScope<ID>,
    ) {
        let allocs = Rc::new(RefCell::new(LinkedList::new()));

        (
            EncapfnMockRt {
                _id: PhantomData,
                allocs: allocs.clone(),
            },
            unsafe { AllocScope::new(EncapfnMockRtAllocTracker(allocs)) },
            unsafe { AccessScope::new() },
        )
    }
}

impl<ID: EFID> EncapfnRt for EncapfnMockRt<ID> {
    type TargetABI = MockABI;
    type ID = ID;
    type AllocTracker = EncapfnMockRtAllocTracker;

    fn allocate_stacked<F, R>(&self, size: usize, align: usize, fun: F) -> Result<R, EFError>
    where
        F: FnOnce(*mut u8) -> R,
    {
        assert!(size > 0 && align > 0);

        // Fake a separate stack allocator and place all allocations on the Rust
        // heap. Store all required components to be able to reconstruct the Vec
        // when deallocating this memory. We add `align - 1` to guarantee that
        // we can seek an aligned allocation in this byte-vec.
        let len = size + align - 1;
        let (ptr, vec_len, cap) = (vec![0_u8; len]).into_raw_parts();
        debug_assert!(len == vec_len);

        // Create an aligned start and end pointer:
        let aligned_start = unsafe { ptr.add(ptr.align_offset(align)) };
        let aligned_end = unsafe { aligned_start.add(size) };

        // Add this array to the list of valid allocations:
        (*self.allocs.borrow_mut()).push_back(Range {
            start: aligned_start,
            end: aligned_end,
        });

        // Now it's safe to provide this allocation to the closure:
        let res = fun(aligned_start);

        // Function returned, free the allocation. We're handing out a
        // byte-array and hence don't have to worry about calling any
        // destructor, etc.
        unsafe { Vec::<u8>::from_raw_parts(ptr, len, cap) };

        // We're done here:
        Ok(res)
    }

    fn allocate_stacked_t<'a, T: Sized, F, R>(
        &self,
        alloc_scope: &'a mut AllocScope<Self::AllocTracker, ID>,
        fun: F,
    ) -> Result<R, EFError>
    where
        F: FnOnce(EFAllocation<'_, ID, T>, &mut AllocScope<Self::AllocTracker, ID>) -> R,
    {
        self.allocate_stacked(
            core::mem::size_of::<T>(),
            core::mem::align_of::<T>(),
            |allocated_ptr| {
                fun(
                    unsafe { EFAllocation::from_allocated_ptr(allocated_ptr) },
                    &mut unsafe { AllocScope::new(alloc_scope.tracker().clone()) },
                )
            },
        )
    }

    fn allocate_stacked_array<'a, const N: usize, T: Sized, F, R>(
        &self,
        alloc_scope: &'a mut AllocScope<Self::AllocTracker, ID>,
        fun: F,
    ) -> Result<R, EFError>
    where
        F: FnOnce(*mut [T; N], &mut AllocScope<Self::AllocTracker, ID>) -> R,
    {
        self.allocate_stacked(
            core::mem::size_of::<T>() * N,
            core::mem::align_of::<T>(),
            |allocated_ptr| {
                fun(allocated_ptr as *mut [T; N], &mut unsafe {
                    AllocScope::new(alloc_scope.tracker().clone())
                })
            },
        )
    }

    fn allocate_stacked_slice<'a, T, F, R>(
        &self,
        len: usize,
        alloc_scope: &'a mut AllocScope<Self::AllocTracker, ID>,
        fun: F,
    ) -> Result<R, EFError>
    where
        F: FnOnce(*mut [T], &mut AllocScope<Self::AllocTracker, ID>) -> R,
    {
        self.allocate_stacked(
            core::mem::size_of::<T>() * len,
            core::mem::align_of::<T>(),
            |allocated_ptr| {
                fun(
                    unsafe { core::slice::from_raw_parts_mut(allocated_ptr as *mut T, len) }
                        as *mut [T],
                    &mut unsafe { AllocScope::new(alloc_scope.tracker().clone()) },
                )
            },
        )
    }

    fn resolve_function_pointer(&self, function_index: usize) -> Option<*const fn()> {
        unimplemented!();
    }

    fn invoke_service<'a>(
        &self,
        fun: *const fn(),
        params: <Self::TargetABI as EncapfnABI>::ParametersContainer,
        _access_scope: &mut AccessScope<ID>,
    ) -> Result<(usize, usize), ()> {
        let [a0, a1, a2, a3] = params;
        Ok(unsafe {
            (core::mem::transmute::<
                *const fn(),
                extern "C" fn(usize, usize, usize, usize) -> (usize, usize),
            >(fun))(a0, a1, a2, a3)
        })
    }
}

#[cfg(test)]
pub mod test {
    pub extern "C" fn say_hello(bytes: *const [u8; 16]) {
        panic!("Hello World from extern fn: {:?}!", unsafe { &*bytes });
    }

    #[test]
    fn test_call() {
        crate::branding::new(|id| {
            use crate::{abi::Encapfn4WRegABI, EncapfnRt};

            let (rt, mut alloc_scope, mut access_scope) =
                unsafe { crate::mock_rt::EncapfnMockRt::new(id) };

            rt.allocate_stacked_t::<'_, [u8; 16], _, _>(&mut alloc_scope, |bytes, alloc_scope| {
                let val = bytes.initialize(
                    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
                    &alloc_scope,
                    &mut access_scope,
                );

                rt.invoke_service(
                    unsafe {
                        core::mem::transmute::<extern "C" fn(*const [u8; 16]), *const fn()>(
                            say_hello,
                        )
                    },
                    crate::abi::mock::MockABI::encode_1w(val.as_ref().as_ptr().into()),
                    &mut access_scope,
                );
            });
        });
    }
}
