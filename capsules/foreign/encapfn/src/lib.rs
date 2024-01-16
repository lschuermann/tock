#![no_std]
// Required for the Cortex-M runtime
#![feature(naked_functions)]

#[cfg(feature = "mock-rt")]
extern crate std;

pub mod abi;
pub mod binary;
pub mod branding;
pub mod tock_cortexm_c_rt;
pub mod tock_rv32i_c_rt;
pub mod types;

#[cfg(feature = "mock-rt")]
pub mod mock_rt;

use abi::EncapfnABI;
use branding::EFID;
use types::{AccessScope, AllocScope, AllocTracker, EFAllocation};

#[macro_export]
macro_rules! ef_struct_getter {
    ($field_name:ident, $field_ty:ty) => {
        pub fn $field_name<'alloc, 'access, ID: ::encapfn::branding::EFID>(
            r: EFMutVal<'alloc, 'access, ID, Self>,
        ) -> EFMutRef<'alloc, ID, $field_ty> {
            let struct_ptr: *mut Self = r.as_ref().as_ptr().into();
            unsafe {
                let field_ptr: *mut $field_ty = core::ptr::addr_of_mut!((*struct_ptr).$field_name);
                let ef_ptr = EFPtr::from(field_ptr as *mut $field_ty);
                ef_ptr.upgrade_unchecked()
            }
        }
    };
}

#[derive(Copy, Clone, Debug)]
pub enum EFError {
    AllocNoMem,
}

pub trait EncapfnRt {
    type TargetABI: EncapfnABI;
    type ID: EFID;
    type AllocTracker: AllocTracker;

    fn allocate_stacked<F, R>(&self, size: usize, align: usize, fun: F) -> Result<R, EFError>
    where
        F: FnOnce(*mut u8) -> R;

    fn allocate_stacked_t<'a, T: Sized, F, R>(
        &self,
        _alloc_scope: &'a mut AllocScope<Self::AllocTracker, Self::ID>,
        fun: F,
    ) -> Result<R, EFError>
    where
        F: FnOnce(
            EFAllocation<'_, Self::ID, T>,
            &mut AllocScope<Self::AllocTracker, Self::ID>,
        ) -> R;

    fn allocate_stacked_array<'a, const N: usize, T: Sized, F, R>(
        &self,
        _alloc_scope: &'a mut AllocScope<Self::AllocTracker, Self::ID>,
        fun: F,
    ) -> Result<R, EFError>
    where
        F: FnOnce(*mut [T; N], &mut AllocScope<Self::AllocTracker, Self::ID>) -> R;

    fn allocate_stacked_slice<'a, T, F, R>(
        &self,
        len: usize,
        _alloc_scope: &'a mut AllocScope<Self::AllocTracker, Self::ID>,
        fun: F,
    ) -> Result<R, EFError>
    where
        F: FnOnce(*mut [T], &mut AllocScope<Self::AllocTracker, Self::ID>) -> R;

    fn resolve_function_pointer(&self, function_index: usize) -> Option<*const fn()>;

    // TODO: make private, provide a higher-level interface
    fn invoke_service<'a>(
        &self,
        fun: *const fn(),
        params: <Self::TargetABI as EncapfnABI>::ParametersContainer,
        access_scope: &mut AccessScope<Self::ID>,
    ) -> Result<(usize, usize), ()>;
}
