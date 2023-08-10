#![no_std]

pub mod binary;
pub mod rt;
pub mod types;

#[macro_export]
macro_rules! ef_struct_getter {
    ($field_name:ident, $field_ty:ty) => {
        pub fn $field_name<'alloc, 'access>(
            r: EFMutRefVal<'alloc, 'access, Self>,
            alloc_scope: &'alloc AllocScope,
        ) -> EFMutRef<'alloc, $field_ty> {
            let struct_ptr: *mut Self = EFMutRef::from(r.clone()).into_ptr().as_t_ptr_mut();
            unsafe {
                let field_ptr: *mut EFMutTy<$field_ty> =
                    core::ptr::addr_of_mut!((*struct_ptr).$field_name);
                let ef_ptr = EFMutPtr::from_t_ptr(field_ptr as *mut $field_ty);
                ef_ptr.into_ref_unchecked(alloc_scope)
            }
        }
    };
}
