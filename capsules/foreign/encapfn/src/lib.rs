#![no_std]

pub mod binary;
pub mod rt;
pub mod types;

#[macro_export]
macro_rules! ef_struct_getter {
    ($field_name:ident, $field_ty:ty) => {
        pub fn $field_name<'alloc, 'access>(
            r: EFMutVal<'alloc, 'access, Self>,
        ) -> EFMutRef<'alloc, $field_ty> {
            let struct_ptr: *mut Self = r.as_ref().as_ptr().into();
            unsafe {
                let field_ptr: *mut $field_ty =
                    core::ptr::addr_of_mut!((*struct_ptr).$field_name);
                let ef_ptr = EFMutPtr::from(field_ptr as *mut $field_ty);
                ef_ptr.upgrade_unchecked()
            }
        }
    };
}
