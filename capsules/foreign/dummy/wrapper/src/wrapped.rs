use crate::bindgen;
use encapfn::ef_struct_getter;
use encapfn::types::*;

#[repr(C)]
pub struct PointerDemoStruct {
    pub some_number: ::core::ffi::c_uint,
    pub some_char_ptr: EFMutPtr<::core::ffi::c_char>,
}

unsafe impl EFType for PointerDemoStruct {
    fn validate(t_: *mut Self) -> bool {
        true
    }
}

impl PointerDemoStruct {
    ef_struct_getter!(some_number, ::core::ffi::c_uint);
    ef_struct_getter!(some_char_ptr, EFMutPtr<::core::ffi::c_char>);
}
