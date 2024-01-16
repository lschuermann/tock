use encapfn::ef_struct_getter;
use encapfn::types::*;

#[repr(usize)]
pub enum ExportedFunctions {
    TestAdd = 0,
    TestPointerManipulation = 1,
    TestAddConst = 2,
}

#[repr(C)]
pub struct PointerDemoStruct {
    pub some_number: ::core::ffi::c_uint,
    pub some_char_ptr: EFPtr<::core::ffi::c_char>,
}

unsafe impl EFType for PointerDemoStruct {
    fn validate(_t: *mut Self) -> bool {
        true
    }
}

unsafe impl EFValDeref for PointerDemoStruct {}

impl PointerDemoStruct {
    ef_struct_getter!(some_number, ::core::ffi::c_uint);
    ef_struct_getter!(some_char_ptr, EFPtr<::core::ffi::c_char>);
}
