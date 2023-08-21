use crate::bindgen;
use encapfn::ef_struct_getter;
use encapfn::types::*;

// TODO: we should implement some test / compile time assertion to
// verify that this has a compatible type layout to the C struct, as
// bindgen does.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct HmacContext {
    pub data: [u32; 42usize],
}

unsafe impl EFType for HmacContext {
    fn validate(_t: *mut Self) -> bool {
        true
    }
}

impl HmacContext {
    ef_struct_getter!(data, [u32; 42usize]);
}
