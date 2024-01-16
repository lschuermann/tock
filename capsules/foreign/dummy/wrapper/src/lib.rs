#![no_std]

#[allow(dead_code)]
mod bindgen;

pub mod wrapped;

pub fn test_add<
    ID: encapfn::branding::EFID,
    ABI: encapfn::abi::Encapfn4WRegABI,
    RT: encapfn::EncapfnRt<ID = ID, TargetABI = ABI>,
>(
    rt: &RT,
    _alloc_scope: &mut encapfn::types::AllocScope<RT::AllocTracker, ID>,
    access_scope: &mut encapfn::types::AccessScope<ID>,
    a: ::core::ffi::c_int,
    b: ::core::ffi::c_int,
) -> Result<::core::ffi::c_int, ()> {
    rt.invoke_service(
        rt.resolve_function_pointer(wrapped::ExportedFunctions::TestAdd as usize)
            .ok_or(())?,
        ABI::encode_2w(a as usize, b as usize),
        access_scope,
    )
    .map(|(res, _)| res as ::core::ffi::c_int)
}

#[inline(never)]
pub fn call_test_pointer_manipulation<
    ID: encapfn::branding::EFID,
    ABI: encapfn::abi::Encapfn4WRegABI,
    RT: encapfn::EncapfnRt<ID = ID, TargetABI = ABI>,
>(
    rt: &RT,
    alloc_scope: &mut encapfn::types::AllocScope<RT::AllocTracker, ID>,
    access_scope: &mut encapfn::types::AccessScope<ID>,
) -> u32 {
    rt.allocate_stacked_t::<wrapped::PointerDemoStruct, _, _>(
        alloc_scope,
        |dummystruct_alloc, alloc_scope| {
            let dummystruct_ref = dummystruct_alloc.into_ref(&alloc_scope);
            let dummystruct_ptr = dummystruct_ref.as_ptr();

            rt.invoke_service(
                // test_pointer_manipulation(struct pointer_demo_struct*)
                rt.resolve_function_pointer(
                    wrapped::ExportedFunctions::TestPointerManipulation as usize,
                )
                .unwrap(),
                ABI::encode_1w(
                    Into::<*mut wrapped::PointerDemoStruct>::into(dummystruct_ptr) as usize,
                ),
                access_scope,
            )
            .unwrap();

            // let alloc_scope = encapfn::wrapped::AllocScope::new();
            // let dummystruct_ref = dummystruct_ptr.into_ref(&alloc_scope).unwrap();
            let dummystruct_ref_val = dummystruct_ref.validate(&access_scope).unwrap();

            // let dummystruct_ref_val = initialized;

            let string_length: u32 = *wrapped::PointerDemoStruct::some_number(dummystruct_ref_val)
                .validate(&access_scope)
                .unwrap();

            let _string_bytes: &[i8] =
                &(*wrapped::PointerDemoStruct::some_char_ptr(dummystruct_ref_val)
                    .validate(&access_scope)
                    .unwrap())
                .upgrade_slice_mut(string_length as usize, &alloc_scope)
                .unwrap()
                .validate(&access_scope)
                .unwrap();

            // panic!("Message: {}", unsafe { core::str::from_utf8_unchecked(core::mem::transmute(string_bytes)) });

            string_length
        },
    )
    .unwrap()
}
