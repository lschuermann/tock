#![no_std]

mod bindgen;
pub mod wrapped;

#[inline(never)]
pub fn call_test_pointer_manipulation<C: kernel::platform::chip::Chip>(
    rt: &encapfn::rt::EncapfnRt<C>,
) -> u32 {
    let (mut alloc_scope, mut access_scope) = rt.enter_scope().unwrap();

    rt.allocate_stacked_t::<wrapped::PointerDemoStruct, _, _>(
        &mut alloc_scope,
        |dummystruct_alloc, alloc_scope| {
            // let initialized = dummystruct_alloc.initialize(
            //     wrapped::PointerDemoStruct {
            //         some_number: 42_u32.into(),
            //         some_char_ptr: encapfn::wrapped::EFMutPtr::null(),
            //     },
            //     &alloc_scope,
            //  &access_scope,
            // );

            // let dummystruct_ref = encapfn::wrapped::EFMutRef::from(initialized);

            let dummystruct_ref = dummystruct_alloc.into_ref(&alloc_scope);
            let dummystruct_ptr = dummystruct_ref.as_ptr();

            rt.invoke_service(
                // test_pointer_manipulation(struct pointer_demo_struct*)
                rt.resolve_function_pointer(1).unwrap(),
                Into::<*mut wrapped::PointerDemoStruct>::into(dummystruct_ptr) as usize,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                &mut access_scope,
            ).unwrap();

            // let alloc_scope = encapfn::wrapped::AllocScope::new();
            // let dummystruct_ref = dummystruct_ptr.into_ref(&alloc_scope).unwrap();
            let dummystruct_ref_val = dummystruct_ref
                .validate(&access_scope)
                .unwrap();

            // let dummystruct_ref_val = initialized;

            let string_length: u32 =
                *wrapped::PointerDemoStruct::some_number(dummystruct_ref_val)
                .validate(&access_scope)
                .unwrap();

	    let string_bytes: &[i8] =
		&(*wrapped::PointerDemoStruct::some_char_ptr(dummystruct_ref_val)
		.validate(&access_scope) .unwrap())
		.upgrade_slice(string_length as usize, &alloc_scope)
		.unwrap() .validate(&access_scope).unwrap();

	    // panic!("Message: {}", unsafe { core::str::from_utf8_unchecked(core::mem::transmute(string_bytes)) });

            string_length
        },
    )
    .unwrap()
}
