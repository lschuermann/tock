#![no_std]

mod bindgen;
pub mod wrapped;

#[inline(never)]
pub fn call_test_pointer_manipulation<C: kernel::platform::chip::Chip>(
    rt: &encapfn::rt::EncapfnRt<C>,
) -> usize {
    let (alloc_scope, access_scope) = rt.enter_scope();

    rt.allocate_stacked_t::<wrapped::PointerDemoStruct, _, _>(
        alloc_scope,
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
            let dummystruct_ptr = dummystruct_ref.into_ptr();

            let (res, access_scope) = rt.invoke_service(
                // test_pointer_manipulation(struct pointer_demo_struct*)
                rt.resolve_function_pointer(1).unwrap(),
                dummystruct_ptr.as_t_ptr() as usize,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                access_scope,
            );
            res.unwrap();

            // let alloc_scope = encapfn::wrapped::AllocScope::new();
            // let dummystruct_ref = dummystruct_ptr.into_ref(&alloc_scope).unwrap();
            let dummystruct_ref_val = dummystruct_ref
                .validate(&access_scope)
                .map_err(|_| ())
                .unwrap();

            // let dummystruct_ref_val = initialized;

            let string_length =
                wrapped::PointerDemoStruct::some_number(dummystruct_ref_val, &alloc_scope)
                    .validate_ref(&access_scope)
                    .unwrap()
                    .get() as usize;

            string_length
        },
    )
    .0
    .unwrap()
}
