use core::cell::UnsafeCell;
use core::marker::PhantomData;
use core::mem::MaybeUninit;
use core::ops::Deref;

pub struct AllocScope;
impl AllocScope {
    pub unsafe fn new() -> Self {
        AllocScope
    }
}

pub struct AccessScope;
impl AccessScope {
    pub unsafe fn new() -> Self {
        AccessScope
    }
}

pub unsafe trait EFType {
    // For primitives, verify that type layout is correct.
    fn validate(t: *mut Self) -> bool;
}

pub unsafe trait EFValDeref: EFType {}

// -----------------------------------------------------------------------------

#[derive(Debug)]
#[repr(transparent)]
pub struct EFMutPtr<T>(pub *mut T);

impl<T> Clone for EFMutPtr<T> {
    fn clone(&self) -> Self {
	*self
    }
}

impl<T> Copy for EFMutPtr<T> {}

impl<T> From<*mut T> for EFMutPtr<T> {
    fn from(ptr: *mut T) -> Self {
	EFMutPtr(ptr)
    }
}

impl<T> From<EFMutPtr<T>> for *mut T {
    fn from(efmutptr: EFMutPtr<T>) -> Self {
	efmutptr.0
    }
}

impl<T> EFMutPtr<T> {
    pub fn null() -> Self {
        EFMutPtr(core::ptr::null_mut())
    }

    pub unsafe fn upgrade_unchecked<'alloc>(&self) -> EFMutRef<'alloc, T> {
	EFMutRef(&*(self.0 as *mut UnsafeCell<MaybeUninit<T>> as *const _))
    }

    // pub fn into_ref<'a>(self, _alloc_scope: &'a AllocScope) -> Option<EFMutRef<'a, T>> {
    //     // TODO: check that object pointed to is in mutably accessible
    //     // memory. We can probably put some associated data into the AllocScope
    //     // for this check, or have that reference the EncapfnRt.
    //     Some(EFMutRef {
    //         ptr: self,
    //         _lt: PhantomData,
    //     })
    // }

    pub fn upgrade<'alloc>(
        &self, _alloc_scope: &'alloc AllocScope,
    ) -> Option<EFMutRef<'alloc, T>> {
        // TODO: check that object pointed to is in mutably accessible
        // memory, and well-aligned.

        Some(unsafe { self.upgrade_unchecked() })
	
    }

    pub unsafe fn upgrade_slice_unchecked<'alloc>(
	&self, length: usize,
    ) -> EFMutSlice<'alloc, T> {
	// TODO: this is unsound. It forms an intermediate slice reference,
	// which is not yet contained in the `UnsafeCell<MaybeUninit<T>>`
	// container.
	EFMutSlice(
	    core::slice::from_raw_parts_mut(self.0 as *mut _ as *mut UnsafeCell<MaybeUninit<T>>, length)
	)
    }

    
    pub fn upgrade_slice<'alloc>(
	&self, length: usize, _alloc_scope: &'alloc AllocScope,
    ) -> Option<EFMutSlice<'alloc, T>> {
        // TODO: check that the entire slice will be in mutably accessible
        // memory, and well-aligned.
	Some(unsafe { self.upgrade_slice_unchecked(length) })
    }
	

    // pub fn as_slice_ref<'a, 's, C: Chip>(
    //     &'s self,
    //     _alloc_scope: &'a AllocScope,
    //     length: usize,
    // ) -> Option<&'a [EFMutTy<T>]> {
    //     Some(unsafe { core::slice::from_raw_parts(self.inner, length) })
    // }

    // pub fn as_t_ptr(&self) -> *const T {
    //     self.inner as *const T
    // }

    // pub fn as_t_ptr_mut(&self) -> *mut T {
    //     self.inner as *mut T
    // }
}

// -----------------------------------------------------------------------------

// A reference which is validated to be well-aligned and contained in
// mutably-accessible memory.
#[repr(transparent)]
pub struct EFMutRef<'alloc, T>(pub &'alloc UnsafeCell<MaybeUninit<T>>);

impl<'alloc, T> Clone for EFMutRef<'alloc, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'alloc, T> Copy for EFMutRef<'alloc, T> {}

impl<'alloc, T: EFType> EFMutRef<'alloc, T> {
    // pub fn validate<'access>(
    //     &self,
    //     access_scope: &'access AccessScope,
    // ) -> Result<EFMutVal<'alloc, 'access, T>, Self> {
    //     if let Some(val) = self.validate_ref(access_scope) {
    //         Ok(*val)
    //     } else {
    //         Err(self)
    //     }
    // }


    pub fn validate<'access>(
        &self,
        access_scope: &'access AccessScope,
    ) -> Option<EFMutVal<'alloc, 'access, T>> {
        if <T as EFType>::validate(self.0 as *const _ as *mut UnsafeCell<MaybeUninit<T>> as *mut T) {
            Some(unsafe { self.assume_valid(access_scope) })
        } else {
            None
        }
    }

    // pub unsafe fn assume_valid_ref<'access>(
    //     &self,
    //     _access_scope: &'access AccessScope,
    // ) -> &EFMutVal<'alloc, 'access, T> {
    //     // We can transmute here, as &'a UnsafeCell<T> and *const
    //     // UnsafeCell<T> are type-layout compatible, and both
    //     // `EFMutRef` and `EFMutVal` are `repr(transparent)`
    //     // wrappers around these respective types.
    //     unsafe {
    //         core::mem::transmute::<&EFMutRef<'alloc, T>, &EFMutVal<'alloc, 'access, T>>(&self)
    //     }
    // }

    // Doesn't work due to lifetime constraints. Could probably use a closure
    // with a more restricted scope?
    //
    // pub fn write_access<'access>(&self, val: T, access_scope: &'access mut AccessScope)
    //   -> &EFMutVal<'alloc, 'access, T> {
    //  self.write(val, access_scope);
    //  unsafe { self.assume_valid_ref(access_scope) }
    // }
}

impl<'alloc, T> EFMutRef<'alloc, T> {
    pub unsafe fn assume_valid<'access>(
	&self,
	_access_scope: &'access AccessScope,
    ) -> EFMutVal<'alloc, 'access, T> {
	EFMutVal(&*(self.0 as *const _ as *const MaybeUninit<T>), PhantomData)
    }

    pub fn as_ptr(&self) -> EFMutPtr<T> {
        EFMutPtr(self.0 as *const _ as *mut UnsafeCell<MaybeUninit<T>> as *mut T)
    }

    pub fn write(&self, val: T, _access_scope: &mut AccessScope) {
	(unsafe { &mut *self.0.get() }).write(val);
    }
}

#[repr(transparent)]
pub struct EFMutSlice<'alloc, T>(pub &'alloc [UnsafeCell<MaybeUninit<T>>]);

impl<'alloc, T: EFType> EFMutSlice<'alloc, T> {
    pub unsafe fn assume_valid<'access>(
	&self,
    ) -> EFMutSliceVal<'alloc, 'access, T> {
	EFMutSliceVal(core::mem::transmute::<&[UnsafeCell<MaybeUninit<T>>], &[MaybeUninit<T>]>(&self.0), PhantomData)
    }
    
    pub fn validate<'access>(
        &self,
        access_scope: &'access AccessScope,
    ) -> Option<EFMutSliceVal<'alloc, 'access, T>> {
        if self.0.iter().all(|elem: &UnsafeCell<MaybeUninit<T>>| <T as EFType>::validate(elem as *const _ as *mut UnsafeCell<MaybeUninit<T>> as *mut T)) {
            Some(unsafe { self.assume_valid() })
        } else {
            None
        }
    }
}


// -----------------------------------------------------------------------------

#[repr(transparent)]
pub struct EFMutVal<'alloc, 'access, T>(&'access MaybeUninit<T>, PhantomData<&'alloc T>);

impl<'alloc, 'access, T> EFMutVal<'alloc, 'access, T> {
    pub fn as_ref(&self) -> EFMutRef<'alloc, T> {
	EFMutRef(unsafe { &*(self.0 as *const _ as *const UnsafeCell<MaybeUninit<T>>) })
    }
}


impl<'alloc, 'access, T: EFValDeref> Deref for EFMutVal<'alloc, 'access, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.assume_init_ref() }
    }
}

impl<'alloc, 'access, T> Clone for EFMutVal<'alloc, 'access, T> {
    fn clone(&self) -> Self {
	*self
    }
}

impl<'alloc, 'access, T> Copy for EFMutVal<'alloc, 'access, T> {}

impl<'alloc, 'access, const N: usize, T> EFMutVal<'alloc, 'access, [T; N]> {
    pub fn as_array(&self) -> &[EFMutVal<'alloc, 'access, T>; N] {
	unsafe { core::mem::transmute::<&EFMutVal<'alloc, 'access, [T; N]>, &[EFMutVal<'alloc, 'access, T>; N]>(&self) }
    }
}

#[repr(transparent)]
pub struct EFMutSliceVal<'alloc, 'access, T>(&'access [MaybeUninit<T>], PhantomData<&'alloc [T]>);

impl<'alloc, 'access, T: EFValDeref> Deref for EFMutSliceVal<'alloc, 'access, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute::<&[MaybeUninit<T>], &[T]>(&self.0) }
    }
}


// impl<'alloc, 'access, T> From<EFMutVal<'alloc, 'access, T>> for EFMutRef<'alloc, T> {
//     fn from(val_ref: EFMutVal<'alloc, 'access, T>) -> Self {
//         EFMutRef {
//             ptr: EFMutPtr {
//                 inner: val_ref.inner as *const EFMutTyVal<T> as *const EFMutTy<T>,
//             },
//             _lt: PhantomData,
//         }
//     }
// }

// -----------------------------------------------------------------------------

// A top-level memory allocation. This type is always owned, and is
// returned by the EncapfnRt allocator. Its only purpose it to
// represent uninitialized memory which can be initialized (and thus
// turned into an `EFMutVal`) or used as an uninitialized, and thus
// non-validated `EFMutRef`.
//
// Because this uses a mutable reference, it must not be aliased (yet)!
#[repr(transparent)]
pub struct EFAllocation<'alloc, T: Sized>(&'alloc UnsafeCell<MaybeUninit<T>>);

impl<'alloc, T> EFAllocation<'alloc, T> {
    pub(crate) unsafe fn from_allocated_ptr(ptr: *mut u8) -> Self {
        EFAllocation(
            unsafe { &mut *(ptr as *mut UnsafeCell<MaybeUninit<T>>) },
        )
    }

    // TODO: check lifetime bounds here
    pub fn initialize<'access>(
        self,
        val: T,
        alloc_scope: &'alloc AllocScope,
        access_scope: &'access mut AccessScope,
    ) -> EFMutVal<'alloc, 'access, T> {
	let r = self.into_ref(alloc_scope);
	r.write(val, access_scope);
	unsafe { r.assume_valid(access_scope) }
    }

    pub fn into_ref(self, _alloc_scope: &'alloc AllocScope) -> EFMutRef<'alloc, T> {
        EFMutRef(self.0)
    }
}

// pub struct EFMutGuardedRef<'alloc, 'guard, T>(EFMutRef<'alloc, T>, PhantomData<&'guard ()>);

// impl<'alloc, 'guard, T> EFMutGuardedRef<'alloc, 'guard, T> {
//     fn write(self, 
// }


// #[repr(transparent)]
// pub struct EFMutTy<T> {
//     inner: UnsafeCell<T>,
// }

// impl<T> From<T> for EFMutTy<T> {
//     fn from(val: T) -> Self {
//         EFMutTy {
//             inner: UnsafeCell::new(val),
//         }
//     }
// }

// // impl<T: EFType> EFMutTy<T> {
// //     pub fn validate_ref<'access>(&'s self, _access_scope: &'access AccessScope) -> Option<&'s EFMutTyVal<T>> {
// //         if <T as EFType>::validate(UnsafeCell::get(&self.inner)) {
// //             Some(unsafe { core::mem::transmute::<&EFMutTy<T>, &EFMutTyVal<T>>(&self) })
// //         } else {
// //             None
// //         }
// //     }
// // }

// #[repr(transparent)]
// pub struct EFMutTyVal<T> {
//     inner: UnsafeCell<T>,
// }

// impl<T> From<T> for EFMutTyVal<T> {
//     fn from(val: T) -> Self {
//         EFMutTyVal {
//             inner: UnsafeCell::new(val),
//         }
//     }
// }

// impl<T: Copy> EFMutTyVal<T> {
//     pub fn get(&self) -> T {
//         unsafe { *self.inner.get() }
//     }
// }

// impl<const N: usize, T: Copy> EFMutTyVal<[T; N]> {
//     pub fn as_array_ref(&self) -> &[EFMutTyVal<T>; N] {
//         // TODO: const-assert that align and size match!
//         unsafe { core::mem::transmute::<&EFMutTyVal<[T; N]>, &[EFMutTyVal<T>; N]>(self) }
//     }
// }

// This is illegal, as the resulting Cell would not be bound to an access-scope,
// and its contents can be changed to an invalid value by some aliased reference.
//
// impl<T> Deref for EFMutTyVal<T> {
//     type Target = Cell<T>;

//     fn deref(&self) -> &Self::Target {
//         unsafe { core::mem::transmute::<&UnsafeCell<T>, &Cell<T>>(&self.inner) }
//     }
// }



pub mod primitives {
    use super::{EFType, EFValDeref, EFMutPtr};

    unsafe impl<T> EFType for EFMutPtr<T> {
	fn validate(_t: *mut Self) -> bool {
	    true
	}
    }

    unsafe impl<T> EFValDeref for EFMutPtr<T> {}

    unsafe impl<const N: usize, T> EFType for [T; N] {
	fn validate(_t: *mut Self) -> bool {
	    true
	}   
    }

    unsafe impl<const N: usize, T: EFValDeref> EFValDeref for [T; N] {}

    unsafe impl EFType for u32 {
	fn validate(_t: *mut Self) -> bool {
	    true
	}
    }

    unsafe impl EFValDeref for u32 {}

    unsafe impl EFType for i8 {
        fn validate(_t: *mut Self) -> bool {
            true
        }
    }

    unsafe impl EFValDeref for i8 {}
}
