use core::cell::{Cell, UnsafeCell};
use core::marker::PhantomData;
use core::mem::MaybeUninit;
use core::ops::Deref;

use kernel::platform::chip::Chip;

pub unsafe trait EFType {
    // For primitives, verify that type layout is correct. For
    // references, make sure that the entire reference is in
    // accessible memory.
    fn validate(t: *mut Self) -> bool;
}

// A top-level memory allocation. This type is always owned, and is
// returned by the EncapfnRt allocator. Its only purpose it to
// represent uninitialized memory which can be initialized (and thus
// turned into an `EFMutRefVal`) or used as an uninitialized, and thus
// non-validated `EFMutRef`.
//
// Because this type is still an exclusive reference to the given
// allocation, it can contain an &mut reference to the the
// MaybeUninit.
//
// Because this uses a mutable reference, it must not be aliased (yet)!
#[repr(transparent)]
pub struct EFAllocation<'alloc, T: Sized> {
    inner: &'alloc mut MaybeUninit<EFMutTyVal<T>>,
}

impl<'alloc, T> EFAllocation<'alloc, T> {
    pub(crate) unsafe fn from_allocated_ptr(ptr: *mut u8) -> Self {
        EFAllocation {
            inner: unsafe { &mut *(ptr as *mut MaybeUninit<EFMutTyVal<T>>) },
        }
    }

    // TODO: check lifetime bounds here
    pub fn initialize<'access>(
        self,
        val: T,
        _alloc_scope: &'alloc AllocScope,
        _access_scope: &'access AccessScope,
    ) -> EFMutRefVal<'alloc, 'access, T> {
        // Move T into the allocation, and return it as an EFMutRefVal
        let initialized = self.inner.write(EFMutTyVal {
            inner: UnsafeCell::new(val),
        });

        EFMutRefVal {
            inner: unsafe {
                core::mem::transmute::<&'alloc EFMutTyVal<T>, &'access EFMutTyVal<T>>(initialized)
            },
            _alloc_lt: PhantomData,
        }
    }

    pub fn into_ref(self, _alloc_scope: &'alloc AllocScope) -> EFMutRef<'alloc, T> {
        EFMutRef {
            ptr: EFMutPtr {
                inner: self.inner.as_ptr() as *const EFMutTy<T>,
            },
            _lt: PhantomData,
        }
    }
}

#[repr(transparent)]
pub struct EFMutRefVal<'alloc, 'access, T> {
    // TODO: remove pub
    pub inner: &'access EFMutTyVal<T>,
    _alloc_lt: PhantomData<&'alloc T>,
}

impl<'alloc, 'access, T> Deref for EFMutRefVal<'alloc, 'access, T> {
    type Target = EFMutTyVal<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'alloc, 'access, T> Clone for EFMutRefVal<'alloc, 'access, T> {
    fn clone(&self) -> Self {
        EFMutRefVal {
            inner: self.inner,
            _alloc_lt: PhantomData,
        }
    }
}

impl<'alloc, 'access, T> Copy for EFMutRefVal<'alloc, 'access, T> {}

// A reference which is validated to be well-aligned and contained in
// mutably-accessible memory
#[repr(transparent)]
pub struct EFMutRef<'alloc, T> {
    pub ptr: EFMutPtr<T>,
    _lt: PhantomData<&'alloc mut T>,
}

impl<'alloc, T> Clone for EFMutRef<'alloc, T> {
    fn clone(&self) -> Self {
        EFMutRef {
            ptr: self.ptr,
            _lt: PhantomData,
        }
    }
}

impl<'alloc, T> Copy for EFMutRef<'alloc, T> {}

impl<'alloc, T: EFType> EFMutRef<'alloc, T> {
    pub fn validate<'access>(
        self,
        access_scope: &'access AccessScope,
    ) -> Result<EFMutRefVal<'alloc, 'access, T>, Self> {
        if let Some(val) = self.validate_ref(access_scope) {
            Ok(*val)
        } else {
            Err(self)
        }
    }

    pub fn validate_ref<'access>(
        &self,
        access_scope: &'access AccessScope,
    ) -> Option<&EFMutRefVal<'alloc, 'access, T>> {
        if <T as EFType>::validate(self.ptr.inner as *mut EFMutTy<T> as *mut T) {
	    Some(unsafe { self.assume_valid_ref(access_scope) })
        } else {
            None
        }
    }

    pub unsafe fn assume_valid_ref<'access>(
        &self,
        _access_scope: &'access AccessScope,
    ) -> &EFMutRefVal<'alloc, 'access, T> {
        // We can transmute here, as &'a UnsafeCell<T> and *const
        // UnsafeCell<T> are type-layout compatible, and both
        // `EFMutRef` and `EFMutRefVal` are `repr(transparent)`
        // wrappers around these respective types.
        unsafe {
            core::mem::transmute::<&EFMutRef<'alloc, T>, &EFMutRefVal<'alloc, 'access, T>>(
                &self,
            )
        }
    }

    // Doesn't work due to lifetime constraints. Could probably use a closure
    // with a more restricted scope?
    //
    // pub fn write_access<'access>(&self, val: T, access_scope: &'access mut AccessScope)
    //   -> &EFMutRefVal<'alloc, 'access, T> {
    // 	self.write(val, access_scope);
    // 	unsafe { self.assume_valid_ref(access_scope) }
    // }
}

impl<'alloc, T> EFMutRef<'alloc, T> {
    pub fn into_ptr(self) -> EFMutPtr<T> {
        self.ptr
    }

    pub fn write(&self, val: T, _access_scope: &mut AccessScope) {
	unsafe { core::ptr::write((*self.ptr.inner).inner.get(), val) };
    }
}

impl<'alloc, 'access, T> From<EFMutRefVal<'alloc, 'access, T>> for EFMutRef<'alloc, T> {
    fn from(val_ref: EFMutRefVal<'alloc, 'access, T>) -> Self {
        EFMutRef {
            ptr: EFMutPtr {
                inner: val_ref.inner as *const EFMutTyVal<T> as *const EFMutTy<T>,
            },
            _lt: PhantomData,
        }
    }
}

#[repr(transparent)]
pub struct EFMutTy<T> {
    inner: UnsafeCell<T>,
}

impl<T> From<T> for EFMutTy<T> {
    fn from(val: T) -> Self {
        EFMutTy {
            inner: UnsafeCell::new(val),
        }
    }
}

// impl<T: EFType> EFMutTy<T> {
//     pub fn validate_ref<'access>(&'s self, _access_scope: &'access AccessScope) -> Option<&'s EFMutTyVal<T>> {
//         if <T as EFType>::validate(UnsafeCell::get(&self.inner)) {
//             Some(unsafe { core::mem::transmute::<&EFMutTy<T>, &EFMutTyVal<T>>(&self) })
//         } else {
//             None
//         }
//     }
// }

#[repr(transparent)]
pub struct EFMutTyVal<T> {
    inner: UnsafeCell<T>,
}

impl<T> From<T> for EFMutTyVal<T> {
    fn from(val: T) -> Self {
        EFMutTyVal {
            inner: UnsafeCell::new(val),
        }
    }
}

impl<T: Copy> EFMutTyVal<T> {
    pub fn get(&self) -> T {
	unsafe { *self.inner.get() }
    }
}

impl<const N: usize, T: Copy> EFMutTyVal<[T; N]> {
    pub fn as_array_ref(&self) -> &[EFMutTyVal<T>; N] {
	// TODO: const-assert that align and size match!
	unsafe { core::mem::transmute::<&EFMutTyVal<[T; N]>, &[EFMutTyVal<T>; N]>(self) }
    }
}

// This is illegal, as the resulting Cell would not be bound to an access-scope,
// and its contents can be changed to an invalid value by some aliased reference.
//
// impl<T> Deref for EFMutTyVal<T> {
//     type Target = Cell<T>;

//     fn deref(&self) -> &Self::Target {
//         unsafe { core::mem::transmute::<&UnsafeCell<T>, &Cell<T>>(&self.inner) }
//     }
// }

pub struct AllocScope;
impl AllocScope {
    // TODO: remove!
    pub fn new() -> Self {
        AllocScope
    }
}

pub struct AccessScope;
impl AccessScope {
    // TODO: remove!
    pub fn new() -> Self {
        AccessScope
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct EFMutPtr<T> {
    // TODO: make non-pub
    pub inner: *const EFMutTy<T>,
}

impl<T> Clone for EFMutPtr<T> {
    fn clone(&self) -> Self {
        EFMutPtr { inner: self.inner }
    }
}

impl<T> Copy for EFMutPtr<T> {}

impl<T: Sized> EFMutPtr<T> {
    pub fn null() -> Self {
        EFMutPtr {
            inner: core::ptr::null(),
        }
    }

    // Safe, as this pointer is cannot be safely dereferenced. When 
    pub fn from_t_ptr(ptr: *mut T) -> Self {
        EFMutPtr {
            inner: ptr as *const _ as *const EFMutTy<T>,
        }
    }

    pub unsafe fn into_ref_unchecked<'a>(self, _alloc_scope: &'a AllocScope) -> EFMutRef<'a, T> {
        EFMutRef {
            ptr: self,
            _lt: PhantomData,
        }
    }

    pub fn into_ref<'a>(self, _alloc_scope: &'a AllocScope) -> Option<EFMutRef<'a, T>> {
        // TODO: check that object pointed to is in mutably accessible
        // memory. We can probably put some associated data into the AllocScope
        // for this check, or have that reference the EncapfnRt.
        Some(EFMutRef {
            ptr: self,
            _lt: PhantomData,
        })
    }

    pub fn as_ref<'a, 's, C: Chip>(
        &'s self,
        _alloc_scope: &'a AllocScope,
    ) -> Option<&'s EFMutRef<'a, T>> {
        // TODO: check that object pointed to is in mutably accessible
        // memory, and well-aligned.

        // Transmute safe here, types have identical layout:
        Some(unsafe { core::mem::transmute::<&EFMutPtr<T>, &EFMutRef<'a, T>>(&self) })
    }

    pub fn as_slice_ref<'a, 's, C: Chip>(
        &'s self,
        _alloc_scope: &'a AllocScope,
        length: usize,
    ) -> Option<&'a [EFMutTy<T>]> {
        Some(unsafe { core::slice::from_raw_parts(self.inner, length) })
    }

    pub fn as_t_ptr(&self) -> *const T {
        self.inner as *const T
    }

    pub fn as_t_ptr_mut(&self) -> *mut T {
        self.inner as *mut T
    }
}

pub mod primitives {
    use super::EFType;

    unsafe impl EFType for ::core::ffi::c_uint {
        fn validate(_t: *mut Self) -> bool {
            true
        }
    }

    unsafe impl EFType for ::core::ffi::c_char {
        fn validate(_t: *mut Self) -> bool {
            true
        }
    }
}
