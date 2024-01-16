use core::cell::UnsafeCell;
use core::marker::PhantomData;
use core::mem::MaybeUninit;
use core::ops::Deref;

use crate::branding::EFID;

pub trait AllocTracker {}

pub struct AllocScope<R: AllocTracker, ID: EFID>(R, PhantomData<ID>);
impl<R: AllocTracker, ID: EFID> AllocScope<R, ID> {
    pub unsafe fn new(tracker: R) -> Self {
        AllocScope(tracker, PhantomData)
    }

    pub fn tracker(&self) -> &R {
        &self.0
    }
}

pub struct AccessScope<ID: EFID>(PhantomData<ID>);
impl<ID: EFID> AccessScope<ID> {
    pub unsafe fn new() -> Self {
        AccessScope(PhantomData)
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
pub struct EFPtr<T>(pub *mut T);

impl<T> Clone for EFPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for EFPtr<T> {}

impl<T> From<*mut T> for EFPtr<T> {
    fn from(ptr: *mut T) -> Self {
        EFPtr(ptr)
    }
}

impl<T> From<*const T> for EFPtr<T> {
    fn from(ptr: *const T) -> Self {
        EFPtr(ptr as *mut T)
    }
}

impl<T> From<usize> for EFPtr<T> {
    fn from(ptr: usize) -> Self {
        EFPtr(ptr as *mut T)
    }
}

impl<T> From<EFPtr<T>> for *mut T {
    fn from(efmutptr: EFPtr<T>) -> Self {
        efmutptr.0
    }
}

impl<T> From<EFPtr<T>> for *const T {
    fn from(efmutptr: EFPtr<T>) -> Self {
        efmutptr.0 as *const T
    }
}

impl<T> From<EFPtr<T>> for usize {
    fn from(efmutptr: EFPtr<T>) -> Self {
        efmutptr.0 as usize
    }
}

impl<T> EFPtr<T> {
    pub fn null() -> Self {
        EFPtr(core::ptr::null_mut())
    }

    pub unsafe fn upgrade_unchecked<'alloc, ID: EFID>(&self) -> EFMutRef<'alloc, ID, T> {
        EFMutRef(
            &*(self.0 as *mut UnsafeCell<MaybeUninit<T>> as *const _),
            PhantomData,
        )
    }

    pub fn upgrade_mut<'alloc, R: AllocTracker, ID: EFID>(
        &self,
        _alloc_scope: &'alloc AllocScope<R, ID>,
    ) -> Option<EFMutRef<'alloc, ID, T>> {
        // TODO: check that object pointed to is in mutably accessible
        // memory, and well-aligned.

        Some(unsafe { self.upgrade_unchecked() })
    }

    pub unsafe fn upgrade_slice_unchecked_mut<'alloc, ID: EFID>(
        &self,
        length: usize,
    ) -> EFMutSlice<'alloc, ID, T> {
        // TODO: this is unsound. It forms an intermediate slice reference,
        // which is not yet contained in the `UnsafeCell<MaybeUninit<T>>`
        // container.
        EFMutSlice(
            core::slice::from_raw_parts_mut(
                self.0 as *mut _ as *mut UnsafeCell<MaybeUninit<T>>,
                length,
            ),
            PhantomData,
        )
    }

    pub fn upgrade_slice_mut<'alloc, R: AllocTracker, ID: EFID>(
        &self,
        length: usize,
        _alloc_scope: &'alloc AllocScope<R, ID>,
    ) -> Option<EFMutSlice<'alloc, ID, T>> {
        // TODO: check that the entire slice will be in mutably accessible
        // memory, and well-aligned.
        Some(unsafe { self.upgrade_slice_unchecked_mut(length) })
    }
}

// -----------------------------------------------------------------------------

// A reference which is validated to be well-aligned and contained in
// mutably-accessible memory.
#[repr(transparent)]
pub struct EFMutRef<'alloc, ID: EFID, T>(&'alloc UnsafeCell<MaybeUninit<T>>, PhantomData<ID>);

impl<'alloc, ID: EFID, T> Clone for EFMutRef<'alloc, ID, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'alloc, ID: EFID, T> Copy for EFMutRef<'alloc, ID, T> {}

impl<'alloc, ID: EFID, T: EFType> EFMutRef<'alloc, ID, T> {
    pub fn validate<'access>(
        &self,
        access_scope: &'access AccessScope<ID>,
    ) -> Option<EFMutVal<'alloc, 'access, ID, T>> {
        if <T as EFType>::validate(self.0 as *const _ as *mut UnsafeCell<MaybeUninit<T>> as *mut T)
        {
            Some(unsafe { self.assume_valid(access_scope) })
        } else {
            None
        }
    }
}

impl<'alloc, ID: EFID, T> EFMutRef<'alloc, ID, T> {
    pub unsafe fn assume_valid<'access>(
        &self,
        _access_scope: &'access AccessScope<ID>,
    ) -> EFMutVal<'alloc, 'access, ID, T> {
        EFMutVal(
            &*(self.0 as *const _ as *const MaybeUninit<T>),
            PhantomData,
            PhantomData,
        )
    }

    pub fn as_ptr(&self) -> EFPtr<T> {
        EFPtr(self.0 as *const _ as *mut UnsafeCell<MaybeUninit<T>> as *mut T)
    }

    pub fn write<'access>(
        &self,
        val: T,
        _access_scope: &'access mut AccessScope<ID>,
    ) -> EFMutVal<'alloc, 'access, ID, T> {
        (unsafe { &mut *self.0.get() }).write(val);
        unsafe {
            EFMutVal(
                &*(self.0 as *const _ as *const MaybeUninit<T>),
                PhantomData,
                PhantomData,
            )
        }
    }
}

#[repr(transparent)]
pub struct EFMutSlice<'alloc, ID: EFID, T>(
    pub &'alloc [UnsafeCell<MaybeUninit<T>>],
    PhantomData<ID>,
);

impl<'alloc, ID: EFID, T: EFType> EFMutSlice<'alloc, ID, T> {
    pub unsafe fn assume_valid<'access>(&self) -> EFMutSliceVal<'alloc, 'access, ID, T> {
        EFMutSliceVal(
            core::mem::transmute::<&[UnsafeCell<MaybeUninit<T>>], &[MaybeUninit<T>]>(&self.0),
            PhantomData,
            PhantomData,
        )
    }

    pub fn validate<'access>(
        &self,
        access_scope: &'access AccessScope<ID>,
    ) -> Option<EFMutSliceVal<'alloc, 'access, ID, T>> {
        if self.0.iter().all(|elem: &UnsafeCell<MaybeUninit<T>>| {
            <T as EFType>::validate(elem as *const _ as *mut UnsafeCell<MaybeUninit<T>> as *mut T)
        }) {
            Some(unsafe { self.assume_valid() })
        } else {
            None
        }
    }
}

// -----------------------------------------------------------------------------

#[repr(transparent)]
pub struct EFMutVal<'alloc, 'access, ID: EFID, T>(
    &'access MaybeUninit<T>,
    PhantomData<&'alloc T>,
    PhantomData<ID>,
);

impl<'alloc, 'access, ID: EFID, T> EFMutVal<'alloc, 'access, ID, T> {
    pub fn as_ref(&self) -> EFMutRef<'alloc, ID, T> {
        EFMutRef(
            unsafe { &*(self.0 as *const _ as *const UnsafeCell<MaybeUninit<T>>) },
            PhantomData,
        )
    }
}

impl<'alloc, 'access, ID: EFID, T: EFValDeref> Deref for EFMutVal<'alloc, 'access, ID, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.assume_init_ref() }
    }
}

impl<'alloc, 'access, ID: EFID, T> Clone for EFMutVal<'alloc, 'access, ID, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'alloc, 'access, ID: EFID, T> Copy for EFMutVal<'alloc, 'access, ID, T> {}

impl<'alloc, 'access, const N: usize, ID: EFID, T> EFMutVal<'alloc, 'access, ID, [T; N]> {
    pub fn as_array(&self) -> &[EFMutVal<'alloc, 'access, ID, T>; N] {
        unsafe {
            core::mem::transmute::<
                &EFMutVal<'alloc, 'access, ID, [T; N]>,
                &[EFMutVal<'alloc, 'access, ID, T>; N],
            >(&self)
        }
    }
}

#[repr(transparent)]
pub struct EFMutSliceVal<'alloc, 'access, ID: EFID, T>(
    &'access [MaybeUninit<T>],
    PhantomData<&'alloc [T]>,
    PhantomData<ID>,
);

impl<'alloc, 'access, ID: EFID, T: EFValDeref> Deref for EFMutSliceVal<'alloc, 'access, ID, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute::<&[MaybeUninit<T>], &[T]>(&self.0) }
    }
}

// -----------------------------------------------------------------------------

// A top-level memory allocation. This type is always owned, and is
// returned by the EncapfnRt allocator. Its only purpose it to
// represent uninitialized memory which can be initialized (and thus
// turned into an `EFMutVal`) or used as an uninitialized, and thus
// non-validated `EFMutRef`.
//
// Because this uses a mutable reference, it must not be aliased (yet)!
#[repr(transparent)]
pub struct EFAllocation<'alloc, ID: EFID, T: Sized>(
    &'alloc UnsafeCell<MaybeUninit<T>>,
    PhantomData<ID>,
);

impl<'alloc, ID: EFID, T> EFAllocation<'alloc, ID, T> {
    pub(crate) unsafe fn from_allocated_ptr(ptr: *mut u8) -> Self {
        EFAllocation(
            unsafe { &mut *(ptr as *mut UnsafeCell<MaybeUninit<T>>) },
            PhantomData,
        )
    }

    // TODO: check lifetime bounds here
    pub fn initialize<'access, R: AllocTracker>(
        self,
        val: T,
        alloc_scope: &'alloc AllocScope<R, ID>,
        access_scope: &'access mut AccessScope<ID>,
    ) -> EFMutVal<'alloc, 'access, ID, T> {
        let r = self.into_ref(alloc_scope);
        r.write(val, access_scope);
        unsafe { r.assume_valid(access_scope) }
    }

    pub fn into_ref<R: AllocTracker>(
        self,
        _alloc_scope: &'alloc AllocScope<R, ID>,
    ) -> EFMutRef<'alloc, ID, T> {
        EFMutRef(self.0, PhantomData)
    }
}

pub mod primitives {
    use super::{EFPtr, EFType, EFValDeref};

    unsafe impl<T> EFType for EFPtr<T> {
        fn validate(_t: *mut Self) -> bool {
            true
        }
    }

    unsafe impl<T> EFValDeref for EFPtr<T> {}

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
