// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2022.

use core::cell::UnsafeCell;

#[repr(transparent)]
pub struct ThreadLocal<T>(UnsafeCell<[T; 1]>);

// T: Copy to ensure compatibility with a future multi-threaded version.
impl<T: Copy> ThreadLocal<T> {
    pub const fn init(init: T) -> Self {
        ThreadLocal(UnsafeCell::new([init; 1]))
    }
}

impl<T> ThreadLocal<T> {
    // For T: !Copy, we can also construct a ThreadLocal by passing in the full
    // array of `NUM_THREADS` instances. For now, this is effectively identical
    // to `init`, except for the relaxed Copy requirement.
    pub const fn new(val: [T; 1]) -> Self {
        ThreadLocal(UnsafeCell::new(val))
    }

    #[inline(always)]
    fn get_cell_slice<'a>(&'a self) -> &'a [UnsafeCell<T>; 1] {
        unsafe { core::mem::transmute::<&UnsafeCell<[T; 1]>, &[UnsafeCell<T>; 1]>(&self.0) }
    }

    #[inline(always)]
    pub fn get_mut<'a>(&'a self) -> NonReentrant<'a, T> {
        NonReentrant(&self.get_cell_slice()[0])
    }
}

// TODO: document safety
unsafe impl<T> Sync for ThreadLocal<T> {}

// -----------------------------------------------------------------------------

pub struct NonReentrant<'a, T>(&'a UnsafeCell<T>);

impl<'a, T> NonReentrant<'a, T> {
    pub unsafe fn enter_nonreentrant<R, F: FnOnce(&mut T) -> R>(&self, f: F) -> R {
        f(&mut *self.0.get())
    }
}
