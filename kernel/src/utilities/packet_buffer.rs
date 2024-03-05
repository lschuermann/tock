use core::any::Any;

/// Internal `PacketBufferDyn` trait, shared across various packet buffer
/// backends (such as [`PacketSlice`]).
///
/// This is a safe interface, but should not be used directly. Instead,
/// manipulate `PacketBufferDyn`s using the [`PacketBufferMut`] container.
pub trait PacketBufferDyn: Any {
    /// Length of the allocated data in this buffer (excluding head- and
    /// tailroom).
    fn len(&self) -> usize;

    /// Available headroom in the underlying buffer.
    fn headroom(&self) -> usize;

    /// Available tailroom in the underlying buffer.
    fn tailroom(&self) -> usize;

    /// Force-reclaim a given amount of headroom in this buffer. This will
    /// ignore any current data stored in the buffer (but not immediately
    /// overwrite it). It will not move past the tailroom marker.
    ///
    /// This method returns a boolean indicating success. A `false` return value
    /// indicates that the `PacketBufferDyn` was not modified.
    fn restore_headroom(&mut self, new_headroom: usize) -> bool;

    /// Force-reset the buffer to length `0`, and set a new headroom
    /// pointer. This will ensure that a subsequent prepend operation starts at
    /// this new headroom pointer.
    ///
    /// This method returns a boolean indicating success. It may fail if
    /// `new_headroom > self.headroom() + self.len() + self.tailroom()`. A
    /// `false` return value indicates that the `PacketBufferDyn` was not
    /// modified.
    fn reset(&mut self, new_headroom: usize) -> bool;
}

// TODO: do we need this?
// impl<T: PacketBufferDyn + Any + ?Sized> PacketBufferDyn for &'static T {
//     fn len(&self) -> usize {
//         (**self).len()
//     }
// }

impl<T: PacketBufferDyn + Any + ?Sized> PacketBufferDyn for &'static mut T {
    fn len(&self) -> usize {
        (**self).len()
    }

    fn headroom(&self) -> usize {
	(**self).headroom()
    }

    fn tailroom(&self) -> usize {
	(**self).tailroom()
    }
}

/// Mutable reference to a packet buffer, with explicit headroom (`HEAD`) and
/// tailroom (`TAIL`) annotations.
///
/// This wraper type guarantees that the underlying buffer has
/// - a headroom of at least `HEAD` bytes, and
/// - a tailroom of at least `TAIL` bytes.
///
/// Methods on this struct generally consume the original packet buffer
/// reference, and return a new one with different const generic
/// annotations. These methods ensure that the const generic parameters are
/// consistent with the inner reference's advertised head- and tailroom.
///
/// This wrapper can be constructed from an arbitrary mutable [`PacketBufferDyn`]
/// reference using [`PacketBufferMut::new`], which will ensure that the `HEAD`
/// and `TAIL` constraints hold initially.
///
/// The original type can be restored through the [`PacketBufferDyn::downcast`]
/// method. It can also be destructed into its inner reference type using
/// [`PacketBufferDyn::into_inner`].
#[repr(transparent)]
pub struct PacketBufferMut<const HEAD: usize, const TAIL: usize> {
    inner: &'static mut dyn PacketBufferDyn,
}

impl<const HEAD: usize, const TAIL: usize> PacketBufferMut<HEAD, TAIL> {
    #[inline(always)]
    pub fn new(inner: &'static mut dyn PacketBufferDyn) -> Option<Self> {
	if inner.headroom() >= HEAD && inner.tailroom >= TAIL {
	    Some(PacketBufferMut { inner })
	} else {
	    None
	}
    }

    /// Length of the allocated data in this buffer (excluding head- and
    /// tailroom).
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Actual available headroom in the underlying buffer. Must be greater or
    /// equal to the `HEAD` parameter.
    #[inline(always)]
    fn headroom(&self) -> usize {
	self.inner.headroom()
    }

    /// Actual available tailroom in the underlying buffer. Must be greater or
    /// equal to the `TAIL` parameter.
    #[inline(always)]
    fn tailroom(&self) -> usize {
	self.inner.headroom()
    }

    /// Reduce the advertised headroom of this buffer, without modifying the
    /// underlying reference.
    ///
    /// This uses an assertion to ensure that `NEW_HEAD <= HEAD`. Because this
    /// assertion exclusively uses compile-time accessible constants, a
    /// violation of this constraint is going to result in a compile time
    /// error. However, this error will only be raised when generating the final
    /// monomorphized types, and as such will not occur on builds using `cargo
    /// check`, etc. See [1].
    ///
    /// [1]: https://github.com/rust-lang/rust/issues/99682
    #[inline(always)]
    pub fn reduce_headroom<const NEW_HEAD: usize>(self) -> PacketBufferMut<NEW_HEAD, TAIL> {
	let _: () = assert!(NEW_HEAD <= HEAD);
	PacketBufferMut { inner: self.inner }
    }

    /// Attempt to restore the headroom of this buffer in a non-destructive way
    /// (not discarding any data in the underlying buffer).
    ///
    /// For this method to return `Ok(_)`, the underlying buffer's
    /// [`PacketBufferDyn::headroom`] must be larger or equal to
    /// `NEW_HEAD`. Otherwise, the old `self` is returned in the `Err(_)`
    /// variant.
    #[inline(always)]
    pub fn restore_headroom<const NEW_HEAD: usize>(self) -> Result<PacketBufferMut<NEW_HEAD, TAIL>, Self> {
	if self.inner.headroom() >= NEW_HEAD {
	    Ok(PacketBufferMut { inner: self.inner })
	} else {
	    Err(self)
	}
    }

    /// Force-reclaim a given amount of headroom in this buffer.
    ///
    /// This will ignore any current data stored in the buffer (but not
    /// immediately overwrite it). It will not move past the tailroom marker.
    ///
    // TODO: document return value, and that in the `Err(_)` case the buffer has
    // not been modified.
    #[inline(always)]
    pub fn reclaim_headroom<const NEW_HEAD: usize>(self) -> Result<PacketBufferDyn<NEW_HEAD, TAIL>, Self> {
	if self.inner.reclaim_headroom(NEW_HEAD) {
	    Ok(PacketBufferMut { inner: self.inner })
	} else {
	    Err(self)
	}
    }

    /// Force-reclaim a given amount of headroom in this buffer.
    ///
    /// This will ignore any current data stored in the buffer (but not
    /// immediately overwrite it). It will not move past the tailroom marker.
    ///
    // TODO: document return value, and that in the `Err(_)` case the buffer has
    // not been modified.
    #[inline(always)]
    pub fn reclaim_headroom<const NEW_HEAD: usize>(self) -> Result<PacketBufferDyn<NEW_HEAD, TAIL>, Self> {
	if self.inner.reclaim_headroom(NEW_HEAD) {
	    Ok(PacketBufferMut { inner: self.inner })
	} else {
	    Err(self)
	}
    }
