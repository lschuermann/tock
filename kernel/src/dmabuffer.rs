use core::cell::Cell;
use core::cmp;
use core::mem;
use core::ops::Range;
use core::ptr::NonNull;
use core::slice;

/// Kernel-global counter for DMABuffer handle identifiers.
///
/// Each `DMABuffer` which can hand out [`ReadableDMAHandle`]s must have a
/// unique handle such that it can distinguish handles issued by it from handles
/// issued by other instances. This global counter serves as a basis to create
/// these IDs from. It imposes an upper limit on the number of `DMABuffer`
/// instances which can be created. Once an ID is assigned, it must not be
/// reused. `DMABuffer`s shall use `new_handle_id` to allocate a new unique
/// handle ID based on this counter.
static mut DMA_BUFFER_COUNT: usize = 0;

fn new_handle_id() -> usize {
    unsafe {
        let handle = DMA_BUFFER_COUNT;
        DMA_BUFFER_COUNT = DMA_BUFFER_COUNT.checked_add(1).unwrap();
        handle
    }
}

/// Handle to a DMA buffer, exposing an immutable interface.
///
/// This handle can be issued by `DMABuffer`s to provide underlying layers
/// access to a buffer. While this handle exist, the `DMABuffer` containers must
/// not provide access to the held buffer. As such it serves as a token for
/// exclusive access to a buffer, between call stack invocations.
///
/// Because the buffer is guaranteed to not be modified as long as this handle
/// is held, it is safe to use it in DMA operations. No DMA operation must be
/// running when this handle is passed back to its originating `DMABuffer`
/// container.
#[derive(Debug)]
pub struct ReadableDMABufferHandle {
    ptr: NonNull<u8>,
    len: usize,
    handle_id: usize,
}

impl ReadableDMABufferHandle {
    pub fn as_ptr(&self) -> *const u8 {
        self.ptr.as_ptr() as *const _
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn get<'a>(&'a self) -> &'a [u8] {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr() as *const _, self.len) }
    }
}

/// Container for a mutable buffer, usable with DMA operations.
///
/// This construct is similar to `TakeCell<'static, [u8]>` in that it can
/// optionally hold a mutable buffer with a `'static` lifetime. Furthermore, it
/// can be used to issue a [`ReadableDMABufferHandle`] via
/// [`borrow_readable`](MutableDMABuffer::borrow_readable) to lower layers, for
/// performing DMA operations on the immutable buffer contents. The buffer is
/// inaccessible as long as the [`ReadableDMABufferHandle`] is in scope and
/// until it is returned to the [`MutableDMABuffer`] with a call to
/// [`return_readable`](MutableDMABuffer::return_readable).
pub struct MutableDMABuffer {
    buffer: Cell<Option<(NonNull<u8>, usize)>>,
    taken: Cell<bool>,
    handle_id: usize,
}

impl MutableDMABuffer {
    pub fn empty() -> Self {
        MutableDMABuffer {
            buffer: Cell::new(None),
            taken: Cell::new(false),
            handle_id: new_handle_id(),
        }
    }

    pub fn new(buf: &'static mut [u8]) -> Self {
        MutableDMABuffer {
            buffer: Cell::new(Some((
                unsafe { NonNull::new_unchecked(buf.as_mut_ptr()) },
                buf.len(),
            ))),
            taken: Cell::new(false),
            handle_id: new_handle_id(),
        }
    }

    pub fn has_buffer(&self) -> bool {
        self.buffer.get().is_some()
    }

    pub fn buffer_accessible(&self) -> bool {
        self.has_buffer() && !self.taken.get()
    }

    pub fn take(&self) -> Option<&'static mut [u8]> {
        if self.taken.get() {
            None
        } else if let Some((ptr, len)) = self.buffer.replace(None) {
            Some(unsafe { slice::from_raw_parts_mut(ptr.as_ptr(), len) })
        } else {
            None
        }
    }

    pub fn replace(&self, new_buf: &'static mut [u8]) -> Option<&'static mut [u8]> {
        if self.taken.get() {
            None
        } else {
            self.buffer
                .replace(Some((
                    unsafe { NonNull::new_unchecked(new_buf.as_mut_ptr()) },
                    new_buf.len(),
                )))
                .map(|(prev_ptr, prev_len)| unsafe {
                    slice::from_raw_parts_mut(prev_ptr.as_ptr(), prev_len)
                })
        }
    }

    pub fn borrow_readable(&self) -> Option<ReadableDMABufferHandle> {
        if self.taken.get() {
            None
        } else if let Some((ptr, len)) = self.buffer.get() {
            self.taken.set(true);
            Some(ReadableDMABufferHandle {
                ptr,
                len,
                handle_id: self.handle_id,
            })
        } else {
            None
        }
    }

    pub fn borrow_readable_window(&self, window: Range<usize>) -> Option<ReadableDMABufferHandle> {
        // Implemented to reuse the `borrow_readable` method and reduce the
        // retrieved pointer range respectively.
        if let Some(full_handle) = self.borrow_readable() {
            if window.start <= window.end
                && window.end < full_handle.len
                && window.end <= (isize::MAX as usize)
            {
                Some(ReadableDMABufferHandle {
                    ptr: unsafe {
                        NonNull::new_unchecked(full_handle.ptr.as_ptr().add(window.start))
                    },
                    len: window.end - window.start,
                    handle_id: full_handle.handle_id,
                })
            } else {
                self.return_readable(full_handle).unwrap();
                None
            }
        } else {
            None
        }
    }

    pub fn return_readable(
        &self,
        handle: ReadableDMABufferHandle,
    ) -> Result<(), ReadableDMABufferHandle> {
        if self.taken.get() && self.handle_id == handle.handle_id {
            // We are in the taken state and the handles match, so this handle
            // must belong to us. Drop it to be sure and set the state to not
            // taken. Also, make sure we still hold the buffer reference.
            mem::drop(handle);
            assert!(self.buffer.get().is_some());
            self.taken.set(false);
            Ok(())
        } else {
            if !self.taken.get() {
                // The buffer must be taken in order to match, so this handle
                // cannot possibly correspond to us. Nonetheless assert that the
                // handle_ids are different, as otherwise this would indicate an
                // internal inconsistency!
                assert!(self.handle_id != handle.handle_id);
            }

            // The passed handle does not correspond to us, return it.
            Err(handle)
        }
    }

    pub fn advance_readable_window(
        &self,
        handle: ReadableDMABufferHandle,
    ) -> Result<Option<ReadableDMABufferHandle>, ReadableDMABufferHandle> {
        let prev_ptr: *const u8 = handle.ptr.as_ptr();
        let prev_len: usize = handle.len;

        self.return_readable(handle).map(|()| {
            // Handle has been returned, we can check whether the next slice
            // would be of zero length in which case we don't try to
            // reborrow.

            // A buffer has to be present, given we've just returned a
            // buffer handle.
            let (buffer_ptr, buffer_len) = self.buffer.get().unwrap();

            // Calculate the next window start address.
            let prev_offset = unsafe { prev_ptr.offset_from(buffer_ptr.as_ptr()) } as usize;
            let next_offset = prev_offset + prev_len;

            if next_offset >= buffer_len {
                // We can't advance the window any further
                None
            } else {
                // There's still a subsequent region left in the buffer,
                // calculate its length and try to reborrow.
                let next_len = cmp::min(buffer_len - next_offset, prev_len);
                // The borrowing has to work, given we've calculated a range
                // which is guaranteed to be in bounds and just successfully
                // returned the previous handle:
                Some(
                    self.borrow_readable_window(next_offset..(next_offset + next_len))
                        .unwrap(),
                )
            }
        })
    }

    pub fn map<R, F: FnOnce(&mut [u8]) -> R>(&self, fun: F) -> Option<R> {
        if let (false, Some((ptr, len))) = (self.taken.get(), self.buffer.get()) {
            self.taken.set(true);
            let res = fun(unsafe { slice::from_raw_parts_mut(ptr.as_ptr(), len) });
            self.taken.set(false);
            Some(res)
        } else {
            None
        }
    }
}

pub struct ImmutableDMABuffer {
    buffer: Cell<Option<&'static [u8]>>,
    taken: Cell<bool>,
    handle_id: usize,
}

impl ImmutableDMABuffer {
    pub fn empty() -> Self {
        ImmutableDMABuffer {
            buffer: Cell::new(None),
            taken: Cell::new(false),
            handle_id: new_handle_id(),
        }
    }

    pub fn new(buf: &'static [u8]) -> Self {
        ImmutableDMABuffer {
            buffer: Cell::new(Some(buf)),
            taken: Cell::new(false),
            handle_id: new_handle_id(),
        }
    }

    pub fn has_buffer(&self) -> bool {
        self.buffer.get().is_some()
    }

    pub fn buffer_accessible(&self) -> bool {
        self.has_buffer() && !self.taken.get()
    }

    pub fn take(&self) -> Option<&'static [u8]> {
        if self.taken.get() {
            None
        } else {
            self.buffer.replace(None)
        }
    }

    pub fn replace(&self, new_buf: &'static [u8]) -> Option<&'static [u8]> {
        if self.taken.get() {
            None
        } else {
            self.buffer.replace(Some(new_buf))
        }
    }

    pub fn borrow_readable(&self) -> Option<ReadableDMABufferHandle> {
        if self.taken.get() {
            None
        } else if let Some(buf) = self.buffer.get() {
            self.taken.set(true);
            Some(ReadableDMABufferHandle {
                ptr: unsafe { NonNull::new_unchecked(buf.as_ptr() as *mut _) },
                len: buf.len(),
                handle_id: self.handle_id,
            })
        } else {
            None
        }
    }

    pub fn borrow_readable_window(&self, window: Range<usize>) -> Option<ReadableDMABufferHandle> {
        // Implemented to reuse the `borrow_readable` method and reduce the
        // retrieved pointer range respectively.
        if let Some(full_handle) = self.borrow_readable() {
            if window.start <= window.end
                && window.end < full_handle.len
                && window.end <= (isize::MAX as usize)
            {
                Some(ReadableDMABufferHandle {
                    ptr: unsafe {
                        NonNull::new_unchecked(full_handle.ptr.as_ptr().add(window.start))
                    },
                    len: window.end - window.start,
                    handle_id: full_handle.handle_id,
                })
            } else {
                self.return_readable(full_handle).unwrap();
                None
            }
        } else {
            None
        }
    }

    pub fn return_readable(
        &self,
        handle: ReadableDMABufferHandle,
    ) -> Result<(), ReadableDMABufferHandle> {
        if self.taken.get() && self.handle_id == handle.handle_id {
            // We are in the taken state and the handles match, so this handle
            // must belong to us. Drop it to be sure and set the state to not
            // taken. Also, make sure we still hold the buffer reference.
            mem::drop(handle);
            assert!(self.buffer.get().is_some());
            self.taken.set(false);
            Ok(())
        } else {
            if !self.taken.get() {
                // The buffer must be taken in order to match, so this handle
                // cannot possibly correspond to us. Nonetheless assert that the
                // handle_ids are different, as otherwise this would indicate an
                // internal inconsistency!
                assert!(self.handle_id != handle.handle_id);
            }

            // The passed handle does not correspond to us, return it.
            Err(handle)
        }
    }

    pub fn advance_readable_window(
        &self,
        handle: ReadableDMABufferHandle,
    ) -> Result<Option<ReadableDMABufferHandle>, ReadableDMABufferHandle> {
        let prev_ptr: *const u8 = handle.ptr.as_ptr();
        let prev_len: usize = handle.len;

        self.return_readable(handle).map(|()| {
            // Handle has been returned, we can check whether the next slice
            // would be of zero length in which case we don't try to
            // reborrow.

            // A buffer has to be present, given we've just returned a
            // buffer handle.
            let buffer = self.buffer.get().unwrap();

            // Calculate the next window start address.
            let prev_offset = unsafe { prev_ptr.offset_from(buffer.as_ptr()) } as usize;
            let next_offset = prev_offset + prev_len;

            if next_offset >= buffer.len() {
                // We can't advance the window any further
                None
            } else {
                // There's still a subsequent region left in the buffer,
                // calculate its length and try to reborrow.
                let next_len = cmp::min(buffer.len() - next_offset, prev_len);
                // The borrowing has to work, given we've calculated a range
                // which is guaranteed to be in bounds and just successfully
                // returned the previous handle:
                Some(
                    self.borrow_readable_window(next_offset..(next_offset + next_len))
                        .unwrap(),
                )
            }
        })
    }
}
