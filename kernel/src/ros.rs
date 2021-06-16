//! Read Only Systems calls
//!
//! This modules provides infrastructure required to implement "read
//! only system calls" to userspace applications. This is in concept
//! similar to Linux vDSO system calls.
//!
//! The benefits of using this infrastructure is that applications can
//! avoid potential context switch overheads associated with
//! traditional system calls into the kernel, by reading values which
//! have been proactively placed in memory.
//!
//! This infrastructure provides an [`ROSDriver`] trait which serves
//! as a hook in the kernel scheduler. Prior to switching to a
//! userspace process, this hook is invoked to update any ROS buffers
//! of this process.
//!
//! Because of the nature of this mechanism, read values will only be
//! as accurate as the last time the application was switched to by
//! the kernel.
//!
//! The layouts of the ROS regions are defined as part of their
//! respective specifications and implementations. Because
//! pseudo-simultaneous writes from the kernel and reads from
//! userspace results in potential race conditions, each implementor
//! of an [`ROSDriver`] must have a formal specification in the form
//! of a Tock TRD, outlining how userspace can ensure that read values
//! are in itself consistent.

use crate::process::ProcessId;

/// Read only system call mechanism trait
///
/// This trait can be used to implement custom read-only system calls,
/// as outlined in the module documentation.
pub trait ROSDriver {
    /// Update the values in the ROS regions of a to be scheduled
    /// process
    ///
    /// This method is invoked as part of the Tock kernel loop, prior
    /// to a process being scheduled. It is also invoked the first
    /// time a given process is scheduled. Thus, this invocation can
    /// be used for code to determine whether the ROS mechanism is
    /// enabled.
    fn update_values(&self, process_id: ProcessId);
}

/// Dummy implementation of a ROSDriver hook
///
/// The unit type (`()`) can be used when no proper [`ROSDriver`] is
/// to be hooked into the kernel.
impl ROSDriver for () {
    fn update_values(&self, _process_id: ProcessId) {
        // Default implementation for when no ROSDriver is installed,
        // hence do nothing
    }
}
