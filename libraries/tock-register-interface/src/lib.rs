//! Tock Register Interface
//!
//!

#![feature(const_fn_trait_bound)]
#![no_std]

pub mod fields;
pub mod interfaces;
pub mod macros;
pub mod registers;

use core::fmt;
use core::marker::PhantomData;
use core::ops::{BitAnd, BitOr, BitOrAssign, Not, Shl, Shr};

use fields::{Field, FieldValue, TryFromValue};

/// Trait representing the base type of registers.
///
/// IntLike defines basic properties of types required to
/// read/write/modify a register through its methods and supertrait
/// requirements.
///
/// It features a range of default implementations for common integer
/// types, such as [`u8`], [`u16`], [`u32`], [`u64`], [`u128`] and
/// [`usize`].
pub trait IntLike:
    BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitOrAssign
    + Not<Output = Self>
    + Eq
    + Shr<usize, Output = Self>
    + Shl<usize, Output = Self>
    + Copy
    + Clone
{
    /// Return the representation of the value `0` in the implementing
    /// type.
    ///
    /// This can be used to acquire values of the [`IntLike`] type,
    /// even in generic implementations. For instance, to get the
    /// value `1`, one can use `<T as IntLike>::zero() + 1`. To get
    /// the largest representable value, use a bitwise negation: `~(<T
    /// as IntLike>::zero())`.
    fn zero() -> Self;
}

// Helper macro for implementing the IntLike trait on differrent
// types.
macro_rules! IntLike_impl_for {
    ($type:ty) => {
        impl IntLike for $type {
            fn zero() -> Self {
                0
            }
        }
    };
}

IntLike_impl_for!(u8);
IntLike_impl_for!(u16);
IntLike_impl_for!(u32);
IntLike_impl_for!(u64);
IntLike_impl_for!(u128);
IntLike_impl_for!(usize);

/// Descriptive name for each register.
pub trait RegisterLongName {}

// Useful implementation for when no RegisterLongName is required
// (e.g. no fields need to be accessed, just the raw register values)
impl RegisterLongName for () {}

/// A read-write copy of register contents.
///
/// This behaves very similarly to a read-write register, but instead of doing a
/// volatile read to MMIO to get the value for each function call, a copy of the
/// register contents are stored locally in memory. This allows a peripheral
/// to do a single read on a register, and then check which bits are set without
/// having to do a full MMIO read each time. It also allows the value of the
/// register to be "cached" in case the peripheral driver needs to clear the
/// register in hardware yet still be able to check the bits.
/// You can write to a local register, which will modify the stored value, but
/// will not modify any hardware because it operates only on local copy.
///
/// This type does not implement the
/// [`Readable`](interfaces::Readable) and
/// [`Writeable`](interfaces::Writeable) traits because it requires a
/// mutable reference to modify the contained value. It still mirrors
/// the interface which would be exposed by a type implementing
/// [`Readable`](interfaces::Readable),
/// [`Writeable`](interfaces::Writeable) and
/// [`ReadWriteable`](interfaces::ReadWriteable).
#[derive(Copy, Clone)]
pub struct LocalRegisterCopy<T: IntLike, R: RegisterLongName = ()> {
    value: T,
    associated_register: PhantomData<R>,
}

impl<T: IntLike, R: RegisterLongName> LocalRegisterCopy<T, R> {
    pub const fn new(value: T) -> Self {
        LocalRegisterCopy {
            value: value,
            associated_register: PhantomData,
        }
    }

    /// Get the raw register value
    #[inline]
    pub fn get(&self) -> T {
        self.value
    }

    /// Set the raw register value
    #[inline]
    pub fn set(&mut self, value: T) {
        self.value = value;
    }

    /// Read the value of the given field
    #[inline]
    pub fn read(&self, field: Field<T, R>) -> T {
        field.read(self.get())
    }

    /// Read value of the given field as an enum member
    #[inline]
    pub fn read_as_enum<E: TryFromValue<T, EnumType = E>>(&self, field: Field<T, R>) -> Option<E> {
        field.read_as_enum(self.get())
    }

    /// Write the value of one or more fields, overwriting the other fields with zero
    #[inline]
    pub fn write(&mut self, field: FieldValue<T, R>) {
        self.set(field.value);
    }

    /// Write the value of one or more fields, leaving the other fields unchanged
    #[inline]
    pub fn modify(&mut self, field: FieldValue<T, R>) {
        self.set(field.modify(self.get()));
    }

    /// Check if one or more bits in a field are set
    #[inline]
    pub fn is_set(&self, field: Field<T, R>) -> bool {
        field.is_set(self.get())
    }

    /// Check if any specified parts of a field match
    #[inline]
    pub fn matches_any(&self, field: FieldValue<T, R>) -> bool {
        field.matches_any(self.get())
    }

    /// Check if all specified parts of a field match
    #[inline]
    pub fn matches_all(&self, field: FieldValue<T, R>) -> bool {
        field.matches_all(self.get())
    }

    /// Do a bitwise AND operation of the stored value and the passed in value
    /// and return a new LocalRegisterCopy.
    #[inline]
    pub fn bitand(&self, rhs: T) -> LocalRegisterCopy<T, R> {
        LocalRegisterCopy::new(self.value & rhs)
    }
}

impl<T: IntLike + fmt::Debug, R: RegisterLongName> fmt::Debug for LocalRegisterCopy<T, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

// Helper macro to implement From<LocalRegisterCopy<T: IntLike>, R>>
// for <T: IntLike>
macro_rules! From_impl_for {
    ($type:ty) => {
        impl<R: RegisterLongName> From<LocalRegisterCopy<$type, R>> for $type {
            fn from(r: LocalRegisterCopy<$type, R>) -> $type {
                r.value
            }
        }
    };
}

From_impl_for!(u8);
From_impl_for!(u16);
From_impl_for!(u32);
From_impl_for!(u64);
From_impl_for!(u128);
From_impl_for!(usize);
