use crate::refine::*;
use derive_more::Display;
use num_traits::*;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::Debug;
use tap::prelude::*;

//
// Crude
//

#[derive(Debug)]
pub enum FCrude {}

impl<T> Refine<T> for FCrude {
    type RefineError = Infallible;
}

//
// Positive
//

#[derive(Debug)]
pub enum FPositive {}

#[derive(
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Copy,
    Clone,
    Debug,
    Display,
)]
#[display("{:?}", self)]
pub struct PositiveError<T>(pub T)
where
    T: Debug;
impl<T> Error for PositiveError<T> where T: Debug {}

impl<T> Refine<T> for FPositive
where
    T: PartialOrd + Zero + Debug,
{
    type RefineError = PositiveError<T>;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        if rep > Zero::zero() {
            Ok(rep)
        } else {
            PositiveError(rep).pipe(Err)
        }
    }
}

//
// Non-Negative
//

#[derive(Debug)]
pub enum FNonNeg {}

#[derive(
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Copy,
    Clone,
    Debug,
    Display,
)]
#[display("{:?}", self)]
pub struct NonNegError<T>(pub T)
where
    T: Debug;
impl<T> Error for NonNegError<T> where T: Debug {}

impl<T> Refine<T> for FNonNeg
where
    T: PartialOrd + Zero + Debug,
{
    type RefineError = NonNegError<T>;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        if rep >= Zero::zero() {
            Ok(rep)
        } else {
            NonNegError(rep).pipe(Err)
        }
    }
}

//
// Non-Empty
//

#[derive(Debug)]
pub enum FNonEmpty {}

#[derive(
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Copy,
    Clone,
    Debug,
    Display,
)]
#[display("{:?}", self)]
pub struct NonEmptyError<T>(pub T)
where
    T: Debug;
impl<T> Error for NonEmptyError<T> where T: Debug {}

impl<T> Refine<T> for FNonEmpty
where
    T: Debug + HasLength,
{
    type RefineError = NonEmptyError<T>;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        if rep.zero_length() {
            NonEmptyError(rep).pipe(Err)
        } else {
            Ok(rep)
        }
    }
}

//
// HasLength
//

pub trait HasLength {
    fn length(&self) -> usize;
    fn zero_length(&self) -> bool {
        self.length() == 0
    }
}

// Blanket

impl<T: HasLength + ?Sized> HasLength for &T {
    fn length(&self) -> usize {
        (**self).length()
    }
}

impl<T: HasLength + ?Sized> HasLength for &mut T {
    fn length(&self) -> usize {
        (**self).length()
    }
}

// Slices

impl<T> HasLength for [T] {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T, const N: usize> HasLength for [T; N] {
    fn length(&self) -> usize {
        N
    }
}

// Strings

impl HasLength for str {
    fn length(&self) -> usize {
        self.len()
    }
}

impl HasLength for String {
    fn length(&self) -> usize {
        self.len()
    }
}

impl HasLength for std::path::Path {
    fn length(&self) -> usize {
        self.as_os_str().len()
    }
}

impl HasLength for std::path::PathBuf {
    fn length(&self) -> usize {
        self.as_os_str().len()
    }
}

impl HasLength for std::ffi::OsStr {
    fn length(&self) -> usize {
        self.len()
    }
}

impl HasLength for std::ffi::OsString {
    fn length(&self) -> usize {
        self.len()
    }
}

impl HasLength for std::ffi::CStr {
    fn length(&self) -> usize {
        self.to_bytes().len()
    }
}

impl HasLength for std::ffi::CString {
    fn length(&self) -> usize {
        self.as_bytes().len()
    }
}

// Smart Pointers

impl<T> HasLength for Box<T>
where
    T: HasLength + ?Sized,
{
    fn length(&self) -> usize {
        (**self).length()
    }
}

impl<T> HasLength for std::borrow::Cow<'_, T>
where
    T: HasLength + ?Sized + ToOwned,
{
    fn length(&self) -> usize {
        (**self).length()
    }
}

impl<T> HasLength for std::rc::Rc<T>
where
    T: HasLength + ?Sized,
{
    fn length(&self) -> usize {
        (**self).length()
    }
}

impl<T> HasLength for std::sync::Arc<T>
where
    T: HasLength + ?Sized,
{
    fn length(&self) -> usize {
        (**self).length()
    }
}

// Collections

impl<T> HasLength for Vec<T> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T, S> HasLength for std::collections::HashSet<T, S> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<K, V, S> HasLength
    for std::collections::HashMap<K, V, S>
{
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> HasLength for std::collections::BTreeSet<T> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<K, V> HasLength for std::collections::BTreeMap<K, V> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> HasLength for std::collections::VecDeque<T> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> HasLength for std::collections::LinkedList<T> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> HasLength for std::collections::BinaryHeap<T> {
    fn length(&self) -> usize {
        self.len()
    }
}
