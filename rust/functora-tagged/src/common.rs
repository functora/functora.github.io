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

//
// HasLen
//

pub trait HasLen {
    fn length(&self) -> usize;
    fn zero_length(&self) -> bool {
        self.length() == 0
    }
}

impl<T> HasLen for [T] {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T, const N: usize> HasLen for [T; N] {
    fn length(&self) -> usize {
        N
    }
}

impl HasLen for str {
    fn length(&self) -> usize {
        self.len()
    }
}

impl HasLen for String {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> HasLen for Vec<T> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> HasLen for Box<T>
where
    T: HasLen + ?Sized,
{
    fn length(&self) -> usize {
        (**self).length()
    }
}

impl<T> HasLen for std::borrow::Cow<'_, T>
where
    T: HasLen + ?Sized + ToOwned,
{
    fn length(&self) -> usize {
        (**self).length()
    }
}

impl<T, S> HasLen for std::collections::HashSet<T, S> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<K, V, S> HasLen
    for std::collections::HashMap<K, V, S>
{
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> HasLen for std::collections::BTreeSet<T> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<K, V> HasLen for std::collections::BTreeMap<K, V> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> HasLen for std::collections::VecDeque<T> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> HasLen for std::collections::LinkedList<T> {
    fn length(&self) -> usize {
        self.len()
    }
}

impl<T> Refine<T> for FNonEmpty
where
    T: Debug + HasLen,
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
