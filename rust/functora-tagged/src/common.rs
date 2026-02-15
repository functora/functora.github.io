use crate::refine::*;
use derive_more::Display;
use num_traits::*;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::Debug;
use std::ops::Deref;
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
// IsEmpty
//

pub trait IsEmpty {
    fn is_empty(&self) -> bool;
}

impl<T> IsEmpty for Vec<T> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<T> IsEmpty for Box<[T]> {
    fn is_empty(&self) -> bool {
        self.deref().is_empty()
    }
}

impl<T: Clone> IsEmpty for std::borrow::Cow<'_, [T]> {
    fn is_empty(&self) -> bool {
        self.deref().is_empty()
    }
}

impl IsEmpty for String {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl IsEmpty for &str {
    fn is_empty(&self) -> bool {
        (*self).is_empty()
    }
}

impl<T, S> IsEmpty for std::collections::HashSet<T, S> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<K, V, S> IsEmpty
    for std::collections::HashMap<K, V, S>
{
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<T> IsEmpty for std::collections::BTreeSet<T> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<K, V> IsEmpty for std::collections::BTreeMap<K, V> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<T> IsEmpty for std::collections::VecDeque<T> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<T> IsEmpty for std::collections::LinkedList<T> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<T> Refine<T> for FNonEmpty
where
    T: Debug + IsEmpty,
{
    type RefineError = NonEmptyError<T>;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        if rep.is_empty() {
            NonEmptyError(rep).pipe(Err)
        } else {
            Ok(rep)
        }
    }
}
