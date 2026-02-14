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
    T: Debug + IntoIterator + FromIterator<T::Item>,
    <T as IntoIterator>::IntoIter: ExactSizeIterator,
{
    type RefineError = NonEmptyError<T>;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        let xs = rep.into_iter();
        if xs.len() > 0 {
            Ok(xs.collect::<T>())
        } else {
            NonEmptyError(xs.collect::<T>()).pipe(Err)
        }
    }
}
