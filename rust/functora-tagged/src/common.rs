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
pub struct FCrude;

impl<T> Refine<T> for FCrude {
    type RefineError = Infallible;
}

//
// Positive
//

#[derive(Debug)]
pub struct FPositive;

#[derive(Debug, Display)]
#[display("{:?}", self)]
pub struct PositiveError<T>(T)
where
    T: Debug;
impl<T> Error for PositiveError<T> where T: Debug {}

impl<T> Refine<T> for FPositive
where
    T: Ord + Zero + Debug,
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
pub struct FNonNeg;

#[derive(Debug, Display)]
#[display("{:?}", self)]
pub struct NonNegError<T>(T)
where
    T: Debug;
impl<T> Error for NonNegError<T> where T: Debug {}

impl<T> Refine<T> for FNonNeg
where
    T: Ord + Zero + Debug,
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
pub struct FNonEmpty;

#[derive(Debug, Display)]
#[display("{:?}", self)]
pub struct NonEmptyError<T>(T)
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
