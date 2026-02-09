use crate::refine::*;
pub use derive_more::Display;
pub use std::error::Error;
pub use std::fmt::{Debug, Display};
use std::marker::PhantomData;
pub use std::str::FromStr;

#[derive(Debug, Display)]
#[display("{:?}", self)]
pub enum ParseError<T, D, F>
where
    T: Debug + FromStr,
    D: Debug,
    F: Debug + Refine<T>,
    T::Err: Debug,
    F::RefineError: Debug,
{
    Decode(T::Err, PhantomData<(D, F)>),
    Refine(F::RefineError, PhantomData<(D, F)>),
}

impl<T, D, F> Error for ParseError<T, D, F>
where
    T: Debug + FromStr,
    D: Debug,
    F: Debug + Refine<T>,
    T::Err: Error + Debug + Display + 'static,
    F::RefineError: Error + Debug + Display + 'static,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ParseError::Decode(e, PhantomData) => Some(e),
            ParseError::Refine(e, PhantomData) => Some(e),
        }
    }
}

impl<T, D, F> Eq for ParseError<T, D, F>
where
    T: Debug + FromStr,
    D: Debug,
    F: Debug + Refine<T>,
    T::Err: Debug + Eq,
    F::RefineError: Debug + Eq,
{
}

impl<T, D, F> PartialEq for ParseError<T, D, F>
where
    T: Debug + FromStr,
    D: Debug,
    F: Debug + Refine<T>,
    T::Err: Debug + PartialEq,
    F::RefineError: Debug + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                ParseError::Decode(a, PhantomData),
                ParseError::Decode(b, PhantomData),
            ) => a == b,
            (
                ParseError::Refine(a, PhantomData),
                ParseError::Refine(b, PhantomData),
            ) => a == b,
            _ => false,
        }
    }
}

impl<T, D, F> Clone for ParseError<T, D, F>
where
    T: Debug + FromStr,
    D: Debug,
    F: Debug + Refine<T>,
    T::Err: Debug + Clone,
    F::RefineError: Debug + Clone,
{
    fn clone(&self) -> Self {
        match self {
            ParseError::Decode(err, PhantomData) => {
                ParseError::Decode(err.clone(), PhantomData)
            }
            ParseError::Refine(err, PhantomData) => {
                ParseError::Refine(err.clone(), PhantomData)
            }
        }
    }
}
