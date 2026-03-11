use crate::common::*;
use crate::{InfallibleInto, Tagged};
use num_traits::*;
use std::fmt::Debug;
use tap::prelude::*;

//
// Crude
//

impl<T, D> Tagged<T, D, FCrude>
where
    T: Zero,
{
    #[must_use]
    pub fn zero() -> Self {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        T::zero().pipe(Tagged::new).infallible()
    }
}

impl<T, D> Tagged<T, D, FCrude>
where
    T: One,
{
    #[must_use]
    pub fn one() -> Self {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        T::one().pipe(Tagged::new).infallible()
    }
}

//
// Non-Negative
//

impl<T, D> Tagged<T, D, FNonNeg>
where
    T: PartialOrd + Zero + Debug,
{
    #[must_use]
    pub fn zero() -> Self {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        T::zero().pipe(Tagged::new).unwrap()
    }
}

impl<T, D> Tagged<T, D, FNonNeg>
where
    T: PartialOrd + Zero + One + Debug,
{
    #[must_use]
    pub fn one() -> Self {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        T::one().pipe(Tagged::new).unwrap()
    }
}
