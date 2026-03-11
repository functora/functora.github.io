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

//
// Non-Empty
//

impl<T, D> Tagged<T, D, FNonEmpty> {
    #[must_use]
    pub fn first<U>(&self) -> &U
    where
        T: AsRef<[U]>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.as_ref().first().unwrap()
    }

    #[must_use]
    pub fn last<U>(&self) -> &U
    where
        T: AsRef<[U]>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.as_ref().last().unwrap()
    }

    pub fn min_by_key<U, V>(
        &self,
        f: impl FnMut(&&U) -> V,
    ) -> &U
    where
        T: AsRef<[U]>,
        V: Ord,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.as_ref().iter().min_by_key(f).unwrap()
    }

    pub fn max_by_key<U, V>(
        &self,
        f: impl FnMut(&&U) -> V,
    ) -> &U
    where
        T: AsRef<[U]>,
        V: Ord,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.as_ref().iter().max_by_key(f).unwrap()
    }

    pub fn map<U, V, W>(
        &self,
        f: impl FnMut(&U) -> V,
    ) -> Tagged<W, D, FNonEmpty>
    where
        T: AsRef<[U]>,
        W: Debug + HasLength + FromIterator<V>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.as_ref()
            .iter()
            .map(f)
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }
}
