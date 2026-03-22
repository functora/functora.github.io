use crate::common::*;
use crate::{InfallibleInto, Tagged};
use num_traits::*;
use std::cmp::Ordering;
use std::fmt::Debug;
use tap::prelude::*;

//
// Crude
//

impl<T, D> Tagged<T, D, FCrude> {
    #[must_use]
    pub fn zero() -> Self
    where
        T: Zero,
    {
        T::zero().pipe(Tagged::new).infallible()
    }

    #[must_use]
    pub fn one() -> Self
    where
        T: One,
    {
        T::one().pipe(Tagged::new).infallible()
    }
}

//
// Positive
//

impl<T, D> Tagged<T, D, FPositive>
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

    #[must_use]
    pub fn one() -> Self
    where
        T: One,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        T::one().pipe(Tagged::new).unwrap()
    }
}

//
// Zero (inclusive) to One (exclusive)
//

impl<T, D> Tagged<T, D, FZeroInclToOneExcl>
where
    T: PartialOrd + Zero + One + Debug,
{
    #[must_use]
    pub fn zero() -> Self {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        T::zero().pipe(Tagged::new).unwrap()
    }
}

//
// Zero (exclusive) to One (inclusive)
//

impl<T, D> Tagged<T, D, FZeroExclToOneIncl>
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
// Zero (inclusive) to One (inclusive)
//

impl<T, D> Tagged<T, D, FZeroInclToOneIncl>
where
    T: PartialOrd + Zero + One + Debug,
{
    #[must_use]
    pub fn zero() -> Self {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        T::zero().pipe(Tagged::new).unwrap()
    }

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
    pub fn iter<'a, I>(&'a self) -> impl Iterator<Item = I>
    where
        &'a T: IntoIterator<Item = I>,
    {
        (*self).into_iter()
    }

    #[must_use]
    pub fn first<'a, I>(&'a self) -> I
    where
        &'a T: IntoIterator<Item = I>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        (*self).into_iter().next().unwrap()
    }

    #[must_use]
    pub fn last<'a, I>(&'a self) -> I
    where
        &'a T: IntoIterator<Item = I>,
        <&'a T as IntoIterator>::IntoIter:
            DoubleEndedIterator,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        (*self).into_iter().next_back().unwrap()
    }

    pub fn minimum<'a, I>(&'a self) -> I
    where
        &'a T: IntoIterator<Item = I>,
        I: Ord,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().min().unwrap()
    }

    pub fn maximum<'a, I>(&'a self) -> I
    where
        &'a T: IntoIterator<Item = I>,
        I: Ord,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().max().unwrap()
    }

    pub fn min_by<'a, I>(
        &'a self,
        f: impl FnMut(&I, &I) -> Ordering,
    ) -> I
    where
        &'a T: IntoIterator<Item = I>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().min_by(f).unwrap()
    }

    pub fn max_by<'a, I>(
        &'a self,
        f: impl FnMut(&I, &I) -> Ordering,
    ) -> I
    where
        &'a T: IntoIterator<Item = I>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().max_by(f).unwrap()
    }

    pub fn min_by_key<'a, I, V>(
        &'a self,
        f: impl FnMut(&I) -> V,
    ) -> I
    where
        &'a T: IntoIterator<Item = I>,
        V: Ord,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().min_by_key(f).unwrap()
    }

    pub fn max_by_key<'a, I, V>(
        &'a self,
        f: impl FnMut(&I) -> V,
    ) -> I
    where
        &'a T: IntoIterator<Item = I>,
        V: Ord,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().max_by_key(f).unwrap()
    }

    #[allow(clippy::should_implement_trait)]
    pub fn into_iter<U>(self) -> T::IntoIter
    where
        T: IntoIterator<Item = U>,
    {
        self.untag().into_iter()
    }

    pub fn map<U, V, W>(
        self,
        f: impl FnMut(U) -> V,
    ) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator<Item = U>,
        W: Debug + HasLength + FromIterator<V>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.into_iter()
            .map(f)
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }

    pub fn reduce<U>(self, f: impl FnMut(U, U) -> U) -> U
    where
        T: IntoIterator<Item = U>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.into_iter().reduce(f).unwrap()
    }

    pub fn rev<U, W>(self) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator<Item = U>,
        T::IntoIter: DoubleEndedIterator,
        W: Debug + HasLength + FromIterator<U>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.into_iter()
            .rev()
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }

    pub fn sort<U, W>(self) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator<Item = U>,
        U: Ord,
        W: Debug + HasLength + FromIterator<U>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.into_iter()
            .collect::<Vec<_>>()
            .tap_mut(|v| v.sort())
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }

    pub fn sort_by_key<U, V, W>(
        self,
        f: impl FnMut(&U) -> V,
    ) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator<Item = U>,
        V: Ord,
        W: Debug + HasLength + FromIterator<U>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.into_iter()
            .collect::<Vec<_>>()
            .tap_mut(|v| v.sort_by_key(f))
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }

    pub fn dedup<U, W>(self) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator<Item = U>,
        U: PartialEq,
        W: Debug + HasLength + FromIterator<U>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.into_iter()
            .collect::<Vec<_>>()
            .tap_mut(Vec::dedup)
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }
}
