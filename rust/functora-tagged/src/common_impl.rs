use crate::common::*;
use crate::{InfallibleInto, Tagged};
use num_traits::*;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::slice::Iter;
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
    pub fn iter<U>(&self) -> Iter<'_, U>
    where
        T: AsRef<[U]>,
    {
        self.as_ref().iter()
    }

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

    pub fn minimum<U>(&self) -> &U
    where
        T: AsRef<[U]>,
        U: Ord,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().min().unwrap()
    }

    pub fn maximum<U>(&self) -> &U
    where
        T: AsRef<[U]>,
        U: Ord,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().max().unwrap()
    }

    pub fn min_by<U>(
        &self,
        f: impl FnMut(&&U, &&U) -> Ordering,
    ) -> &U
    where
        T: AsRef<[U]>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().min_by(f).unwrap()
    }

    pub fn max_by<U>(
        &self,
        f: impl FnMut(&&U, &&U) -> Ordering,
    ) -> &U
    where
        T: AsRef<[U]>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().max_by(f).unwrap()
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
        self.iter().min_by_key(f).unwrap()
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
        self.iter().max_by_key(f).unwrap()
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
        self.iter()
            .map(f)
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }

    pub fn reduce<U>(&self, f: impl FnMut(U, U) -> U) -> U
    where
        T: AsRef<[U]>,
        U: Clone,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter().cloned().reduce(f).unwrap()
    }

    pub fn rev<U, W>(&self) -> Tagged<W, D, FNonEmpty>
    where
        T: AsRef<[U]>,
        U: Clone,
        W: Debug + HasLength + FromIterator<U>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.iter()
            .rev()
            .cloned()
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }

    pub fn sort<U, W>(&self) -> Tagged<W, D, FNonEmpty>
    where
        T: AsRef<[U]>,
        U: Clone + Ord,
        W: Debug + HasLength + FromIterator<U>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.as_ref()
            .to_vec()
            .tap_mut(|v| v.sort())
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }

    pub fn sort_by_key<U, V, W>(
        &self,
        f: impl FnMut(&U) -> V,
    ) -> Tagged<W, D, FNonEmpty>
    where
        T: AsRef<[U]>,
        U: Clone,
        V: Ord,
        W: Debug + HasLength + FromIterator<U>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.as_ref()
            .to_vec()
            .tap_mut(|v| v.sort_by_key(f))
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }

    pub fn dedup<U, W>(&self) -> Tagged<W, D, FNonEmpty>
    where
        T: AsRef<[U]>,
        U: Clone + PartialEq,
        W: Debug + HasLength + FromIterator<U>,
    {
        #[allow(clippy::unwrap_used)]
        #[allow(clippy::missing_panics_doc)]
        self.as_ref()
            .to_vec()
            .tap_mut(Vec::dedup)
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .unwrap()
    }
}
