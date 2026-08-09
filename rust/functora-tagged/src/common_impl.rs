use crate::common::*;
use crate::{InfallibleInto, Tagged};
use num_traits::*;
use std::cmp::Ordering;
use std::fmt::{Debug, Display};
use std::ops::Deref;
use tap::prelude::*;

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

impl<T, D> Tagged<T, D, FPositive>
where
    T: PartialOrd + Zero + One + Debug,
{
    #[must_use]
    pub fn one() -> Self {
        T::one().pipe(Tagged::new).pipe(proven)
    }
}

impl<T, D> Tagged<T, D, FNonNeg>
where
    T: PartialOrd + Zero + Debug,
{
    #[must_use]
    pub fn zero() -> Self {
        T::zero().pipe(Tagged::new).pipe(proven)
    }

    #[must_use]
    pub fn one() -> Self
    where
        T: One,
    {
        T::one().pipe(Tagged::new).pipe(proven)
    }
}

impl<T, D> Tagged<T, D, FZeroInclToOneExcl>
where
    T: PartialOrd + Zero + One + Debug,
{
    #[must_use]
    pub fn zero() -> Self {
        T::zero().pipe(Tagged::new).pipe(proven)
    }

    #[must_use]
    pub fn one() -> Self {
        T::one().pipe(Tagged::new).pipe(proven)
    }
}

impl<T, D> Tagged<T, D, FZeroExclToOneIncl>
where
    T: PartialOrd + Zero + One + Debug,
{
    #[must_use]
    pub fn one() -> Self {
        T::one().pipe(Tagged::new).pipe(proven)
    }
}

impl<T, D> Tagged<T, D, FZeroInclToOneIncl>
where
    T: PartialOrd + Zero + One + Debug,
{
    #[must_use]
    pub fn zero() -> Self {
        T::zero().pipe(Tagged::new).pipe(proven)
    }

    #[must_use]
    pub fn one() -> Self {
        T::one().pipe(Tagged::new).pipe(proven)
    }
}

impl<T, D> Tagged<T, D, FNonEmpty> {
    #[must_use]
    pub fn singleton<U>(item: U) -> Self
    where
        T: Debug + HasLength + FromIterator<U>,
    {
        std::iter::once(item)
            .collect::<T>()
            .pipe(Tagged::new)
            .pipe(proven)
    }

    #[must_use]
    pub fn extend<U>(
        self,
        iter: impl IntoIterator<Item = U>,
    ) -> Self
    where
        T: Debug + HasLength + Extend<U>,
    {
        self.untag()
            .tap_mut(|rep| rep.extend(iter))
            .pipe(Tagged::new)
            .pipe(proven)
    }

    pub fn iter<'a, U>(&'a self) -> impl Iterator<Item = U>
    where
        &'a T: IntoIterator<Item = U>,
    {
        (*self).into_iter()
    }

    #[must_use]
    pub fn first<'a, U>(&'a self) -> U
    where
        &'a T: IntoIterator<Item = U>,
    {
        (*self).into_iter().next().pipe(total)
    }

    #[must_use]
    pub fn last<'a, U>(&'a self) -> U
    where
        &'a T: IntoIterator<Item = U>,
        <&'a T as IntoIterator>::IntoIter:
            DoubleEndedIterator,
    {
        (*self).into_iter().next_back().pipe(total)
    }

    pub fn minimum<'a, U>(&'a self) -> U
    where
        &'a T: IntoIterator<Item = U>,
        U: Ord,
    {
        self.iter().min().pipe(total)
    }

    pub fn maximum<'a, U>(&'a self) -> U
    where
        &'a T: IntoIterator<Item = U>,
        U: Ord,
    {
        self.iter().max().pipe(total)
    }

    pub fn min_by<'a, U>(
        &'a self,
        f: impl FnMut(&U, &U) -> Ordering,
    ) -> U
    where
        &'a T: IntoIterator<Item = U>,
    {
        self.iter().min_by(f).pipe(total)
    }

    pub fn max_by<'a, U>(
        &'a self,
        f: impl FnMut(&U, &U) -> Ordering,
    ) -> U
    where
        &'a T: IntoIterator<Item = U>,
    {
        self.iter().max_by(f).pipe(total)
    }

    pub fn min_by_key<'a, U, V>(
        &'a self,
        f: impl FnMut(&U) -> V,
    ) -> U
    where
        &'a T: IntoIterator<Item = U>,
        V: Ord,
    {
        self.iter().min_by_key(f).pipe(total)
    }

    pub fn max_by_key<'a, U, V>(
        &'a self,
        f: impl FnMut(&U) -> V,
    ) -> U
    where
        &'a T: IntoIterator<Item = U>,
        V: Ord,
    {
        self.iter().max_by_key(f).pipe(total)
    }

    pub fn map<U, V, W>(
        self,
        f: impl FnMut(U) -> V,
    ) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator<Item = U>,
        W: Debug + HasLength + FromIterator<V>,
    {
        self.into_iter()
            .map(f)
            .collect::<W>()
            .pipe(Tagged::new)
            .pipe(proven)
    }

    pub fn reduce<U>(self, f: impl FnMut(U, U) -> U) -> U
    where
        T: IntoIterator<Item = U>,
    {
        self.into_iter().reduce(f).pipe(total)
    }

    pub fn rev<U, W>(self) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator<Item = U>,
        T::IntoIter: DoubleEndedIterator,
        W: Debug + HasLength + FromIterator<U>,
    {
        self.into_iter()
            .rev()
            .collect::<W>()
            .pipe(Tagged::new)
            .pipe(proven)
    }

    pub fn sort<U, W>(self) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator<Item = U>,
        U: Ord,
        W: Debug + HasLength + FromIterator<U>,
    {
        self.into_iter()
            .collect::<Vec<_>>()
            .tap_mut(|v| v.sort())
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .pipe(proven)
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
        self.into_iter()
            .collect::<Vec<_>>()
            .tap_mut(|v| v.sort_by_key(f))
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .pipe(proven)
    }

    pub fn dedup<U, W>(self) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator<Item = U>,
        U: PartialEq,
        W: Debug + HasLength + FromIterator<U>,
    {
        self.into_iter()
            .collect::<Vec<_>>()
            .tap_mut(Vec::dedup)
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .pipe(proven)
    }

    pub fn via_into<W>(self) -> Tagged<W, D, FNonEmpty>
    where
        W: From<T>,
        W: Debug + HasLength,
    {
        self.untag()
            .pipe(W::from)
            .pipe(Tagged::new)
            .pipe(proven)
    }

    pub fn via_iter<W>(self) -> Tagged<W, D, FNonEmpty>
    where
        T: IntoIterator,
        W: Debug + HasLength + FromIterator<T::Item>,
    {
        self.untag()
            .into_iter()
            .collect::<W>()
            .pipe(Tagged::new)
            .pipe(proven)
    }
}

impl<T, D> Tagged<T, D, FNonEmpty>
where
    T: Deref,
{
    pub fn ref_iter<'a, U>(
        &'a self,
    ) -> impl Iterator<Item = U> + 'a
    where
        &'a <T as Deref>::Target: IntoIterator<Item = U>,
    {
        (**self).into_iter()
    }

    #[must_use]
    pub fn ref_first<'a, U>(&'a self) -> U
    where
        &'a <T as Deref>::Target: IntoIterator<Item = U>,
    {
        (**self).into_iter().next().pipe(total)
    }

    #[must_use]
    pub fn ref_last<'a, U>(&'a self) -> U
    where
        &'a <T as Deref>::Target: IntoIterator<Item = U>,
        <&'a <T as Deref>::Target as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        (**self).into_iter().next_back().pipe(total)
    }

    pub fn ref_minimum<'a, U>(&'a self) -> U
    where
        &'a <T as Deref>::Target: IntoIterator<Item = U>,
        U: Ord,
    {
        self.ref_iter().min().pipe(total)
    }

    pub fn ref_maximum<'a, U>(&'a self) -> U
    where
        &'a <T as Deref>::Target: IntoIterator<Item = U>,
        U: Ord,
    {
        self.ref_iter().max().pipe(total)
    }

    pub fn ref_min_by<'a, U>(
        &'a self,
        f: impl FnMut(&U, &U) -> Ordering,
    ) -> U
    where
        &'a <T as Deref>::Target: IntoIterator<Item = U>,
    {
        self.ref_iter().min_by(f).pipe(total)
    }

    pub fn ref_max_by<'a, U>(
        &'a self,
        f: impl FnMut(&U, &U) -> Ordering,
    ) -> U
    where
        &'a <T as Deref>::Target: IntoIterator<Item = U>,
    {
        self.ref_iter().max_by(f).pipe(total)
    }

    pub fn ref_min_by_key<'a, U, V>(
        &'a self,
        f: impl FnMut(&U) -> V,
    ) -> U
    where
        &'a <T as Deref>::Target: IntoIterator<Item = U>,
        V: Ord,
    {
        self.ref_iter().min_by_key(f).pipe(total)
    }

    pub fn ref_max_by_key<'a, U, V>(
        &'a self,
        f: impl FnMut(&U) -> V,
    ) -> U
    where
        &'a <T as Deref>::Target: IntoIterator<Item = U>,
        V: Ord,
    {
        self.ref_iter().max_by_key(f).pipe(total)
    }
}

fn proven<T, D, F>(
    result: Result<Tagged<T, D, F>, F::RefineError>,
) -> Tagged<T, D, F>
where
    F: crate::refine::Refine<T>,
    F::RefineError: Display,
{
    match result {
        Ok(tagged) => tagged,
        Err(err) => panic!(
            "value is guaranteed to satisfy the refinery invariant: {err}"
        ),
    }
}

fn total<U>(option: Option<U>) -> U {
    match option {
        Some(item) => item,
        None => panic!(
            "collection is guaranteed to be non-empty"
        ),
    }
}
