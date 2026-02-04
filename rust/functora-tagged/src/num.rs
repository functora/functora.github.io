use crate::refine::*;
use crate::tagged::*;
use derive_more::Display;
use num_traits::*;
use std::error::Error;
use std::fmt::Debug;
use std::marker::PhantomData;
use tap::prelude::*;

/////////////
// Algebra //
/////////////

#[derive(Debug)]
pub struct Identity<I>(PhantomData<I>);
#[derive(Debug)]
pub struct Prim<P>(PhantomData<P>);
#[derive(Debug)]
pub struct Times<L, R, A>(PhantomData<(L, R, A)>);
#[derive(Debug)]
pub struct Per<L, R, A>(PhantomData<(L, R, A)>);

impl<T, I> Refine<T> for Identity<I>
where
    I: Refine<T>,
{
    type RefineError = I::RefineError;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        I::refine(rep)
    }
}

impl<T, P> Refine<T> for Prim<P>
where
    P: Refine<T>,
{
    type RefineError = P::RefineError;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        P::refine(rep)
    }
}

impl<T, L, R, A> Refine<T> for Times<L, R, A>
where
    A: Refine<T>,
{
    type RefineError = A::RefineError;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        A::refine(rep)
    }
}

impl<T, L, R, A> Refine<T> for Per<L, R, A>
where
    A: Refine<T>,
{
    type RefineError = A::RefineError;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        A::refine(rep)
    }
}

pub trait DMul<R, O> {}

pub trait DDiv<R, O> {}

// 1. L * R = L ✖ R
impl<L, R, A> DMul<Prim<R>, Times<Prim<L>, Prim<R>, A>>
    for Prim<L>
{
}

// 2. (L ÷ R) * R = L
impl<L, R, A> DMul<R, L> for Per<L, R, A> {}

// 3. R * (L ÷ R) = L
impl<L, R, A> DMul<Per<L, R, A>, L> for R {}

// 4. L / R = L ÷ R
impl<L, R, A> DDiv<Prim<R>, Per<Prim<L>, Prim<R>, A>>
    for Prim<L>
{
}

// 5. L / (L ÷ R) = R
impl<L, R, A> DDiv<Per<L, R, A>, R> for L {}

// 6. (L ✖ R) / R = L
impl<L, R, A> DDiv<R, L> for Times<L, R, A> {}

// 7. (L ✖ R) / L = R
// Since this overlap with (L ✖ R) / R = L when L = R, we pick the only one.
// Rule 6 already handles the R cancellation. Rule 7 will conflict.

// 8. (L ÷ R) / L = 1 ÷ R
impl<L, R, A, B, I> DDiv<Prim<L>, Per<Identity<I>, R, B>>
    for Per<Prim<L>, R, A>
{
}

// 9. 1 * T = T
impl<T, I> DMul<T, T> for Identity<I> {}

// 10. T * 1 = T
impl<P, I> DMul<Identity<I>, Prim<P>> for Prim<P> {}

// 11. (L ÷ R) * 1 = L ÷ R
impl<L, R, A, I> DMul<Identity<I>, Per<L, R, A>>
    for Per<L, R, A>
{
}

// 12. (L ÷ R) * 1 = L ÷ R
impl<L, R, A, I> DMul<Identity<I>, Times<L, R, A>>
    for Times<L, R, A>
{
}

// 13. T / 1 = T
impl<T, I> DDiv<Identity<I>, T> for T {}

// 14. 1 / T = 1 ÷ T
impl<P, A, I> DDiv<Prim<P>, Per<Identity<I>, Prim<P>, A>>
    for Identity<I>
{
}

// 15. 1 / (L ÷ R) = R ÷ L
impl<L, R, A, B, I>
    DDiv<Per<Prim<L>, Prim<R>, A>, Per<Prim<R>, Prim<L>, B>>
    for Identity<I>
{
}

//////////////
//  Errors  //
//////////////

#[derive(Eq, PartialEq, Debug, Display)]
#[display("{:?}", self)]
pub enum FNumError<L: Debug, R: Debug> {
    Add(L, R),
    Sub(L, R),
    Mul(L, R),
    Div(L, R),
}

impl<L: Debug, R: Debug> Error for FNumError<L, R> {}

///////////
//  Add  //
///////////

pub trait TAdd: Sized + Debug {
    fn tadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>>;
}

impl<T> TAdd for T
where
    T: Copy + Debug + CheckedAdd,
{
    fn tadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<T, T>> {
        self.checked_add(rhs)
            .ok_or(FNumError::Add(*self, *rhs))
    }
}

impl<T, D> TAdd for Tagged<T, D>
where
    T: Copy + TAdd,
    D: Debug + Refine<T>,
{
    fn tadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>> {
        self.rep()
            .tadd(rhs.rep())
            .map_err(|_| FNumError::Add(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Add(*self, *rhs))
    }
}

///////////
//  Sub  //
///////////

pub trait TSub: Sized + Debug {
    fn tsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>>;
}

impl<T> TSub for T
where
    T: Copy + Debug + CheckedSub,
{
    fn tsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<T, T>> {
        self.checked_sub(rhs)
            .ok_or(FNumError::Sub(*self, *rhs))
    }
}

impl<T, D> TSub for Tagged<T, D>
where
    T: Copy + TSub,
    D: Debug + Refine<T>,
{
    fn tsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>> {
        self.rep()
            .tsub(rhs.rep())
            .map_err(|_| FNumError::Sub(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Sub(*self, *rhs))
    }
}

///////////
//  Mul  //
///////////

pub trait TMul<R, O>: Sized + Debug
where
    R: Debug,
{
    fn tmul(
        &self,
        rhs: &R,
    ) -> Result<O, FNumError<Self, R>>;
}

impl<T> TMul<T, T> for T
where
    T: Copy + Debug + CheckedMul,
{
    fn tmul(&self, rhs: &T) -> Result<T, FNumError<T, T>> {
        self.checked_mul(rhs)
            .ok_or(FNumError::Mul(*self, *rhs))
    }
}

impl<T, L, R, O> TMul<Tagged<T, R>, Tagged<T, O>>
    for Tagged<T, L>
where
    T: Copy + TMul<T, T>,
    L: Debug + DMul<R, O>,
    R: Debug,
    O: Refine<T>,
{
    fn tmul(
        &self,
        rhs: &Tagged<T, R>,
    ) -> Result<Tagged<T, O>, FNumError<Self, Tagged<T, R>>>
    {
        self.rep()
            .tmul(rhs.rep())
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

///////////
//  Div  //
///////////

pub trait TDiv<R, O>: Sized + Debug
where
    R: Debug,
{
    fn tdiv(
        &self,
        rhs: &R,
    ) -> Result<O, FNumError<Self, R>>;
}

impl<T> TDiv<T, T> for T
where
    T: Copy + Debug + CheckedDiv,
{
    fn tdiv(&self, rhs: &T) -> Result<T, FNumError<T, T>> {
        self.checked_div(rhs)
            .ok_or(FNumError::Div(*self, *rhs))
    }
}

impl<T, L, R, O> TDiv<Tagged<T, R>, Tagged<T, O>>
    for Tagged<T, L>
where
    T: Copy + TDiv<T, T>,
    L: Debug + DDiv<R, O>,
    R: Debug,
    O: Refine<T>,
{
    fn tdiv(
        &self,
        rhs: &Tagged<T, R>,
    ) -> Result<Tagged<T, O>, FNumError<Self, Tagged<T, R>>>
    {
        self.rep()
            .tdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}
