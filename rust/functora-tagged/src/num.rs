use crate::refine::*;
use crate::tagged::*;
use derive_more::Display;
use num_traits::*;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::Debug;
use std::marker::PhantomData;
use tap::prelude::*;

/////////////
// Algebra //
/////////////

#[derive(Debug)]
pub struct Identity<I, F>(PhantomData<(I, F)>);
#[derive(Debug)]
pub struct Atomic<A, F>(PhantomData<(A, F)>);
#[derive(Debug)]
pub struct Times<L, R, F>(PhantomData<(L, R, F)>);
#[derive(Debug)]
pub struct Per<L, R, F>(PhantomData<(L, R, F)>);

impl<T, I, F> Refine<T> for Identity<I, F>
where
    F: Refine<T>,
{
    type RefineError = F::RefineError;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        F::refine(rep)
    }
}

impl<T, A, F> Refine<T> for Atomic<A, F>
where
    F: Refine<T>,
{
    type RefineError = F::RefineError;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        F::refine(rep)
    }
}

impl<T, L, R, F> Refine<T> for Times<L, R, F>
where
    F: Refine<T>,
{
    type RefineError = F::RefineError;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        F::refine(rep)
    }
}

impl<T, L, R, F> Refine<T> for Per<L, R, F>
where
    F: Refine<T>,
{
    type RefineError = F::RefineError;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        F::refine(rep)
    }
}

pub trait DMul<R, O> {}

pub trait DDiv<R, O> {}

// 1. L * R = L ✖ R
impl<L, R, LF, RF, TF>
    DMul<
        Atomic<R, RF>,
        Times<Atomic<L, LF>, Atomic<R, RF>, TF>,
    > for Atomic<L, LF>
{
}

// 2. (L ÷ R) * R = L
impl<L, R, PF> DMul<R, L> for Per<L, R, PF> {}

// 3. R * (L ÷ R) = L
impl<L, R, PF> DMul<Per<L, R, PF>, L> for R {}

// 4. L / R = L ÷ R
impl<L, R, LF, RF, PF>
    DDiv<
        Atomic<R, RF>,
        Per<Atomic<L, LF>, Atomic<R, RF>, PF>,
    > for Atomic<L, LF>
{
}

// 5. L / (L ÷ R) = R
impl<L, R, PF> DDiv<Per<L, R, PF>, R> for L {}

// 6. (L ✖ R) / R = L
impl<L, R, TF> DDiv<R, L> for Times<L, R, TF> {}

// 7. (L ✖ R) / L = R
// Since this overlap with (L ✖ R) / R = L when L = R, we pick the only one.
// Rule 6 already handles the R cancellation. Rule 7 will conflict.

// 8. (L ÷ R) / L = 1 ÷ R
impl<L, R, I, LF, IF, PF1, PF2>
    DDiv<Atomic<L, LF>, Per<Identity<I, IF>, R, PF2>>
    for Per<Atomic<L, LF>, R, PF1>
{
}

// 9. 1 * T = T
impl<T, I, IF> DMul<T, T> for Identity<I, IF> {}

// 10. T * 1 = T
impl<A, I, AF, IF> DMul<Identity<I, IF>, Atomic<A, AF>>
    for Atomic<A, AF>
{
}

// 11. (L ÷ R) * 1 = L ÷ R
impl<L, R, I, IF, PF> DMul<Identity<I, IF>, Per<L, R, PF>>
    for Per<L, R, PF>
{
}

// 12. (L ÷ R) * 1 = L ÷ R
impl<L, R, I, IF, TF> DMul<Identity<I, IF>, Times<L, R, TF>>
    for Times<L, R, TF>
{
}

// 13. T / 1 = T
impl<T, I, IF> DDiv<Identity<I, IF>, T> for T {}

// 14. 1 / T = 1 ÷ T
impl<A, I, AF, IF, PF>
    DDiv<
        Atomic<A, AF>,
        Per<Identity<I, IF>, Atomic<A, AF>, PF>,
    > for Identity<I, IF>
{
}

// 15. 1 / (L ÷ R) = R ÷ L
impl<L, R, I, LF, RF, IF, PF1, PF2>
    DDiv<
        Per<Atomic<L, LF>, Atomic<R, RF>, PF1>,
        Per<Atomic<R, RF>, Atomic<L, LF>, PF2>,
    > for Identity<I, IF>
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

////////////
// Common //
////////////

//
// Any
//

#[derive(Debug)]
pub enum DAny {}

impl<T> Refine<T> for DAny {
    type RefineError = Infallible;
}

//
// Pos
//

#[derive(Debug)]
pub enum DPos {}

#[derive(Debug, Display)]
#[display("{:?}", self)]
pub struct PosError<T>(T)
where
    T: Debug;
impl<T> Error for PosError<T> where T: Debug {}

impl<T> Refine<T> for DPos
where
    T: Ord + Zero + Debug,
{
    type RefineError = PosError<T>;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        if rep > Zero::zero() {
            Ok(rep)
        } else {
            PosError(rep).pipe(Err)
        }
    }
}

//
// NonNeg
//

#[derive(Debug)]
pub enum DNonNeg {}

#[derive(Debug, Display)]
#[display("{:?}", self)]
pub struct NonNegError<T>(T)
where
    T: Debug;
impl<T> Error for NonNegError<T> where T: Debug {}

impl<T> Refine<T> for DNonNeg
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
