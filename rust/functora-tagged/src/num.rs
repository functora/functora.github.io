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

//////////
// DMul //
//////////

pub trait DMul<R, O> {}

//
// Identity
//

// 1 * T = T
impl<T, I, IF> DMul<T, T> for Identity<I, IF> {}

// A * 1 = A
impl<A, I, AF, IF> DMul<Identity<I, IF>, Atomic<A, AF>>
    for Atomic<A, AF>
{
}

// (L × R) * 1 = L × R
impl<L, R, I, IF, TF> DMul<Identity<I, IF>, Times<L, R, TF>>
    for Times<L, R, TF>
{
}

// (L ÷ R) * 1 = L ÷ R
impl<L, R, I, IF, PF> DMul<Identity<I, IF>, Per<L, R, PF>>
    for Per<L, R, PF>
{
}

//
// Atomic (excluding Identity)
//

// L * R = L ✖ R
impl<L, R, LF, RF, TF>
    DMul<
        Atomic<R, RF>,
        Times<Atomic<L, LF>, Atomic<R, RF>, TF>,
    > for Atomic<L, LF>
{
}

// A * (L ✖ R) = A ✖ (L ✖ R)
impl<A, L, R, AF, TF1, TF2>
    DMul<
        Times<L, R, TF1>,
        Times<Atomic<A, AF>, Times<L, R, TF1>, TF2>,
    > for Atomic<A, AF>
{
}

// A * (L ÷ R) = A ✖ (L ÷ R)
impl<A, L, R, AF, PF, TF>
    DMul<
        Per<L, R, PF>,
        Times<Atomic<A, AF>, Per<L, R, PF>, TF>,
    > for Atomic<A, AF>
{
}

// (L ✖ R) * A = (L ✖ R) ✖ A
impl<L, R, A, TF, AF, TF2>
    DMul<
        Atomic<A, AF>,
        Times<Times<L, R, TF>, Atomic<A, AF>, TF2>,
    > for Times<L, R, TF>
{
}

// (L ÷ R) * A = (L ÷ R) ✖ A
impl<L, R, A, PF, AF, TF>
    DMul<
        Atomic<A, AF>,
        Times<Per<L, R, PF>, Atomic<A, AF>, TF>,
    > for Per<L, R, PF>
{
}

//
// Times (excluding Identity and Atomic)
//

// (L1 × R1) * (L2 × R2) = (L1 × R1) × (L2 × R2)
impl<L1, R1, L2, R2, TF1, TF2, TF3>
    DMul<
        Times<L2, R2, TF2>,
        Times<Times<L1, R1, TF1>, Times<L2, R2, TF2>, TF3>,
    > for Times<L1, R1, TF1>
{
}

// (L1 × R1) * (L2 ÷ R2) = (L1 × R1) × (L2 ÷ R2)
impl<L1, R1, L2, R2, TF1, TF2, PF>
    DMul<
        Per<L2, R2, PF>,
        Times<Times<L1, R1, TF1>, Per<L2, R2, PF>, TF2>,
    > for Times<L1, R1, TF1>
{
}

// (L1 ÷ R1) * (L2 × R2) = (L1 ÷ R1) × (L2 × R2)
impl<L1, R1, L2, R2, TF2, TF3, PF>
    DMul<
        Times<L2, R2, TF2>,
        Times<Per<L1, R1, PF>, Times<L2, R2, TF2>, TF3>,
    > for Per<L1, R1, PF>
{
}

//
// Per (excluding Identity, Atomic and Times)
//

// (L1 ÷ R1) * (L2 ÷ R2) = (L1 ÷ R1) × (L2 ÷ R2)
impl<L1, R1, L2, R2, TF, PF1, PF2>
    DMul<
        Per<L2, R2, PF2>,
        Times<Per<L1, R1, PF1>, Per<L2, R2, PF2>, TF>,
    > for Per<L1, R1, PF1>
{
}

//
// Cancellation
//

// (L ÷ R) * R = L
impl<L, R, PF> DMul<R, L> for Per<L, R, PF> {}

// R * (L ÷ R) = L
impl<L, R, PF> DMul<Per<L, R, PF>, L> for R {}

//////////
// DDiv //
//////////

pub trait DDiv<R, O> {}

//
// Identity
//

// T / 1 = T
impl<T, I, IF> DDiv<Identity<I, IF>, T> for T {}

// 1 / A = 1 ÷ A
impl<A, I, AF, IF, PF>
    DDiv<
        Atomic<A, AF>,
        Per<Identity<I, IF>, Atomic<A, AF>, PF>,
    > for Identity<I, IF>
{
}

// 1 / (L ✖ R) = 1 ÷ (L ✖ R)
impl<L, R, I, IF, TF, PF>
    DDiv<
        Times<L, R, TF>,
        Per<Identity<I, IF>, Times<L, R, TF>, PF>,
    > for Identity<I, IF>
{
}

// 1 / (L ÷ R) = 1 ÷ (L ÷ R)
impl<L, R, I, IF, PF1, PF2>
    DDiv<
        Per<L, R, PF1>,
        Per<Identity<I, IF>, Per<L, R, PF1>, PF2>,
    > for Identity<I, IF>
{
}

//
// Atomic (excluding Identity)
//

// L / R = L ÷ R
impl<L, R, LF, RF, PF>
    DDiv<
        Atomic<R, RF>,
        Per<Atomic<L, LF>, Atomic<R, RF>, PF>,
    > for Atomic<L, LF>
{
}

// A / (L ✖ R) = A ÷ (L ✖ R)
impl<A, L, R, AF, TF, PF>
    DDiv<
        Times<L, R, TF>,
        Per<Atomic<A, AF>, Times<L, R, TF>, PF>,
    > for Atomic<A, AF>
{
}

// A / (L ÷ R) = A ÷ (L ÷ R)
impl<A, L, R, AF, PF, PF2>
    DDiv<
        Per<L, R, PF>,
        Per<Atomic<A, AF>, Per<L, R, PF>, PF2>,
    > for Atomic<A, AF>
{
}

// (L ✖ R) / A = (L ✖ R) ÷ A
impl<L, R, A, TF, AF, PF>
    DDiv<
        Atomic<A, AF>,
        Per<Times<L, R, TF>, Atomic<A, AF>, PF>,
    > for Times<L, R, TF>
{
}

// (L ÷ R) / A = (L ÷ R) ÷ A
impl<L, R, A, PF, AF, PF2>
    DDiv<
        Atomic<A, AF>,
        Per<Per<L, R, PF>, Atomic<A, AF>, PF2>,
    > for Per<L, R, PF>
{
}

//
// Times (excluding Identity and Atomic)
//

// (L1 × R1) / (L2 × R2) = (L1 × R1) ÷ (L2 × R2)
impl<L1, R1, L2, R2, TF1, TF2, PF>
    DDiv<
        Times<L2, R2, TF2>,
        Per<Times<L1, R1, TF1>, Times<L2, R2, TF2>, PF>,
    > for Times<L1, R1, TF1>
{
}

// (L1 × R1) / (L2 ÷ R2) = (L1 × R1) ÷ (L2 ÷ R2)
impl<L1, R1, L2, R2, TF1, PF, PF2>
    DDiv<
        Per<L2, R2, PF>,
        Per<Times<L1, R1, TF1>, Per<L2, R2, PF>, PF2>,
    > for Times<L1, R1, TF1>
{
}

// (L1 ÷ R1) / (L2 × R2) = (L1 ÷ R1) ÷ (L2 × R2)
impl<L1, R1, L2, R2, PF, TF, PF2>
    DDiv<
        Times<L2, R2, TF>,
        Per<Per<L1, R1, PF>, Times<L2, R2, TF>, PF2>,
    > for Per<L1, R1, PF>
{
}

//
// Per (excluding Identity, Atomic and Times)
//

// (L1 ÷ R1) / (L2 ÷ R2) = (L1 ÷ R1) ÷ (L2 ÷ R2)
impl<L1, R1, L2, R2, PF1, PF2, PF3>
    DDiv<
        Per<L2, R2, PF2>,
        Per<Per<L1, R1, PF1>, Per<L2, R2, PF2>, PF3>,
    > for Per<L1, R1, PF1>
{
}

//
// Cancellation
//

// (L ✖ R) / R = L
// (L ✖ R) / L = R (overlaps when L = R)
impl<L, R, TF> DDiv<R, L> for Times<L, R, TF> {}

// L / (L ✖ R) = 1 ÷ R
// R / (L ✖ R) = 1 ÷ L (overlaps when L = R)
impl<L, LF, R, I, IF, TF, PF>
    DDiv<
        Times<Atomic<L, LF>, R, TF>,
        Per<Identity<I, IF>, R, PF>,
    > for Atomic<L, LF>
{
}

// L / (L ÷ R) = R
impl<L, R, PF> DDiv<Per<L, R, PF>, R> for L {}

// (L ÷ R) / L = 1 ÷ R
impl<L, R, I, LF, IF, PF1, PF2>
    DDiv<Atomic<L, LF>, Per<Identity<I, IF>, R, PF2>>
    for Per<Atomic<L, LF>, R, PF1>
{
}

//////////////
//  Errors  //
//////////////

#[derive(Eq, PartialEq, Debug, Display)]
#[display("{:?}", self)]
pub enum TNumError<L: Debug, R: Debug> {
    Add(L, R),
    Sub(L, R),
    Mul(L, R),
    Div(L, R),
}

impl<L: Debug, R: Debug> Error for TNumError<L, R> {}

///////////
//  Add  //
///////////

pub trait TAdd: Sized + Debug {
    fn tadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, TNumError<Self, Self>>;
}

impl<T> TAdd for T
where
    T: Copy + Debug + CheckedAdd,
{
    fn tadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, TNumError<T, T>> {
        self.checked_add(rhs)
            .ok_or(TNumError::Add(*self, *rhs))
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
    ) -> Result<Self, TNumError<Self, Self>> {
        self.rep()
            .tadd(rhs.rep())
            .map_err(|_| TNumError::Add(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| TNumError::Add(*self, *rhs))
    }
}

///////////
//  Sub  //
///////////

pub trait TSub: Sized + Debug {
    fn tsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, TNumError<Self, Self>>;
}

impl<T> TSub for T
where
    T: Copy + Debug + CheckedSub,
{
    fn tsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, TNumError<T, T>> {
        self.checked_sub(rhs)
            .ok_or(TNumError::Sub(*self, *rhs))
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
    ) -> Result<Self, TNumError<Self, Self>> {
        self.rep()
            .tsub(rhs.rep())
            .map_err(|_| TNumError::Sub(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| TNumError::Sub(*self, *rhs))
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
    ) -> Result<O, TNumError<Self, R>>;
}

impl<T> TMul<T, T> for T
where
    T: Copy + Debug + CheckedMul,
{
    fn tmul(&self, rhs: &T) -> Result<T, TNumError<T, T>> {
        self.checked_mul(rhs)
            .ok_or(TNumError::Mul(*self, *rhs))
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
    ) -> Result<Tagged<T, O>, TNumError<Self, Tagged<T, R>>>
    {
        self.rep()
            .tmul(rhs.rep())
            .map_err(|_| TNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| TNumError::Mul(*self, *rhs))
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
    ) -> Result<O, TNumError<Self, R>>;
}

impl<T> TDiv<T, T> for T
where
    T: Copy + Debug + CheckedDiv,
{
    fn tdiv(&self, rhs: &T) -> Result<T, TNumError<T, T>> {
        self.checked_div(rhs)
            .ok_or(TNumError::Div(*self, *rhs))
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
    ) -> Result<Tagged<T, O>, TNumError<Self, Tagged<T, R>>>
    {
        self.rep()
            .tdiv(rhs.rep())
            .map_err(|_| TNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| TNumError::Div(*self, *rhs))
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
