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
pub struct Identity<I, F>(PhantomData<(I, F)>);
#[derive(Debug)]
pub struct Atomic<A, F>(PhantomData<(A, F)>);
#[derive(Debug)]
pub struct Times<L, R, F>(PhantomData<(L, R, F)>);
#[derive(Debug)]
pub struct Per<L, R, F>(PhantomData<(L, R, F)>);

pub trait Raffinate {
    type Refinery;
}
impl<I, F> Raffinate for Identity<I, F> {
    type Refinery = F;
}
impl<A, F> Raffinate for Atomic<A, F> {
    type Refinery = F;
}
impl<L, R, F> Raffinate for Times<L, R, F> {
    type Refinery = F;
}
impl<L, R, F> Raffinate for Per<L, R, F> {
    type Refinery = F;
}

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
impl<I, IF, T> DMul<T, T> for Identity<I, IF> {}

// A * 1 = A
impl<A, AF, I, IF> DMul<Identity<I, IF>, Atomic<A, AF>>
    for Atomic<A, AF>
{
}

// (L × R) * 1 = L × R
impl<L, R, TF1, I, IF>
    DMul<Identity<I, IF>, Times<L, R, TF1>>
    for Times<L, R, TF1>
{
}

// (L ÷ R) * 1 = L ÷ R
impl<L, R, PF1, I, IF> DMul<Identity<I, IF>, Per<L, R, PF1>>
    for Per<L, R, PF1>
{
}

//
// Atomic (excluding Identity)
//

// L * R = L ✖ R
impl<L, LF, R, RF, TF3>
    DMul<
        Atomic<R, RF>,
        Times<Atomic<L, LF>, Atomic<R, RF>, TF3>,
    > for Atomic<L, LF>
{
}

// A * (L ✖ R) = A ✖ (L ✖ R)
impl<A, AF, L, R, TF2, TF3>
    DMul<
        Times<L, R, TF2>,
        Times<Atomic<A, AF>, Times<L, R, TF2>, TF3>,
    > for Atomic<A, AF>
{
}

// A * (L ÷ R) = A ✖ (L ÷ R)
impl<A, AF, L, R, PF2, TF3>
    DMul<
        Per<L, R, PF2>,
        Times<Atomic<A, AF>, Per<L, R, PF2>, TF3>,
    > for Atomic<A, AF>
{
}

// (L ✖ R) * A = (L ✖ R) ✖ A
impl<L, R, TF1, A, AF, TF3>
    DMul<
        Atomic<A, AF>,
        Times<Times<L, R, TF1>, Atomic<A, AF>, TF3>,
    > for Times<L, R, TF1>
{
}

// (L ÷ R) * A = (L ÷ R) ✖ A
impl<L, R, PF1, A, AF, TF3>
    DMul<
        Atomic<A, AF>,
        Times<Per<L, R, PF1>, Atomic<A, AF>, TF3>,
    > for Per<L, R, PF1>
{
}

//
// Times (excluding Identity and Atomic)
//

// (L1 × R1) * (L2 × R2) = (L1 × R1) × (L2 × R2)
impl<L1, R1, TF1, L2, R2, TF2, TF3>
    DMul<
        Times<L2, R2, TF2>,
        Times<Times<L1, R1, TF1>, Times<L2, R2, TF2>, TF3>,
    > for Times<L1, R1, TF1>
{
}

// (L1 × R1) * (L2 ÷ R2) = (L1 × R1) × (L2 ÷ R2)
impl<L1, R1, TF1, L2, R2, PF2, TF3>
    DMul<
        Per<L2, R2, PF2>,
        Times<Times<L1, R1, TF1>, Per<L2, R2, PF2>, TF3>,
    > for Times<L1, R1, TF1>
{
}

// (L1 ÷ R1) * (L2 × R2) = (L1 ÷ R1) × (L2 × R2)
impl<L1, R1, PF1, L2, R2, TF2, TF3>
    DMul<
        Times<L2, R2, TF2>,
        Times<Per<L1, R1, PF1>, Times<L2, R2, TF2>, TF3>,
    > for Per<L1, R1, PF1>
{
}

//
// Per (excluding Identity, Atomic and Times)
//

// (L1 ÷ R1) * (L2 ÷ R2) = (L1 ÷ R1) × (L2 ÷ R2)
impl<L1, R1, PF1, L2, R2, PF2, TF3>
    DMul<
        Per<L2, R2, PF2>,
        Times<Per<L1, R1, PF1>, Per<L2, R2, PF2>, TF3>,
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
impl<I, IF, A, AF, PF3>
    DDiv<
        Atomic<A, AF>,
        Per<Identity<I, IF>, Atomic<A, AF>, PF3>,
    > for Identity<I, IF>
{
}

// 1 / (L ✖ R) = 1 ÷ (L ✖ R)
impl<I, IF, L, R, TF2, PF3>
    DDiv<
        Times<L, R, TF2>,
        Per<Identity<I, IF>, Times<L, R, TF2>, PF3>,
    > for Identity<I, IF>
{
}

// 1 / (L ÷ R) = 1 ÷ (L ÷ R)
impl<I, IF, L, R, PF2, PF3>
    DDiv<
        Per<L, R, PF2>,
        Per<Identity<I, IF>, Per<L, R, PF2>, PF3>,
    > for Identity<I, IF>
{
}

//
// Atomic (excluding Identity)
//

// L / R = L ÷ R
impl<L, LF, R, RF, PF3>
    DDiv<
        Atomic<R, RF>,
        Per<Atomic<L, LF>, Atomic<R, RF>, PF3>,
    > for Atomic<L, LF>
{
}

// A / (L ✖ R) = A ÷ (L ✖ R)
impl<A, AF, L, R, TF2, PF3>
    DDiv<
        Times<L, R, TF2>,
        Per<Atomic<A, AF>, Times<L, R, TF2>, PF3>,
    > for Atomic<A, AF>
{
}

// A / (L ÷ R) = A ÷ (L ÷ R)
impl<A, AF, L, R, PF2, PF3>
    DDiv<
        Per<L, R, PF2>,
        Per<Atomic<A, AF>, Per<L, R, PF2>, PF3>,
    > for Atomic<A, AF>
{
}

// (L ✖ R) / A = (L ✖ R) ÷ A
impl<L, R, TF1, A, AF, PF3>
    DDiv<
        Atomic<A, AF>,
        Per<Times<L, R, TF1>, Atomic<A, AF>, PF3>,
    > for Times<L, R, TF1>
{
}

// (L ÷ R) / A = (L ÷ R) ÷ A
impl<L, R, PF1, A, AF, PF3>
    DDiv<
        Atomic<A, AF>,
        Per<Per<L, R, PF1>, Atomic<A, AF>, PF3>,
    > for Per<L, R, PF1>
{
}

//
// Times (excluding Identity and Atomic)
//

// (L1 × R1) / (L2 × R2) = (L1 × R1) ÷ (L2 × R2)
impl<L1, R1, TF1, L2, R2, TF2, PF3>
    DDiv<
        Times<L2, R2, TF2>,
        Per<Times<L1, R1, TF1>, Times<L2, R2, TF2>, PF3>,
    > for Times<L1, R1, TF1>
{
}

// (L1 × R1) / (L2 ÷ R2) = (L1 × R1) ÷ (L2 ÷ R2)
impl<L1, R1, TF1, L2, R2, PF2, PF3>
    DDiv<
        Per<L2, R2, PF2>,
        Per<Times<L1, R1, TF1>, Per<L2, R2, PF2>, PF3>,
    > for Times<L1, R1, TF1>
{
}

// (L1 ÷ R1) / (L2 × R2) = (L1 ÷ R1) ÷ (L2 × R2)
impl<L1, R1, PF1, L2, R2, TF2, PF3>
    DDiv<
        Times<L2, R2, TF2>,
        Per<Per<L1, R1, PF1>, Times<L2, R2, TF2>, PF3>,
    > for Per<L1, R1, PF1>
{
}

//
// Per (excluding Identity, Atomic and Times)
//

// (L1 ÷ R1) / (L2 ÷ R2) = (L1 ÷ R1) ÷ (L2 ÷ R2)
impl<L1, R1, PF1, L2, R2, PF2, PF3>
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
impl<L, LF, R, TF2, I, IF, PF3>
    DDiv<
        Times<Atomic<L, LF>, R, TF2>,
        Per<Identity<I, IF>, R, PF3>,
    > for Atomic<L, LF>
{
}

// L / (L ÷ R) = R
impl<L, R, PF> DDiv<Per<L, R, PF>, R> for L {}

// (L ÷ R) / L = 1 ÷ R
impl<L, LF, R, PF1, I, IF, PF3>
    DDiv<Atomic<L, LF>, Per<Identity<I, IF>, R, PF3>>
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

impl<T, D, F> TAdd for Tagged<T, D, F>
where
    T: Copy + TAdd,
    D: Debug,
    F: Debug + Refine<T>,
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

impl<T, D, F> TSub for Tagged<T, D, F>
where
    T: Copy + TSub,
    D: Debug,
    F: Debug + Refine<T>,
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

impl<T, L, LF, R, RF, O, OF>
    TMul<Tagged<T, R, RF>, Tagged<T, O, O::Refinery>>
    for Tagged<T, L, LF>
where
    T: Copy + TMul<T, T>,
    L: Debug + DMul<R, O>,
    R: Debug,
    O: Raffinate<Refinery = OF>,
    LF: Debug,
    RF: Debug,
    OF: Refine<T>,
{
    fn tmul(
        &self,
        rhs: &Tagged<T, R, RF>,
    ) -> Result<
        Tagged<T, O, OF>,
        TNumError<Self, Tagged<T, R, RF>>,
    > {
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

impl<T, L, LF, R, RF, O, OF>
    TDiv<Tagged<T, R, RF>, Tagged<T, O, O::Refinery>>
    for Tagged<T, L, LF>
where
    T: Copy + TDiv<T, T>,
    L: Debug + DDiv<R, O>,
    R: Debug,
    O: Raffinate<Refinery = OF>,
    LF: Debug,
    RF: Debug,
    OF: Refine<T>,
{
    fn tdiv(
        &self,
        rhs: &Tagged<T, R, RF>,
    ) -> Result<
        Tagged<T, O, OF>,
        TNumError<Self, Tagged<T, R, RF>>,
    > {
        self.rep()
            .tdiv(rhs.rep())
            .map_err(|_| TNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| TNumError::Div(*self, *rhs))
    }
}
