use crate::refine::*;
use crate::tagged::*;
use derive_more::Display;
use num_traits::*;
use std::borrow::Borrow;
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
pub enum NumError<L: Debug, R: Debug> {
    Add(L, R),
    Sub(L, R),
    Mul(L, R),
    Div(L, R),
}

impl<L: Debug, R: Debug> Error for NumError<L, R> {}

///////////
//  Add  //
///////////

pub trait TAdd: Sized + Debug {
    fn tadd<Rhs: Borrow<Self>>(
        &self,
        rhs: Rhs,
    ) -> Result<Self, NumError<Self, Self>>;
}

impl<T> TAdd for T
where
    T: Copy + Debug + CheckedAdd,
{
    fn tadd<Rhs: Borrow<Self>>(
        &self,
        rhs: Rhs,
    ) -> Result<Self, NumError<Self, Self>> {
        let r = rhs.borrow();
        self.checked_add(r).ok_or(NumError::Add(*self, *r))
    }
}

impl<T, D, F> TAdd for Tagged<T, D, F>
where
    T: Copy + TAdd,
    D: Debug,
    F: Debug + Refine<T>,
{
    fn tadd<Rhs: Borrow<Self>>(
        &self,
        rhs: Rhs,
    ) -> Result<Self, NumError<Self, Self>> {
        let r = rhs.borrow();
        self.rep()
            .tadd(**r)
            .map_err(|_| NumError::Add(*self, *r))?
            .pipe(Tagged::new)
            .map_err(|_| NumError::Add(*self, *r))
    }
}

///////////
//  Sub  //
///////////

pub trait TSub: Sized + Debug {
    fn tsub<Rhs: Borrow<Self>>(
        &self,
        rhs: Rhs,
    ) -> Result<Self, NumError<Self, Self>>;
}

impl<T> TSub for T
where
    T: Copy + Debug + CheckedSub,
{
    fn tsub<Rhs: Borrow<Self>>(
        &self,
        rhs: Rhs,
    ) -> Result<Self, NumError<Self, Self>> {
        let r = rhs.borrow();
        self.checked_sub(r).ok_or(NumError::Sub(*self, *r))
    }
}

impl<T, D, F> TSub for Tagged<T, D, F>
where
    T: Copy + TSub,
    D: Debug,
    F: Debug + Refine<T>,
{
    fn tsub<Rhs: Borrow<Self>>(
        &self,
        rhs: Rhs,
    ) -> Result<Self, NumError<Self, Self>> {
        let r = rhs.borrow();
        self.rep()
            .tsub(**r)
            .map_err(|_| NumError::Sub(*self, *r))?
            .pipe(Tagged::new)
            .map_err(|_| NumError::Sub(*self, *r))
    }
}

///////////
//  Gap  //
///////////

pub trait TGap: Sized + Debug {
    fn tgap<Rhs: Borrow<Self>>(
        &self,
        rhs: Rhs,
    ) -> Result<Self, NumError<Self, Self>>;
}

impl<T> TGap for T
where
    T: Ord + Copy + Debug + TSub,
{
    fn tgap<Rhs: Borrow<Self>>(
        &self,
        rhs: Rhs,
    ) -> Result<Self, NumError<Self, Self>> {
        let r = rhs.borrow();
        self.max(r)
            .tsub(self.min(r))
            .map_err(|_| NumError::Sub(*self, *r))
    }
}

///////////
//  Mul  //
///////////

pub trait TMul<R, O>: Sized + Debug
where
    R: Debug,
{
    fn tmul<Rhs: Borrow<R>>(
        &self,
        rhs: Rhs,
    ) -> Result<O, NumError<Self, R>>;
}

impl<T> TMul<T, T> for T
where
    T: Copy + Debug + CheckedMul,
{
    fn tmul<Rhs: Borrow<T>>(
        &self,
        rhs: Rhs,
    ) -> Result<T, NumError<T, T>> {
        let r = rhs.borrow();
        self.checked_mul(r).ok_or(NumError::Mul(*self, *r))
    }
}

impl<T, L, LF, R, RF, O>
    TMul<Tagged<T, R, RF>, Tagged<T, O, O::Refinery>>
    for Tagged<T, L, LF>
where
    T: Copy + TMul<T, T>,
    L: Debug + DMul<R, O>,
    R: Debug,
    O: Raffinate,
    O::Refinery: Refine<T>,
    LF: Debug,
    RF: Debug,
{
    fn tmul<Rhs: Borrow<Tagged<T, R, RF>>>(
        &self,
        rhs: Rhs,
    ) -> Result<
        Tagged<T, O, O::Refinery>,
        NumError<Self, Tagged<T, R, RF>>,
    > {
        let r = rhs.borrow();
        self.rep()
            .tmul(**r)
            .map_err(|_| NumError::Mul(*self, *r))?
            .pipe(Tagged::new)
            .map_err(|_| NumError::Mul(*self, *r))
    }
}

///////////
//  Div  //
///////////

pub trait TDiv<R, O>: Sized + Debug
where
    R: Debug,
{
    fn tdiv<Rhs: Borrow<R>>(
        &self,
        rhs: Rhs,
    ) -> Result<O, NumError<Self, R>>;
}

impl<T> TDiv<T, T> for T
where
    T: Copy + Debug + CheckedDiv,
{
    fn tdiv<Rhs: Borrow<T>>(
        &self,
        rhs: Rhs,
    ) -> Result<T, NumError<T, T>> {
        let r = rhs.borrow();
        self.checked_div(r).ok_or(NumError::Div(*self, *r))
    }
}

impl<T, L, LF, R, RF, O>
    TDiv<Tagged<T, R, RF>, Tagged<T, O, O::Refinery>>
    for Tagged<T, L, LF>
where
    T: Copy + TDiv<T, T>,
    L: Debug + DDiv<R, O>,
    R: Debug,
    O: Raffinate,
    O::Refinery: Refine<T>,
    LF: Debug,
    RF: Debug,
{
    fn tdiv<Rhs: Borrow<Tagged<T, R, RF>>>(
        &self,
        rhs: Rhs,
    ) -> Result<
        Tagged<T, O, O::Refinery>,
        NumError<Self, Tagged<T, R, RF>>,
    > {
        let r = rhs.borrow();
        self.rep()
            .tdiv(**r)
            .map_err(|_| NumError::Div(*self, *r))?
            .pipe(Tagged::new)
            .map_err(|_| NumError::Div(*self, *r))
    }
}
