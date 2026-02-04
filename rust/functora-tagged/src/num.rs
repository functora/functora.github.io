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
pub struct Scalar<S>(PhantomData<S>);

#[derive(Debug)]
pub struct Times<L, R, A>(PhantomData<(L, R, A)>);

#[derive(Debug)]
pub struct Per<L, R, A>(PhantomData<(L, R, A)>);

pub trait IsScalar {}

impl<T, I> Refine<T> for Identity<I>
where
    I: Refine<T>,
{
    type RefineError = I::RefineError;
    fn refine(rep: T) -> Result<T, Self::RefineError> {
        I::refine(rep)
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

// 1. L * R = Times<L, R>
impl<L, R, A> DMul<R, Times<L, R, A>> for L {}

// 2. Per<L, R> * R = L
impl<L, R, A> DMul<R, L> for Per<L, R, A> {}

// 3. R * Per<L, R> = L
impl<L, R, A> DMul<Per<L, R, A>, L> for R {}

// 4. L / R = Per<L, R, A>
impl<L, R, A> DDiv<R, Per<L, R, A>> for L
where
    L: IsScalar,
    R: IsScalar,
{
}

// 5. L / Per<L, R, A> = R
impl<L, R, A> DDiv<Per<L, R, A>, R> for L {}

// 6. Times<L, R, A> / R = L
impl<L, R, A> DDiv<R, L> for Times<L, R, A> {}

// 8. Per<L, R> / L = Per<Identity, R>
impl<L, R, A, I, B> DDiv<L, Per<Identity<I>, R, B>>
    for Per<L, R, A>
where
    L: IsScalar,
{
}

// 9. Identity * Any = Any
impl<T, I> DMul<T, T> for Identity<I> {}

// 10. Scalar * Identity = Scalar
impl<S, I> DMul<Identity<I>, S> for S where S: IsScalar {}

// 11. Per * Identity = Per
impl<L, R, A, I> DMul<Identity<I>, Per<L, R, A>>
    for Per<L, R, A>
{
}

// 12. Times * Identity = Times
impl<L, R, A, I> DMul<Identity<I>, Times<L, R, A>>
    for Times<L, R, A>
{
}

// 13. Scalar / Identity = Scalar
impl<S, I> DDiv<Identity<I>, S> for S where S: IsScalar {}

// 14. Per / Identity = Per
impl<L, R, A, I> DDiv<Identity<I>, Per<L, R, A>>
    for Per<L, R, A>
{
}

// 15. Times / Identity = Times
impl<L, R, A, I> DDiv<Identity<I>, Times<L, R, A>>
    for Times<L, R, A>
{
}

// 16. Identity / Scalar = Per<Identity, Scalar>
impl<I, S, A> DDiv<S, Per<Identity<I>, S, A>>
    for Identity<I>
where
    S: IsScalar,
{
}

// 17. Identity / Per<L, R> = Per<R, L>
impl<I, L, R, A, B> DDiv<Per<L, R, A>, Per<R, L, B>>
    for Identity<I>
{
}

// 18. Identity / Times<L, R> = Per<Identity, Times<L, R>>
impl<I, L, R, A, B>
    DDiv<
        Times<L, R, A>,
        Per<Identity<I>, Times<L, R, A>, B>,
    > for Identity<I>
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

pub trait FAdd: Sized + Debug {
    fn fadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>>;
}

impl<T> FAdd for T
where
    T: Copy + Debug + CheckedAdd,
{
    fn fadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<T, T>> {
        self.checked_add(rhs)
            .ok_or(FNumError::Add(*self, *rhs))
    }
}

impl<T, D> FAdd for Tagged<T, D>
where
    T: Copy + FAdd,
    D: Debug + Refine<T>,
{
    fn fadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>> {
        self.rep()
            .fadd(rhs.rep())
            .map_err(|_| FNumError::Add(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Add(*self, *rhs))
    }
}

///////////
//  Sub  //
///////////

pub trait FSub: Sized + Debug {
    fn fsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>>;
}

impl<T> FSub for T
where
    T: Copy + Debug + CheckedSub,
{
    fn fsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<T, T>> {
        self.checked_sub(rhs)
            .ok_or(FNumError::Sub(*self, *rhs))
    }
}

impl<T, D> FSub for Tagged<T, D>
where
    T: Copy + FSub,
    D: Debug + Refine<T>,
{
    fn fsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>> {
        self.rep()
            .fsub(rhs.rep())
            .map_err(|_| FNumError::Sub(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Sub(*self, *rhs))
    }
}

///////////
//  Mul  //
///////////

pub trait FMul<R, O>: Sized + Debug
where
    R: Debug,
{
    fn fmul(
        &self,
        rhs: &R,
    ) -> Result<O, FNumError<Self, R>>;
}

impl<T> FMul<T, T> for T
where
    T: Copy + Debug + CheckedMul,
{
    fn fmul(&self, rhs: &T) -> Result<T, FNumError<T, T>> {
        self.checked_mul(rhs)
            .ok_or(FNumError::Mul(*self, *rhs))
    }
}

impl<T, L, R, O> FMul<Tagged<T, R>, Tagged<T, O>>
    for Tagged<T, L>
where
    T: Copy + FMul<T, T>,
    L: Debug + DMul<R, O>,
    R: Debug,
    O: Refine<T>,
{
    fn fmul(
        &self,
        rhs: &Tagged<T, R>,
    ) -> Result<Tagged<T, O>, FNumError<Self, Tagged<T, R>>>
    {
        self.rep()
            .fmul(rhs.rep())
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

///////////
//  Div  //
///////////

pub trait FDiv<R, O>: Sized + Debug
where
    R: Debug,
{
    fn fdiv(
        &self,
        rhs: &R,
    ) -> Result<O, FNumError<Self, R>>;
}

impl<T> FDiv<T, T> for T
where
    T: Copy + Debug + CheckedDiv,
{
    fn fdiv(&self, rhs: &T) -> Result<T, FNumError<T, T>> {
        self.checked_div(rhs)
            .ok_or(FNumError::Div(*self, *rhs))
    }
}

impl<T, L, R, O> FDiv<Tagged<T, R>, Tagged<T, O>>
    for Tagged<T, L>
where
    T: Copy + FDiv<T, T>,
    L: Debug + DDiv<R, O>,
    R: Debug,
    O: Refine<T>,
{
    fn fdiv(
        &self,
        rhs: &Tagged<T, R>,
    ) -> Result<Tagged<T, O>, FNumError<Self, Tagged<T, R>>>
    {
        self.rep()
            .fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}
