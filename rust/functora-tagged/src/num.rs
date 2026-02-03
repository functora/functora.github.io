use crate::refine::*;
use crate::tagged::*;
use derive_more::Display;
use num_traits::*;
use std::error::Error;
use tap::prelude::*;

#[derive(Eq, PartialEq, Debug, Display)]
#[display("{:?}", self)]
pub enum FNumError<L, R>
where
    L: Debug,
    R: Debug,
{
    Add(L, R),
    Sub(L, R),
    Mul(L, R),
    Div(L, R),
}

impl<L, R> Error for FNumError<L, R>
where
    L: Debug,
    R: Debug,
{
}

#[derive(Debug)]
pub enum TimesTag {}
pub type Times<L, R> = (TimesTag, L, R);

#[derive(Debug)]
pub enum PerTag {}
pub type Per<L, R> = (PerTag, L, R);

/////////
// Add //
/////////

pub trait FAdd
where
    Self: Sized + Debug,
{
    fn fadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>>;
}

//
// T + T = T
//
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

//
// Tag + Tag = Tag
//
impl<T, Tag> FAdd for Tagged<T, Tag>
where
    T: Copy + Debug + FAdd,
    Tag: Debug + Refine<T>,
{
    fn fadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>> {
        self.rep()
            .fadd(rhs.rep())
            .map_err(|_| FNumError::Add(*self, *rhs))?
            .pipe(Self::new)
            .map_err(|_| FNumError::Add(*self, *rhs))
    }
}

/////////
// Sub //
/////////

pub trait FSub
where
    Self: Sized + Debug,
{
    fn fsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>>;
}

//
// T - T = T
//
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

//
// Tag - Tag = Tag
//
impl<T, Tag> FSub for Tagged<T, Tag>
where
    T: Copy + Debug + FSub,
    Tag: Debug + Refine<T>,
{
    fn fsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>> {
        self.rep()
            .fsub(rhs.rep())
            .map_err(|_| FNumError::Sub(*self, *rhs))?
            .pipe(Self::new)
            .map_err(|_| FNumError::Sub(*self, *rhs))
    }
}

/////////
// Gap //
/////////

pub trait FGap
where
    Self: Sized + Debug,
{
    fn fgap(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>>;
}

//
// T - T = T
//
impl<T> FGap for T
where
    T: Ord + Copy + Debug + CheckedSub,
{
    fn fgap(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<T, T>> {
        self.max(rhs)
            .checked_sub(self.min(rhs))
            .ok_or(FNumError::Sub(*self, *rhs))
    }
}

//
// Tag - Tag = Tag
//
impl<T, Tag> FGap for Tagged<T, Tag>
where
    T: Ord + Copy + Debug + FSub,
    Tag: Debug + Refine<T>,
{
    fn fgap(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>> {
        self.max(rhs).fsub(self.min(rhs))
    }
}

/////////
// Mul //
/////////

pub trait FMul<Rhs, Output>
where
    Self: Sized + Debug,
    Rhs: Debug,
{
    fn fmul(
        &self,
        rhs: &Rhs,
    ) -> Result<Output, FNumError<Self, Rhs>>;
}

//
// T * T = T
//
impl<T> FMul<T, T> for T
where
    T: Copy + Debug + CheckedMul,
{
    fn fmul(&self, rhs: &T) -> Result<T, FNumError<T, T>> {
        self.checked_mul(rhs)
            .ok_or(FNumError::Mul(*self, *rhs))
    }
}

//
// T * Tag = Tag
//
impl<T, Tag> FMul<Tagged<T, Tag>, Tagged<T, Tag>> for T
where
    T: Copy + Debug + FMul<T, T>,
    Tag: Debug + Refine<T>,
{
    fn fmul(
        &self,
        rhs: &Tagged<T, Tag>,
    ) -> Result<Tagged<T, Tag>, FNumError<T, Tagged<T, Tag>>>
    {
        self.fmul(rhs.rep())
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

//
// Tag * T = Tag
//
impl<T, Tag> FMul<T, Tagged<T, Tag>> for Tagged<T, Tag>
where
    T: Copy + Debug + FMul<T, T>,
    Tag: Debug + Refine<T>,
{
    fn fmul(
        &self,
        rhs: &T,
    ) -> Result<Tagged<T, Tag>, FNumError<Tagged<T, Tag>, T>>
    {
        self.rep()
            .fmul(rhs)
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

//
// L * R = Times<L, R>
//
impl<T, L, R> FMul<Tagged<T, R>, Tagged<T, Times<L, R>>>
    for Tagged<T, L>
where
    T: Copy + Debug + FMul<T, T>,
    L: Debug,
    R: Debug,
    Times<L, R>: Refine<T>,
{
    fn fmul(
        &self,
        rhs: &Tagged<T, R>,
    ) -> Result<
        Tagged<T, Times<L, R>>,
        FNumError<Tagged<T, L>, Tagged<T, R>>,
    > {
        self.rep()
            .fmul(rhs.rep())
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

//
// L * Per<R, L> = R
//
impl<T, L, R> FMul<Tagged<T, Per<R, L>>, Tagged<T, R>>
    for Tagged<T, L>
where
    T: Copy + Debug + FMul<T, T>,
    L: Debug,
    R: Debug + Refine<T>,
    Per<R, L>: Debug,
{
    fn fmul(
        &self,
        rhs: &Tagged<T, Per<R, L>>,
    ) -> Result<
        Tagged<T, R>,
        FNumError<Tagged<T, L>, Tagged<T, Per<R, L>>>,
    > {
        self.rep()
            .fmul(rhs.rep())
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

//
// Per<L, R> * R = L
//
impl<T, L, R> FMul<Tagged<T, R>, Tagged<T, L>>
    for Tagged<T, Per<L, R>>
where
    T: Copy + Debug + FMul<T, T>,
    L: Debug + Refine<T>,
    R: Debug,
    Per<L, R>: Debug,
{
    fn fmul(
        &self,
        rhs: &Tagged<T, R>,
    ) -> Result<
        Tagged<T, L>,
        FNumError<Tagged<T, Per<L, R>>, Tagged<T, R>>,
    > {
        self.rep()
            .fmul(rhs.rep())
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

/////////
// Div //
/////////

pub trait FDiv<Rhs, Output>
where
    Self: Sized + Debug,
    Rhs: Debug,
{
    fn fdiv(
        &self,
        rhs: &Rhs,
    ) -> Result<Output, FNumError<Self, Rhs>>;
}

//
// T / T = T
//
impl<T> FDiv<T, T> for T
where
    T: Copy + Debug + CheckedDiv,
{
    fn fdiv(&self, rhs: &T) -> Result<T, FNumError<T, T>> {
        self.checked_div(rhs)
            .ok_or(FNumError::Div(*self, *rhs))
    }
}

//
// Tag / T = Tag
//
impl<T, Tag> FDiv<T, Tagged<T, Tag>> for Tagged<T, Tag>
where
    T: Copy + Debug + FDiv<T, T>,
    Tag: Debug + Refine<T>,
{
    fn fdiv(
        &self,
        rhs: &T,
    ) -> Result<Tagged<T, Tag>, FNumError<Tagged<T, Tag>, T>>
    {
        self.rep()
            .fdiv(rhs)
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}

//
// T / Tag = Per<T, Tag>
//
impl<T, Tag> FDiv<Tagged<T, Tag>, Tagged<T, Per<T, Tag>>>
    for T
where
    T: Copy + Debug + FDiv<T, T>,
    Tag: Debug,
    Per<T, Tag>: Refine<T>,
{
    fn fdiv(
        &self,
        rhs: &Tagged<T, Tag>,
    ) -> Result<
        Tagged<T, Per<T, Tag>>,
        FNumError<T, Tagged<T, Tag>>,
    > {
        self.fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}

//
// Tag / Tag = T
//
impl<T, Tag> FDiv<Tagged<T, Tag>, T> for Tagged<T, Tag>
where
    T: Copy + Debug + FDiv<T, T>,
    Tag: Debug + Refine<T>,
{
    fn fdiv(
        &self,
        rhs: &Tagged<T, Tag>,
    ) -> Result<T, FNumError<Tagged<T, Tag>, Tagged<T, Tag>>>
    {
        self.rep()
            .fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}

//
// L / R = Per<L, R>
//
impl<T, L, R> FDiv<Tagged<T, R>, Tagged<T, Per<L, R>>>
    for Tagged<T, L>
where
    T: Copy + Debug + FDiv<T, T>,
    L: Debug,
    R: Debug,
    Per<L, R>: Refine<T>,
{
    fn fdiv(
        &self,
        rhs: &Tagged<T, R>,
    ) -> Result<
        Tagged<T, Per<L, R>>,
        FNumError<Tagged<T, L>, Tagged<T, R>>,
    > {
        self.rep()
            .fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}

//
// L / Per<L, R> = R
//
impl<T, L, R> FDiv<Tagged<T, Per<L, R>>, Tagged<T, R>>
    for Tagged<T, L>
where
    T: Copy + Debug + FDiv<T, T>,
    L: Debug,
    R: Debug + Refine<T>,
    Per<L, R>: Debug,
{
    fn fdiv(
        &self,
        rhs: &Tagged<T, Per<L, R>>,
    ) -> Result<
        Tagged<T, R>,
        FNumError<Tagged<T, L>, Tagged<T, Per<L, R>>>,
    > {
        self.rep()
            .fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}

//
// Per<L, R> / L = Per<T, R>
//
impl<T, L, R> FDiv<Tagged<T, L>, Tagged<T, Per<T, R>>>
    for Tagged<T, Per<L, R>>
where
    T: Copy + Debug + FDiv<T, T>,
    L: Debug,
    R: Debug,
    Per<L, R>: Debug,
    Per<T, R>: Refine<T>,
{
    fn fdiv(
        &self,
        rhs: &Tagged<T, L>,
    ) -> Result<
        Tagged<T, Per<T, R>>,
        FNumError<Tagged<T, Per<L, R>>, Tagged<T, L>>,
    > {
        self.rep()
            .fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}
