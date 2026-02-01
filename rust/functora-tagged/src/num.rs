use crate::refine::*;
use crate::tagged::*;
use derive_more::Display;
use num_traits::*;
use std::error::Error;
use tap::prelude::*;

#[derive(Eq, PartialEq, Debug, Display)]
#[display("{:?}", self)]
pub enum FNumError<Lhs, Rhs>
where
    Lhs: Debug,
    Rhs: Debug,
{
    Add(Lhs, Rhs),
    Sub(Lhs, Rhs),
    Mul(Lhs, Rhs),
    Div(Lhs, Rhs),
}

impl<Lhs, Rhs> Error for FNumError<Lhs, Rhs>
where
    Lhs: Debug,
    Rhs: Debug,
{
}

//
// Add
//

pub trait FAdd
where
    Self: Sized + Debug,
{
    fn fadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>>;
}

impl<Rep> FAdd for Rep
where
    Rep: Copy + Debug + CheckedAdd,
{
    fn fadd(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Rep, Rep>> {
        self.checked_add(rhs)
            .ok_or(FNumError::Add(*self, *rhs))
    }
}

impl<Rep, Tag> FAdd for Tagged<Rep, Tag>
where
    Rep: Copy + Debug + FAdd,
    Tag: Debug + Refine<Rep>,
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

//
// Sub
//

pub trait FSub
where
    Self: Sized + Debug,
{
    fn fsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Self, Self>>;
}

impl<Rep> FSub for Rep
where
    Rep: Copy + Debug + CheckedSub,
{
    fn fsub(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Rep, Rep>> {
        self.checked_sub(rhs)
            .ok_or(FNumError::Sub(*self, *rhs))
    }
}

impl<Rep, Tag> FSub for Tagged<Rep, Tag>
where
    Rep: Copy + Debug + FSub,
    Tag: Debug + Refine<Rep>,
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

//
// Mul
//

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

impl<Rep> FMul<Rep, Rep> for Rep
where
    Rep: Copy + Debug + CheckedMul,
{
    fn fmul(
        &self,
        rhs: &Rep,
    ) -> Result<Rep, FNumError<Rep, Rep>> {
        self.checked_mul(rhs)
            .ok_or(FNumError::Mul(*self, *rhs))
    }
}

pub enum TimesTag {}
pub type Times<LTag, RTag> = (TimesTag, LTag, RTag);

impl<Rep, LTag, RTag>
    FMul<Tagged<Rep, RTag>, Tagged<Rep, Times<LTag, RTag>>>
    for Tagged<Rep, LTag>
where
    Rep: Copy + Debug + FMul<Rep, Rep>,
    LTag: Debug,
    RTag: Debug,
    Times<LTag, RTag>: Refine<Rep>,
{
    fn fmul(
        &self,
        rhs: &Tagged<Rep, RTag>,
    ) -> Result<
        Tagged<Rep, Times<LTag, RTag>>,
        FNumError<Tagged<Rep, LTag>, Tagged<Rep, RTag>>,
    > {
        self.rep()
            .fmul(rhs.rep())
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

//
// Div
//

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

impl<Rep> FDiv<Rep, Rep> for Rep
where
    Rep: Copy + Debug + CheckedDiv,
{
    fn fdiv(
        &self,
        rhs: &Rep,
    ) -> Result<Rep, FNumError<Rep, Rep>> {
        self.checked_div(rhs)
            .ok_or(FNumError::Div(*self, *rhs))
    }
}

impl<Rep, Tag> FDiv<Tagged<Rep, Tag>, Rep>
    for Tagged<Rep, Tag>
where
    Rep: Copy + Debug + FDiv<Rep, Rep>,
    Tag: Debug + Refine<Rep>,
{
    fn fdiv(
        &self,
        rhs: &Self,
    ) -> Result<Rep, FNumError<Self, Self>> {
        self.rep()
            .fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}

pub enum PerTag {}
pub type Per<LTag, RTag> = (PerTag, LTag, RTag);

impl<Rep, LTag, RTag>
    FDiv<Tagged<Rep, RTag>, Tagged<Rep, Per<LTag, RTag>>>
    for Tagged<Rep, LTag>
where
    Rep: Copy + Debug + FDiv<Rep, Rep>,
    LTag: Debug,
    RTag: Debug,
    Per<LTag, RTag>: Refine<Rep>,
{
    fn fdiv(
        &self,
        rhs: &Tagged<Rep, RTag>,
    ) -> Result<
        Tagged<Rep, Per<LTag, RTag>>,
        FNumError<Tagged<Rep, LTag>, Tagged<Rep, RTag>>,
    > {
        self.rep()
            .fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}
