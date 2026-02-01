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
        rhs: &Tagged<Rep, Tag>,
    ) -> Result<
        Rep,
        FNumError<Tagged<Rep, Tag>, Tagged<Rep, Tag>>,
    > {
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
