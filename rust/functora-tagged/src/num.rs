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

#[derive(Debug)]
pub enum TimesTag {}
pub type Times<LTag, RTag> = (TimesTag, LTag, RTag);

#[derive(Debug)]
pub enum PerTag {}
pub type Per<LTag, RTag> = (PerTag, LTag, RTag);

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
// Rep + Rep = Rep
//
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

//
// Tag + Tag = Tag
//
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
// Rep - Rep = Rep
//
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

//
// Tag - Tag = Tag
//
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
// Rep - Rep = Rep
//
impl<Rep> FGap for Rep
where
    Rep: Ord + Copy + Debug + CheckedSub,
{
    fn fgap(
        &self,
        rhs: &Self,
    ) -> Result<Self, FNumError<Rep, Rep>> {
        self.max(rhs)
            .checked_sub(self.min(rhs))
            .ok_or(FNumError::Sub(*self, *rhs))
    }
}

//
// Tag - Tag = Tag
//
impl<Rep, Tag> FGap for Tagged<Rep, Tag>
where
    Rep: Ord + Copy + Debug + FSub,
    Tag: Debug + Refine<Rep>,
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
// Rep * Rep = Rep
//
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

//
// Rep * Tag = Tag
//
impl<Rep, Tag> FMul<Tagged<Rep, Tag>, Tagged<Rep, Tag>>
    for Rep
where
    Rep: Copy + Debug + FMul<Rep, Rep>,
    Tag: Debug + Refine<Rep>,
{
    fn fmul(
        &self,
        rhs: &Tagged<Rep, Tag>,
    ) -> Result<
        Tagged<Rep, Tag>,
        FNumError<Rep, Tagged<Rep, Tag>>,
    > {
        self.fmul(rhs.rep())
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

//
// Tag * Rep = Tag
//
impl<Rep, Tag> FMul<Rep, Tagged<Rep, Tag>>
    for Tagged<Rep, Tag>
where
    Rep: Copy + Debug + FMul<Rep, Rep>,
    Tag: Debug + Refine<Rep>,
{
    fn fmul(
        &self,
        rhs: &Rep,
    ) -> Result<
        Tagged<Rep, Tag>,
        FNumError<Tagged<Rep, Tag>, Rep>,
    > {
        self.rep()
            .fmul(rhs)
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

//
// LTag * RTag = Times<LTag, RTag>
//
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
// LTag * Per<RTag, LTag> = RTag
//
impl<Rep, LTag, RTag>
    FMul<Tagged<Rep, Per<RTag, LTag>>, Tagged<Rep, RTag>>
    for Tagged<Rep, LTag>
where
    Rep: Copy + Debug + FMul<Rep, Rep>,
    LTag: Debug,
    RTag: Debug + Refine<Rep>,
    Per<RTag, LTag>: Debug,
{
    fn fmul(
        &self,
        rhs: &Tagged<Rep, Per<RTag, LTag>>,
    ) -> Result<
        Tagged<Rep, RTag>,
        FNumError<
            Tagged<Rep, LTag>,
            Tagged<Rep, Per<RTag, LTag>>,
        >,
    > {
        self.rep()
            .fmul(rhs.rep())
            .map_err(|_| FNumError::Mul(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Mul(*self, *rhs))
    }
}

//
// Per<LTag, RTag> * RTag = LTag
//
impl<Rep, LTag, RTag>
    FMul<Tagged<Rep, RTag>, Tagged<Rep, LTag>>
    for Tagged<Rep, Per<LTag, RTag>>
where
    Rep: Copy + Debug + FMul<Rep, Rep>,
    LTag: Debug + Refine<Rep>,
    RTag: Debug,
    Per<LTag, RTag>: Debug,
{
    fn fmul(
        &self,
        rhs: &Tagged<Rep, RTag>,
    ) -> Result<
        Tagged<Rep, LTag>,
        FNumError<
            Tagged<Rep, Per<LTag, RTag>>,
            Tagged<Rep, RTag>,
        >,
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
// Rep / Rep = Rep
//
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

//
// Tag / Rep = Tag
//
impl<Rep, Tag> FDiv<Rep, Tagged<Rep, Tag>>
    for Tagged<Rep, Tag>
where
    Rep: Copy + Debug + FDiv<Rep, Rep>,
    Tag: Debug + Refine<Rep>,
{
    fn fdiv(
        &self,
        rhs: &Rep,
    ) -> Result<
        Tagged<Rep, Tag>,
        FNumError<Tagged<Rep, Tag>, Rep>,
    > {
        self.rep()
            .fdiv(rhs)
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}

//
// Rep / Tag = Per<Rep, Tag>
//
impl<Rep, Tag>
    FDiv<Tagged<Rep, Tag>, Tagged<Rep, Per<Rep, Tag>>>
    for Rep
where
    Rep: Copy + Debug + FDiv<Rep, Rep>,
    Tag: Debug,
    Per<Rep, Tag>: Refine<Rep>,
{
    fn fdiv(
        &self,
        rhs: &Tagged<Rep, Tag>,
    ) -> Result<
        Tagged<Rep, Per<Rep, Tag>>,
        FNumError<Rep, Tagged<Rep, Tag>>,
    > {
        self.fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}

//
// Tag / Tag = Rep
//
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

//
// LTag / RTag = Per<LTag, RTag>
//
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

//
// LTag / Per<LTag, RTag> = RTag
//
impl<Rep, LTag, RTag>
    FDiv<Tagged<Rep, Per<LTag, RTag>>, Tagged<Rep, RTag>>
    for Tagged<Rep, LTag>
where
    Rep: Copy + Debug + FDiv<Rep, Rep>,
    LTag: Debug,
    RTag: Debug + Refine<Rep>,
    Per<LTag, RTag>: Debug,
{
    fn fdiv(
        &self,
        rhs: &Tagged<Rep, Per<LTag, RTag>>,
    ) -> Result<
        Tagged<Rep, RTag>,
        FNumError<
            Tagged<Rep, LTag>,
            Tagged<Rep, Per<LTag, RTag>>,
        >,
    > {
        self.rep()
            .fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}

//
// Per<LTag, RTag> / LTag = Per<Rep, RTag>
//
impl<Rep, LTag, RTag>
    FDiv<Tagged<Rep, LTag>, Tagged<Rep, Per<Rep, RTag>>>
    for Tagged<Rep, Per<LTag, RTag>>
where
    Rep: Copy + Debug + FDiv<Rep, Rep>,
    LTag: Debug,
    RTag: Debug,
    Per<LTag, RTag>: Debug,
    Per<Rep, RTag>: Refine<Rep>,
{
    fn fdiv(
        &self,
        rhs: &Tagged<Rep, LTag>,
    ) -> Result<
        Tagged<Rep, Per<Rep, RTag>>,
        FNumError<
            Tagged<Rep, Per<LTag, RTag>>,
            Tagged<Rep, LTag>,
        >,
    > {
        self.rep()
            .fdiv(rhs.rep())
            .map_err(|_| FNumError::Div(*self, *rhs))?
            .pipe(Tagged::new)
            .map_err(|_| FNumError::Div(*self, *rhs))
    }
}
