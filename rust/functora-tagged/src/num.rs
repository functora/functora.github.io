use crate::refine::*;
use crate::tagged::*;
use derive_more::Display;
use num_traits::*;
use std::error::Error;
use tap::prelude::*;

#[derive(Debug, Display)]
#[display("{:?}", self)]
pub enum NumError<Num, Rep>
where
    Num: Debug,
    Rep: Debug,
{
    Underflow(Rep),
    Add(Num, Num),
    Sub(Num, Num),
    Div(Num, Num),
    MulRep(Num, Rep),
    DivRep(Num, Rep),
}

impl<Num, Rep> Error for NumError<Num, Rep>
where
    Num: Debug,
    Rep: Debug,
{
}

//
// Common nums
//

#[derive(Clone, Debug)]
pub enum NonNegTag {}
pub type NonNeg<Rep> = Tagged<Rep, NonNegTag>;

impl<Rep> Refine<Rep> for NonNegTag
where
    Rep: Ord + Debug + Zero,
{
    type RefineError = NumError<NonNeg<Rep>, Rep>;
    fn refine(rep: Rep) -> Result<Rep, Self::RefineError> {
        if rep >= zero() {
            Ok(rep)
        } else {
            NumError::Underflow(rep).pipe(Err)
        }
    }
}

#[derive(Clone, Debug)]
pub enum PosTag {}
pub type Pos<Rep> = Tagged<Rep, PosTag>;

impl<Rep> Refine<Rep> for PosTag
where
    Rep: Ord + Debug + Zero,
{
    type RefineError = NumError<Pos<Rep>, Rep>;
    fn refine(rep: Rep) -> Result<Rep, Self::RefineError> {
        if rep > zero() {
            Ok(rep)
        } else {
            NumError::Underflow(rep).pipe(Err)
        }
    }
}

//
// Common math
//

impl<Rep, Tag, E> Tagged<Rep, Tag>
where
    Tag: Debug + Refine<Rep, RefineError = E>,
    Rep: Debug,
    E: Debug + From<NumError<Tagged<Rep, Tag>, Rep>>,
{
    pub fn zero() -> Result<Tagged<Rep, Tag>, E>
    where
        Rep: Zero,
    {
        Rep::zero().pipe(Tagged::new)
    }

    pub fn one() -> Result<Tagged<Rep, Tag>, E>
    where
        Rep: One,
    {
        Rep::one().pipe(Tagged::new)
    }

    pub fn add(
        &self,
        rhs: &Tagged<Rep, Tag>,
    ) -> Result<Tagged<Rep, Tag>, E>
    where
        Rep: Copy + CheckedAdd,
    {
        self.rep()
            .checked_add(rhs.rep())
            .ok_or(NumError::Add(*self, *rhs))?
            .pipe(Tagged::new)
    }

    pub fn sub(
        &self,
        rhs: &Tagged<Rep, Tag>,
    ) -> Result<Tagged<Rep, Tag>, E>
    where
        Rep: Copy + CheckedSub,
    {
        self.rep()
            .checked_sub(rhs.rep())
            .ok_or(NumError::Sub(*self, *rhs))?
            .pipe(Tagged::new)
    }

    pub fn gap(
        &self,
        rhs: &Tagged<Rep, Tag>,
    ) -> Result<Tagged<Rep, Tag>, E>
    where
        Rep: Ord + Copy + CheckedSub,
    {
        self.max(rhs).sub(self.min(rhs))
    }

    pub fn div(
        &self,
        rhs: &Tagged<Rep, Tag>,
    ) -> Result<Rep, E>
    where
        Rep: Copy + CheckedDiv,
    {
        self.rep()
            .checked_div(rhs.rep())
            .ok_or(NumError::Div(*self, *rhs))?
            .pipe(Ok)
    }

    pub fn mul_rep(
        &self,
        rhs: &Rep,
    ) -> Result<Tagged<Rep, Tag>, E>
    where
        Rep: Copy + CheckedMul,
    {
        self.rep()
            .checked_mul(rhs)
            .ok_or(NumError::MulRep(*self, *rhs))?
            .pipe(Tagged::new)
    }

    pub fn div_rep(
        &self,
        rhs: &Rep,
    ) -> Result<Tagged<Rep, Tag>, E>
    where
        Rep: Copy + CheckedDiv,
    {
        self.rep()
            .checked_div(rhs)
            .ok_or(NumError::DivRep(*self, *rhs))?
            .pipe(Tagged::new)
    }
}
