use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::str::FromStr;
use thiserror::Error;

#[derive(
    Eq, PartialEq, Ord, PartialOrd, Clone, Debug, Serialize,
)]
pub struct Tagged<Rep, Tag>(Rep, PhantomData<Tag>)
where
    Rep: Refine<Tag>;

pub trait Refine<Tag>:
    Eq
    + PartialEq
    + Ord
    + PartialOrd
    + Clone
    + Debug
    + Serialize
    + FromStr
{
    type DecodeErr: Debug
        + Display
        + From<<Self as FromStr>::Err>;

    fn decode(txt: &str) -> Result<Self, Self::DecodeErr> {
        Self::from_str(txt).map_err(Self::DecodeErr::from)
    }

    type RefineErr: Debug + Display;

    fn refine(rep: Self) -> Result<Self, Self::RefineErr> {
        Ok(rep)
    }
}

#[derive(Debug, Error)]
pub enum Error<Rep, Tag>
where
    Rep: Refine<Tag>,
{
    #[error("Tagged Decode error: {0}")]
    Decode(Rep::DecodeErr),
    #[error("Tagged Refine error: {0}")]
    Refine(Rep::RefineErr),
}

impl<Rep, Tag> Tagged<Rep, Tag>
where
    Rep: Refine<Tag>,
{
    pub fn new(rep: Rep) -> Result<Self, Error<Rep, Tag>> {
        Rep::refine(rep)
            .map(|rep| Tagged(rep, PhantomData))
            .map_err(Error::Refine)
    }

    pub fn rep(self) -> Rep {
        self.0
    }
}

impl<Rep, Tag> FromStr for Tagged<Rep, Tag>
where
    Rep: Refine<Tag>,
{
    type Err = Error<Rep, Tag>;

    fn from_str(txt: &str) -> Result<Self, Self::Err> {
        Rep::decode(txt)
            .map_err(Error::Decode)
            .and_then(Tagged::new)
    }
}

impl<'a, Rep, Tag> Deserialize<'a> for Tagged<Rep, Tag>
where
    Rep: Refine<Tag>,
{
    fn deserialize<D>(
        deserializer: D,
    ) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        Tagged::<Rep, Tag>::from_str(&String::deserialize(
            deserializer,
        )?)
        .map_err(serde::de::Error::custom)
    }
}

#[macro_export]
macro_rules! lit {
    ($arg:expr) => {{
        $crate::tagged::Tagged::from_str(stringify!($arg))
            .unwrap_or_else(|e| {
                panic!(
                    "lit!({}) failed: {}",
                    stringify!($arg),
                    e
                )
            })
    }};
}

//
// Diesel
//

use diesel::Queryable;
use diesel::backend::Backend;
use diesel::deserialize::FromSql;
use diesel::expression::AsExpression;
use diesel::serialize::{Output, ToSql};
use diesel::sql_types::{SingleValue, SqlType};

impl<Rep, Tag, ST> AsExpression<ST> for Tagged<Rep, Tag>
where
    Rep: Refine<Tag> + AsExpression<ST>,
    ST: SqlType + SingleValue,
{
    type Expression = <Rep as AsExpression<ST>>::Expression;

    fn as_expression(self) -> Self::Expression {
        self.0.clone().as_expression()
    }
}

impl<Rep, Tag, ST> AsExpression<ST> for &Tagged<Rep, Tag>
where
    Rep: Refine<Tag> + AsExpression<ST>,
    ST: SqlType + SingleValue,
{
    type Expression = <Rep as AsExpression<ST>>::Expression;

    fn as_expression(self) -> Self::Expression {
        self.0.clone().as_expression()
    }
}

impl<DB, Rep, Tag, ST> ToSql<ST, DB> for Tagged<Rep, Tag>
where
    Rep: Refine<Tag> + ToSql<ST, DB>,
    ST: SqlType + SingleValue,
    DB: Backend,
    Tag: Debug,
{
    fn to_sql<'a>(
        &'a self,
        out: &mut Output<'a, '_, DB>,
    ) -> diesel::serialize::Result {
        self.0.to_sql(out)
    }
}

impl<DB, Rep, Tag, ST> FromSql<ST, DB> for Tagged<Rep, Tag>
where
    Rep: Refine<Tag> + FromSql<ST, DB>,
    ST: SqlType + SingleValue,
    DB: Backend,
{
    fn from_sql(
        bytes: DB::RawValue<'_>,
    ) -> diesel::deserialize::Result<Self> {
        let rep = Rep::from_sql(bytes)?;
        Tagged::new(rep).map_err(|e| {
            format!("Tagged decode/refine failed: {e}")
                .into()
        })
    }
}

impl<Rep, Tag, ST, DB> Queryable<ST, DB>
    for Tagged<Rep, Tag>
where
    Rep: Refine<Tag> + FromSql<ST, DB> + Queryable<ST, DB>,
    ST: SqlType + SingleValue,
    DB: Backend,
{
    type Row = <Rep as Queryable<ST, DB>>::Row;

    fn build(
        row: Self::Row,
    ) -> diesel::deserialize::Result<Self> {
        let rep = <Rep as Queryable<ST, DB>>::build(row)?;
        Tagged::new(rep).map_err(|e| {
            format!("Tagged decode/refine failed: {e}")
                .into()
        })
    }
}
