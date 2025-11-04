use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::str::FromStr;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct Tagged<Rep, Tag>(Rep, PhantomData<Tag>)
where
    Rep: Refine<Tag>;

pub trait Refine<Tag>: Clone + Sized {
    type RefineErrorRep: Debug + Display;
    fn refine(self) -> Result<Self, Self::RefineErrorRep> {
        Ok(self)
    }
}

#[derive(Debug, Error)]
#[error("Refine error: {0}")]
pub struct RefineError<Rep, Tag>(pub Rep::RefineErrorRep)
where
    Rep: Refine<Tag>;

impl<Rep, Tag> Tagged<Rep, Tag>
where
    Rep: Refine<Tag>,
{
    pub fn new(
        rep: Rep,
    ) -> Result<Self, RefineError<Rep, Tag>> {
        rep.refine()
            .map(|rep| Tagged(rep, PhantomData))
            .map_err(RefineError)
    }
    pub fn rep(&self) -> &Rep {
        &self.0
    }
}

impl<Rep, Tag> PartialEq for Tagged<Rep, Tag>
where
    Rep: Refine<Tag> + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.rep() == other.rep()
    }
}

impl<Rep, Tag> Eq for Tagged<Rep, Tag> where
    Rep: Refine<Tag> + Eq
{
}

impl<Rep, Tag> PartialOrd for Tagged<Rep, Tag>
where
    Rep: Refine<Tag> + PartialOrd,
{
    fn partial_cmp(
        &self,
        other: &Self,
    ) -> Option<std::cmp::Ordering> {
        self.rep().partial_cmp(other.rep())
    }
}

impl<Rep, Tag> Ord for Tagged<Rep, Tag>
where
    Rep: Refine<Tag> + Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.rep().cmp(other.rep())
    }
}

#[derive(Debug, Error)]
pub enum ParseError<Rep, Tag>
where
    Rep: FromStr + Refine<Tag>,
{
    #[error("Decode failed: {0}")]
    Decode(Rep::Err),
    #[error("Refine failed: {0}")]
    Refine(RefineError<Rep, Tag>),
}

impl<Rep, Tag> FromStr for Tagged<Rep, Tag>
where
    Rep: FromStr + Refine<Tag>,
{
    type Err = ParseError<Rep, Tag>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Tagged::new(
            Rep::from_str(s).map_err(ParseError::Decode)?,
        )
        .map_err(ParseError::Refine)
    }
}

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "serde")]
impl<Rep, Tag> Serialize for Tagged<Rep, Tag>
where
    Rep: Serialize + Refine<Tag>,
{
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.rep().serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de, Rep, Tag> Deserialize<'de> for Tagged<Rep, Tag>
where
    Rep: FromStr + Refine<Tag>,
    Rep::Err: Debug + Display,
{
    fn deserialize<D>(
        deserializer: D,
    ) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Tagged::from_str(&s)
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(feature = "diesel")]
mod diesel_impl {
    use crate::*;
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
        type Expression =
            <Rep as AsExpression<ST>>::Expression;
        fn as_expression(self) -> Self::Expression {
            self.rep().clone().as_expression()
        }
    }

    impl<Rep, Tag, ST> AsExpression<ST> for &Tagged<Rep, Tag>
    where
        Rep: Refine<Tag> + AsExpression<ST>,
        ST: SqlType + SingleValue,
    {
        type Expression =
            <Rep as AsExpression<ST>>::Expression;
        fn as_expression(self) -> Self::Expression {
            self.rep().clone().as_expression()
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
            self.rep().to_sql(out)
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
                format!("Refine failed: {e}").into()
            })
        }
    }

    impl<Rep, Tag, ST, DB> Queryable<ST, DB>
        for Tagged<Rep, Tag>
    where
        Rep: Refine<Tag>
            + FromSql<ST, DB>
            + Queryable<ST, DB>,
        ST: SqlType + SingleValue,
        DB: Backend,
    {
        type Row = <Rep as Queryable<ST, DB>>::Row;
        fn build(
            row: Self::Row,
        ) -> diesel::deserialize::Result<Self> {
            let rep =
                <Rep as Queryable<ST, DB>>::build(row)?;
            Tagged::new(rep).map_err(|e| {
                format!("Refine failed: {e}").into()
            })
        }
    }
}
