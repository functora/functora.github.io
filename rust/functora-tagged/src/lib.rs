use std::error::Error;
use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::str::FromStr;
use thiserror::Error;

#[derive(Debug)]
pub struct Tagged<Rep, Tag>(Rep, PhantomData<Tag>);

pub trait Refine<Tag>: Sized {
    type RefineError;

    fn refine(self) -> Result<Self, Self::RefineError> {
        Ok(self)
    }
}

impl<Rep, Tag> Tagged<Rep, Tag> {
    pub fn new(rep: Rep) -> Result<Self, Rep::RefineError>
    where
        Rep: Refine<Tag>,
    {
        rep.refine().map(|rep| Tagged(rep, PhantomData))
    }
    pub fn rep(&self) -> &Rep {
        &self.0
    }
}

impl<Rep: Eq, Tag> Eq for Tagged<Rep, Tag> {}

impl<Rep: PartialEq, Tag> PartialEq for Tagged<Rep, Tag> {
    fn eq(&self, other: &Self) -> bool {
        self.rep() == other.rep()
    }
}

impl<Rep: Ord, Tag> Ord for Tagged<Rep, Tag> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.rep().cmp(other.rep())
    }
}

impl<Rep: PartialOrd, Tag> PartialOrd for Tagged<Rep, Tag> {
    fn partial_cmp(
        &self,
        other: &Self,
    ) -> Option<std::cmp::Ordering> {
        self.rep().partial_cmp(other.rep())
    }
}

impl<Rep: Clone, Tag> Clone for Tagged<Rep, Tag> {
    fn clone(&self) -> Self {
        Tagged(self.rep().clone(), PhantomData)
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
    Refine(Rep::RefineError),
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
impl<Rep: Serialize, Tag> Serialize for Tagged<Rep, Tag> {
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
    Rep: Deserialize<'de> + Refine<Tag>,
    Rep::RefineError: Display,
{
    fn deserialize<D>(
        deserializer: D,
    ) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Rep::deserialize(deserializer).and_then(|rep| {
            Tagged::new(rep)
                .map_err(serde::de::Error::custom)
        })
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
    use diesel::sql_types::SingleValue;

    impl<Rep, Tag, ST> AsExpression<ST> for Tagged<Rep, Tag>
    where
        Rep: Clone + AsExpression<ST>,
        ST: SingleValue,
    {
        type Expression = Rep::Expression;
        fn as_expression(self) -> Self::Expression {
            self.rep().clone().as_expression()
        }
    }

    impl<Rep, Tag, ST> AsExpression<ST> for &Tagged<Rep, Tag>
    where
        Rep: Clone + AsExpression<ST>,
        ST: SingleValue,
    {
        type Expression = Rep::Expression;
        fn as_expression(self) -> Self::Expression {
            self.rep().clone().as_expression()
        }
    }

    impl<DB, Rep, Tag, ST> ToSql<ST, DB> for Tagged<Rep, Tag>
    where
        Rep: ToSql<ST, DB>,
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
        Rep: FromSql<ST, DB> + Refine<Tag>,
        Rep::RefineError: 'static + Error + Send + Sync,
        DB: Backend,
    {
        fn from_sql(
            bytes: DB::RawValue<'_>,
        ) -> diesel::deserialize::Result<Self> {
            let rep = Rep::from_sql(bytes)?;
            Ok(Tagged::new(rep).map_err(Box::new)?)
        }
    }

    impl<Rep, Tag, ST, DB> Queryable<ST, DB>
        for Tagged<Rep, Tag>
    where
        Rep: Queryable<ST, DB> + Refine<Tag>,
        Rep::RefineError: 'static + Error + Send + Sync,
        DB: Backend,
    {
        type Row = Rep::Row;
        fn build(
            row: Self::Row,
        ) -> diesel::deserialize::Result<Self> {
            let rep = Queryable::build(row)?;
            Ok(Tagged::new(rep).map_err(Box::new)?)
        }
    }
}
