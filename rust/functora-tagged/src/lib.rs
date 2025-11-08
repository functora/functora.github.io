#![doc = include_str!("../README.md")]
use derive_more::Display;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;

#[derive(Debug)]
pub struct Tagged<Rep, Tag>(Rep, PhantomData<Tag>);

pub trait Refine<Rep>: Sized {
    type RefineError;

    fn refine(rep: Rep) -> Result<Rep, Self::RefineError> {
        Ok(rep)
    }
}

impl<Rep, Tag> Tagged<Rep, Tag> {
    pub fn new(rep: Rep) -> Result<Self, Tag::RefineError>
    where
        Tag: Refine<Rep>,
    {
        Tag::refine(rep).map(|rep| Tagged(rep, PhantomData))
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

impl<Rep: Display, Tag> Display for Tagged<Rep, Tag> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        self.rep().fmt(f)
    }
}

impl<Rep: Hash, Tag> Hash for Tagged<Rep, Tag> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rep().hash(state);
    }
}

impl<Rep, Tag> Deref for Tagged<Rep, Tag> {
    type Target = Rep;

    fn deref(&self) -> &Self::Target {
        self.rep()
    }
}

#[derive(Debug, Display)]
pub enum ParseError<Rep, Tag>
where
    Rep: FromStr,
    Tag: Refine<Rep>,
{
    Decode(Rep::Err),
    Refine(Tag::RefineError),
}

impl<Rep, Tag> Error for ParseError<Rep, Tag>
where
    Rep: Debug + FromStr,
    Tag: Debug + Refine<Rep>,
    Rep::Err: Debug + Display,
    Tag::RefineError: Debug + Display,
{
}

impl<Rep, Tag> Eq for ParseError<Rep, Tag>
where
    Rep: FromStr,
    Tag: Refine<Rep>,
    Rep::Err: Eq,
    Tag::RefineError: Eq,
{
}

impl<Rep, Tag> PartialEq for ParseError<Rep, Tag>
where
    Rep: FromStr,
    Tag: Refine<Rep>,
    Rep::Err: PartialEq,
    Tag::RefineError: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                ParseError::Decode(a),
                ParseError::Decode(b),
            ) => a == b,
            (
                ParseError::Refine(a),
                ParseError::Refine(b),
            ) => a == b,
            _ => false,
        }
    }
}

impl<Rep, Tag> Clone for ParseError<Rep, Tag>
where
    Rep: FromStr,
    Tag: Refine<Rep>,
    Rep::Err: Clone,
    Tag::RefineError: Clone,
{
    fn clone(&self) -> Self {
        match self {
            ParseError::Decode(err) => {
                ParseError::Decode(err.clone())
            }
            ParseError::Refine(err) => {
                ParseError::Refine(err.clone())
            }
        }
    }
}

impl<Rep, Tag> FromStr for Tagged<Rep, Tag>
where
    Rep: FromStr,
    Tag: Refine<Rep>,
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
    Rep: Deserialize<'de>,
    Tag: Refine<Rep>,
    Tag::RefineError: Display,
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
    use std::error::Error;

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
        Rep: FromSql<ST, DB>,
        Tag: Refine<Rep>,
        Tag::RefineError: 'static + Error + Send + Sync,
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
        Rep: Queryable<ST, DB>,
        Tag: Refine<Rep>,
        Tag::RefineError: 'static + Error + Send + Sync,
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
