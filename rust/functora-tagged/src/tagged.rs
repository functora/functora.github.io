use crate::parse_error::*;
use crate::refine::*;
pub use std::fmt::{Debug, Display};
pub use std::hash::{Hash, Hasher};
pub use std::marker::PhantomData;
pub use std::ops::Deref;
pub use std::str::FromStr;

#[derive(Debug)]
pub struct Tagged<Rep, Tag>(Rep, PhantomData<Tag>);

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

impl<Rep: Copy, Tag> Copy for Tagged<Rep, Tag> {}

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
    use super::*;
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
