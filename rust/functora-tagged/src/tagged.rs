use crate::parse_error::*;
use crate::refine::*;
pub use std::fmt::{Debug, Display};
pub use std::hash::{Hash, Hasher};
pub use std::marker::PhantomData;
pub use std::ops::Deref;
pub use std::str::FromStr;

#[derive(Debug)]
pub struct Tagged<T, D, F>(T, PhantomData<(D, F)>);

impl<T, D, F> Tagged<T, D, F> {
    pub fn new(rep: T) -> Result<Self, F::RefineError>
    where
        F: Refine<T>,
    {
        F::refine(rep).map(|next| Tagged(next, PhantomData))
    }
    pub fn rep(&self) -> &T {
        &self.0
    }
    pub fn untag(self) -> T {
        self.0
    }
}

impl<T: Eq, D, F> Eq for Tagged<T, D, F> {}

impl<T: PartialEq, D, F> PartialEq for Tagged<T, D, F> {
    fn eq(&self, other: &Self) -> bool {
        self.rep() == other.rep()
    }
}

impl<T: Ord, D, F> Ord for Tagged<T, D, F> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.rep().cmp(other.rep())
    }
}

impl<T: PartialOrd, D, F> PartialOrd for Tagged<T, D, F> {
    fn partial_cmp(
        &self,
        other: &Self,
    ) -> Option<std::cmp::Ordering> {
        self.rep().partial_cmp(other.rep())
    }
}

impl<T: Clone, D, F> Clone for Tagged<T, D, F> {
    fn clone(&self) -> Self {
        Tagged(self.rep().clone(), PhantomData)
    }
}

impl<T: Copy, D, F> Copy for Tagged<T, D, F> {}

impl<T: Display, D, F> Display for Tagged<T, D, F> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        self.rep().fmt(f)
    }
}

impl<T: Hash, D, F> Hash for Tagged<T, D, F> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rep().hash(state);
    }
}

impl<T, D, F> Deref for Tagged<T, D, F> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.rep()
    }
}

impl<T, D, F> FromStr for Tagged<T, D, F>
where
    T: FromStr,
    F: Refine<T>,
{
    type Err = ParseError<T, F>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Tagged::new(
            T::from_str(s).map_err(ParseError::Decode)?,
        )
        .map_err(ParseError::Refine)
    }
}

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "serde")]
impl<T: Serialize, D, F> Serialize for Tagged<T, D, F> {
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
impl<'de, T, D, F> Deserialize<'de> for Tagged<T, D, F>
where
    T: Deserialize<'de>,
    F: Refine<T>,
    F::RefineError: Display,
{
    fn deserialize<DE>(
        deserializer: DE,
    ) -> Result<Self, DE::Error>
    where
        DE: serde::Deserializer<'de>,
    {
        T::deserialize(deserializer).and_then(|rep| {
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

    impl<T, D, F, ST> AsExpression<ST> for Tagged<T, D, F>
    where
        T: Clone + AsExpression<ST>,
        ST: SingleValue,
    {
        type Expression = T::Expression;
        fn as_expression(self) -> Self::Expression {
            self.rep().clone().as_expression()
        }
    }

    impl<T, D, F, ST> AsExpression<ST> for &Tagged<T, D, F>
    where
        T: Clone + AsExpression<ST>,
        ST: SingleValue,
    {
        type Expression = T::Expression;
        fn as_expression(self) -> Self::Expression {
            self.rep().clone().as_expression()
        }
    }

    impl<T, D, F, ST, DB> ToSql<ST, DB> for Tagged<T, D, F>
    where
        T: ToSql<ST, DB>,
        D: Debug,
        F: Debug,
        DB: Backend,
    {
        fn to_sql<'a>(
            &'a self,
            out: &mut Output<'a, '_, DB>,
        ) -> diesel::serialize::Result {
            self.rep().to_sql(out)
        }
    }

    impl<T, D, F, ST, DB> FromSql<ST, DB> for Tagged<T, D, F>
    where
        T: FromSql<ST, DB>,
        F: Refine<T>,
        F::RefineError: 'static + Error + Send + Sync,
        DB: Backend,
    {
        fn from_sql(
            bytes: DB::RawValue<'_>,
        ) -> diesel::deserialize::Result<Self> {
            let rep = T::from_sql(bytes)?;
            Ok(Tagged::new(rep).map_err(Box::new)?)
        }
    }

    impl<T, D, F, ST, DB> Queryable<ST, DB> for Tagged<T, D, F>
    where
        T: Queryable<ST, DB>,
        F: Refine<T>,
        F::RefineError: 'static + Error + Send + Sync,
        DB: Backend,
    {
        type Row = T::Row;
        fn build(
            row: Self::Row,
        ) -> diesel::deserialize::Result<Self> {
            let rep = Queryable::build(row)?;
            Ok(Tagged::new(rep).map_err(Box::new)?)
        }
    }
}
