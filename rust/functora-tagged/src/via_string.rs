use crate::parse_error::*;
use crate::refine::*;
pub use std::fmt::{Debug, Display};
pub use std::hash::{Hash, Hasher};
pub use std::marker::PhantomData;
pub use std::ops::Deref;
pub use std::str::FromStr;

#[derive(Debug)]
pub struct ViaString<T, D, F>(T, PhantomData<(D, F)>);

impl<T, D, F> ViaString<T, D, F> {
    pub fn new(rep: T) -> Result<Self, F::RefineError>
    where
        F: Refine<T>,
    {
        F::refine(rep)
            .map(|next| ViaString(next, PhantomData))
    }
    pub fn rep(&self) -> &T {
        &self.0
    }
}

impl<T: Eq, D, F> Eq for ViaString<T, D, F> {}

impl<T: PartialEq, D, F> PartialEq for ViaString<T, D, F> {
    fn eq(&self, other: &Self) -> bool {
        self.rep() == other.rep()
    }
}

impl<T: Ord, D, F> Ord for ViaString<T, D, F> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.rep().cmp(other.rep())
    }
}

impl<T: PartialOrd, D, F> PartialOrd
    for ViaString<T, D, F>
{
    fn partial_cmp(
        &self,
        other: &Self,
    ) -> Option<std::cmp::Ordering> {
        self.rep().partial_cmp(other.rep())
    }
}

impl<T: Clone, D, F> Clone for ViaString<T, D, F> {
    fn clone(&self) -> Self {
        ViaString(self.rep().clone(), PhantomData)
    }
}

impl<T: Copy, D, F> Copy for ViaString<T, D, F> {}

impl<T: Display, D, F> Display for ViaString<T, D, F> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        self.rep().fmt(f)
    }
}

impl<T: Hash, D, F> Hash for ViaString<T, D, F> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rep().hash(state);
    }
}

impl<T, D, F> Deref for ViaString<T, D, F> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.rep()
    }
}

impl<T, D, F> FromStr for ViaString<T, D, F>
where
    T: Debug + FromStr,
    D: Debug,
    F: Debug + Refine<T>,
    T::Err: Debug,
    F::RefineError: Debug,
{
    type Err = ParseError<T, D, F>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        ViaString::new(T::from_str(s).map_err(|e| {
            ParseError::Decode(e, PhantomData)
        })?)
        .map_err(|e| ParseError::Refine(e, PhantomData))
    }
}

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "serde")]
impl<T: ToString, D, F> Serialize for ViaString<T, D, F> {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de, T, D, F> Deserialize<'de> for ViaString<T, D, F>
where
    T: Debug + FromStr,
    D: Debug,
    F: Debug + Refine<T>,
    T::Err: Debug + Display,
    F::RefineError: Debug + Display,
{
    fn deserialize<DE>(
        deserializer: DE,
    ) -> Result<Self, DE::Error>
    where
        DE: serde::Deserializer<'de>,
    {
        String::deserialize(deserializer).and_then(|s| {
            ViaString::from_str(&s)
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
    use diesel::query_builder::bind_collector::RawBytesBindCollector;
    use diesel::serialize::{Output, ToSql};
    use diesel::sql_types::SingleValue;
    use std::error::Error;

    impl<T, D, F, ST> AsExpression<ST> for ViaString<T, D, F>
    where
        T: ToString,
        ST: SingleValue,
        String: AsExpression<ST>,
    {
        type Expression =
            <String as AsExpression<ST>>::Expression;
        fn as_expression(self) -> Self::Expression {
            <String as AsExpression<ST>>::as_expression(
                self.to_string(),
            )
        }
    }

    impl<T, D, F, ST> AsExpression<ST> for &ViaString<T, D, F>
    where
        T: ToString,
        ST: SingleValue,
        String: AsExpression<ST>,
    {
        type Expression =
            <String as AsExpression<ST>>::Expression;
        fn as_expression(self) -> Self::Expression {
            <String as AsExpression<ST>>::as_expression(
                self.to_string(),
            )
        }
    }

    impl<T, D, F, ST, DB> ToSql<ST, DB> for ViaString<T, D, F>
    where
        T: ToString + Debug,
        D: Debug,
        F: Debug,
        String: ToSql<ST, DB>,
        for<'a> DB: Backend<
            BindCollector<'a> = RawBytesBindCollector<DB>,
        >,
    {
        fn to_sql<'a>(
            &'a self,
            out: &mut Output<'a, '_, DB>,
        ) -> diesel::serialize::Result {
            let s = self.to_string();
            <String as ToSql<ST, DB>>::to_sql(
                &s,
                &mut out.reborrow(),
            )
        }
    }

    impl<T, D, F, ST, DB> FromSql<ST, DB> for ViaString<T, D, F>
    where
        T: FromStr,
        T::Err: 'static + Error + Send + Sync,
        F: Refine<T>,
        F::RefineError: 'static + Error + Send + Sync,
        String: FromSql<ST, DB>,
        DB: Backend,
    {
        fn from_sql(
            bytes: DB::RawValue<'_>,
        ) -> diesel::deserialize::Result<Self> {
            let s = String::from_sql(bytes)?;
            let rep = T::from_str(&s)?;
            Ok(ViaString::new(rep).map_err(Box::new)?)
        }
    }

    impl<T, D, F, ST, DB> Queryable<ST, DB>
        for ViaString<T, D, F>
    where
        T: FromStr,
        T::Err: 'static + Error + Send + Sync,
        F: Refine<T>,
        F::RefineError: 'static + Error + Send + Sync,
        String: Queryable<ST, DB>,
        DB: Backend,
    {
        type Row = <String as Queryable<ST, DB>>::Row;
        fn build(
            row: Self::Row,
        ) -> diesel::deserialize::Result<Self> {
            let s: String =
                Queryable::<ST, DB>::build(row)?;
            let rep = T::from_str(&s).map_err(Box::new)?;
            Ok(ViaString::new(rep).map_err(Box::new)?)
        }
    }
}
