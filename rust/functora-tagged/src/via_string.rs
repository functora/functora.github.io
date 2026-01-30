use crate::parse_error::*;
use crate::refine::*;
pub use std::fmt::{Debug, Display};
pub use std::hash::{Hash, Hasher};
pub use std::marker::PhantomData;
pub use std::ops::Deref;
pub use std::str::FromStr;

#[derive(Debug)]
pub struct ViaString<Rep, Tag>(Rep, PhantomData<Tag>);

impl<Rep, Tag> ViaString<Rep, Tag> {
    pub fn new(prev: Rep) -> Result<Self, Tag::RefineError>
    where
        Tag: Refine<Rep>,
    {
        Tag::refine(prev)
            .map(|next| ViaString(next, PhantomData))
    }
    pub fn rep(&self) -> &Rep {
        &self.0
    }
}

impl<Rep: Eq, Tag> Eq for ViaString<Rep, Tag> {}

impl<Rep: PartialEq, Tag> PartialEq
    for ViaString<Rep, Tag>
{
    fn eq(&self, other: &Self) -> bool {
        self.rep() == other.rep()
    }
}

impl<Rep: Ord, Tag> Ord for ViaString<Rep, Tag> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.rep().cmp(other.rep())
    }
}

impl<Rep: PartialOrd, Tag> PartialOrd
    for ViaString<Rep, Tag>
{
    fn partial_cmp(
        &self,
        other: &Self,
    ) -> Option<std::cmp::Ordering> {
        self.rep().partial_cmp(other.rep())
    }
}

impl<Rep: Clone, Tag> Clone for ViaString<Rep, Tag> {
    fn clone(&self) -> Self {
        ViaString(self.rep().clone(), PhantomData)
    }
}

impl<Rep: Copy, Tag> Copy for ViaString<Rep, Tag> {}

impl<Rep: Display, Tag> Display for ViaString<Rep, Tag> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        self.rep().fmt(f)
    }
}

impl<Rep: Hash, Tag> Hash for ViaString<Rep, Tag> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rep().hash(state);
    }
}

impl<Rep, Tag> Deref for ViaString<Rep, Tag> {
    type Target = Rep;

    fn deref(&self) -> &Self::Target {
        self.rep()
    }
}

impl<Rep, Tag> FromStr for ViaString<Rep, Tag>
where
    Rep: FromStr,
    Tag: Refine<Rep>,
{
    type Err = ParseError<Rep, Tag>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        ViaString::new(
            Rep::from_str(s).map_err(ParseError::Decode)?,
        )
        .map_err(ParseError::Refine)
    }
}

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "serde")]
impl<Rep: ToString, Tag> Serialize for ViaString<Rep, Tag> {
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
impl<'de, Rep, Tag> Deserialize<'de> for ViaString<Rep, Tag>
where
    Rep: FromStr,
    Rep::Err: Display,
    Tag: Refine<Rep>,
    Tag::RefineError: Display,
{
    fn deserialize<D>(
        deserializer: D,
    ) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
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

    impl<Rep, Tag, ST> AsExpression<ST> for ViaString<Rep, Tag>
    where
        Rep: ToString,
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

    impl<Rep, Tag, ST> AsExpression<ST> for &ViaString<Rep, Tag>
    where
        Rep: ToString,
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

    impl<DB, Rep, Tag, ST> ToSql<ST, DB> for ViaString<Rep, Tag>
    where
        Rep: ToString + Debug,
        Tag: Debug,
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

    impl<DB, Rep, Tag, ST> FromSql<ST, DB>
        for ViaString<Rep, Tag>
    where
        Rep: FromStr,
        Rep::Err: 'static + Error + Send + Sync,
        Tag: Refine<Rep>,
        Tag::RefineError: 'static + Error + Send + Sync,
        String: FromSql<ST, DB>,
        DB: Backend,
    {
        fn from_sql(
            bytes: DB::RawValue<'_>,
        ) -> diesel::deserialize::Result<Self> {
            let s = String::from_sql(bytes)?;
            let rep = Rep::from_str(&s)?;
            Ok(ViaString::new(rep).map_err(Box::new)?)
        }
    }

    impl<Rep, Tag, ST, DB> Queryable<ST, DB>
        for ViaString<Rep, Tag>
    where
        Rep: FromStr,
        Rep::Err: 'static + Error + Send + Sync,
        Tag: Refine<Rep>,
        Tag::RefineError: 'static + Error + Send + Sync,
        String: Queryable<ST, DB>,
        DB: Backend,
    {
        type Row = <String as Queryable<ST, DB>>::Row;
        fn build(
            row: Self::Row,
        ) -> diesel::deserialize::Result<Self> {
            let s: String =
                Queryable::<ST, DB>::build(row)?;
            let rep =
                Rep::from_str(&s).map_err(Box::new)?;
            Ok(ViaString::new(rep).map_err(Box::new)?)
        }
    }
}
