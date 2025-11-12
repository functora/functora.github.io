use crate::*;
pub use derive_more::Display;
pub use std::error::Error;
pub use std::fmt::{Debug, Display};
pub use std::str::FromStr;

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
