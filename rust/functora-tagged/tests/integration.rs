use functora_tagged::{
    ParseError, Refine, RefineError, Tagged,
};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum NonEmptyTag {}

pub type NonEmpty<T> = Tagged<T, NonEmptyTag>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UserIdTag {}

pub type UserId = Tagged<NonEmpty<String>, UserIdTag>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum EmailTag {}

pub type Email = Tagged<NonEmpty<String>, EmailTag>;

#[derive(Debug, Error)]
#[error("Empty value is not allowed")]
pub struct EmptyError;

impl Refine<NonEmptyTag> for String {
    type RefineErrorRep = EmptyError;
    fn refine(self) -> Result<Self, Self::RefineErrorRep> {
        if self.is_empty() {
            Err(EmptyError)
        } else {
            Ok(self)
        }
    }
}

#[derive(Debug, Error)]
#[error("Invalid UserId format")]
pub struct UserIdError;

impl Refine<UserIdTag> for NonEmpty<String> {
    type RefineErrorRep = UserIdError;
    fn refine(self) -> Result<Self, Self::RefineErrorRep> {
        let txt = self.rep();
        if txt.starts_with("user_") && txt.len() > 5 {
            Ok(self)
        } else {
            Err(UserIdError)
        }
    }
}

#[derive(Debug, Error)]
#[error("string too short: {0}, minimum length 3")]
pub struct EmailError(usize);

impl Refine<EmailTag> for NonEmpty<String> {
    type RefineErrorRep = EmailError;
    fn refine(self) -> Result<Self, Self::RefineErrorRep> {
        let len = self.clone().rep().len();
        if len < 3 {
            Err(EmailError(len))
        } else {
            Ok(self)
        }
    }
}

#[test]
fn test_non_empty_from_str_success() {
    let ne: NonEmpty<String> = "hello".parse().unwrap();
    assert_eq!(ne.rep(), "hello");
}

#[test]
fn test_non_empty_from_str_refine_error() {
    let err: Result<NonEmpty<String>, _> = "".parse();
    assert!(matches!(err, Err(ParseError::Refine(_))));
}

#[test]
fn test_user_id_success() {
    let inner =
        "user_123".parse::<NonEmpty<String>>().unwrap();
    let uid = UserId::new(inner).unwrap();
    assert_eq!(uid.rep().rep(), "user_123");
}

#[test]
fn test_user_id_refine_failure() {
    let inner =
        "invalid".parse::<NonEmpty<String>>().unwrap();
    let err = UserId::new(inner).unwrap_err();
    assert!(matches!(err, RefineError(_)));
}

#[test]
fn test_user_id_from_str_success() {
    let uid: UserId = "user_123".parse().unwrap();
    assert_eq!(uid.rep().rep(), "user_123");
}

#[test]
fn test_user_id_from_str_refine_failure() {
    let err: Result<UserId, _> = "invalid".parse();
    assert!(matches!(err, Err(ParseError::Refine(_))));
}

#[test]
fn test_email_success() {
    let email: Email = "a@b.com".parse().unwrap();
    assert_eq!(email.rep().rep(), "a@b.com");
}

#[test]
fn test_email_refine_failure() {
    let inner = "ab".parse::<NonEmpty<String>>().unwrap();
    let err = Email::new(inner).unwrap_err();
    assert!(matches!(err, RefineError(_)));
}

#[test]
fn test_email_from_str_refine_failure() {
    let err: Result<Email, _> = "ab".parse();
    assert!(matches!(err, Err(ParseError::Refine(_))));
}

#[test]
fn test_tagged_eq_ord() {
    let a: NonEmpty<String> = "abc".parse().unwrap();
    let b: NonEmpty<String> = "abc".parse().unwrap();
    let c: NonEmpty<String> = "def".parse().unwrap();
    assert_eq!(a, b);
    assert!(a < c);
}

#[test]
fn test_tagged_clone_debug() {
    let tagged: NonEmpty<String> = "test".parse().unwrap();
    let cloned = tagged.clone();
    assert_eq!(tagged, cloned);
    let dbg = format!("{:?}", tagged);
    assert!(dbg.contains("Tagged"));
    assert!(dbg.contains("PhantomData"));
}

#[cfg(feature = "serde")]
#[test]
fn test_serde_user_id_roundtrip() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Wrapper {
        user_id: UserId,
    }
    let original = Wrapper {
        user_id: "user_456".parse().unwrap(),
    };
    let toml = toml::to_string(&original).unwrap();
    let deserialized: Wrapper =
        toml::from_str(&toml).unwrap();
    assert_eq!(original, deserialized);
    assert_eq!(
        deserialized.user_id.rep().rep(),
        "user_456"
    );
}

#[cfg(feature = "serde")]
#[test]
fn test_serde_user_id_invalid_refine() {
    #[derive(Deserialize, Debug)]
    struct Wrapper {
        user_id: UserId,
    }
    let toml = r#"user_id = "bad""#;
    let err = toml::from_str::<Wrapper>(toml).unwrap_err();
    assert!(
        err.to_string().contains("Refine failed"),
        "Unexpected failure: {err}"
    );
    let toml = r#"user_id = "user_123""#;
    let wrapper: Wrapper = toml::from_str(toml).unwrap();
    assert_eq!(wrapper.user_id.rep().rep(), "user_123");
}

#[cfg(feature = "serde")]
#[test]
fn test_serde_email_roundtrip() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Wrapper {
        email: Email,
    }
    let original = Wrapper {
        email: "hello@example.com".parse().unwrap(),
    };
    let toml = toml::to_string(&original).unwrap();
    let deserialized: Wrapper =
        toml::from_str(&toml).unwrap();
    assert_eq!(original, deserialized);
    assert_eq!(
        deserialized.email.rep().rep(),
        "hello@example.com"
    );
}

#[cfg(feature = "diesel")]
mod diesel_tests {
    use super::*;
    use diesel::insert_into;
    use diesel::prelude::*;
    use diesel::sql_query;
    use diesel::sql_types::Text;
    use diesel::sqlite::SqliteConnection;

    table! {
        users (id) {
            id -> Text,
            email -> Text,
        }
    }

    #[derive(QueryableByName, PartialEq, Debug)]
    struct UserRow {
        #[diesel(sql_type = Text)]
        id: UserId,
        #[diesel(sql_type = Text)]
        email: Email,
    }

    fn memory_db() -> SqliteConnection {
        let mut conn =
            SqliteConnection::establish(":memory:")
                .unwrap_or_else(|_| {
                    panic!("cannot create in-memory DB")
                });
        sql_query(
            "CREATE TABLE users (id TEXT NOT NULL, email TEXT NOT NULL);",
        )
        .execute(&mut conn)
        .expect("failed to create table");
        conn
    }

    #[test]
    fn test_diesel_queryable_success() {
        let mut conn = memory_db();
        insert_into(users::table)
            .values((
                users::id.eq("user_789"),
                users::email.eq("hello@example.com"),
            ))
            .execute(&mut conn)
            .unwrap();
        let rows: Vec<UserRow> =
            sql_query("SELECT id, email FROM users")
                .load(&mut conn)
                .unwrap();
        assert_eq!(rows.len(), 1);
        assert_eq!(
            rows[0].id.clone().rep().rep(),
            "user_789"
        );
        assert_eq!(
            rows[0].email.clone().rep().rep(),
            "hello@example.com"
        );
    }

    #[test]
    fn test_diesel_queryable_refine_failure() {
        let mut conn = memory_db();
        insert_into(users::table)
            .values((
                users::id.eq("bad_id"),
                users::email.eq("ab"),
            ))
            .execute(&mut conn)
            .unwrap();
        let err = sql_query("SELECT id, email FROM users")
            .load::<UserRow>(&mut conn)
            .unwrap_err();
        assert!(err.to_string().contains("Refine failed"));
    }

    #[test]
    fn test_diesel_to_sql() {
        let mut conn = memory_db();
        let uid: UserId = "user_999".parse().unwrap();
        let email: Email =
            "test@domain.com".parse().unwrap();
        insert_into(users::table)
            .values((
                users::id.eq(&uid),
                users::email.eq(&email),
            ))
            .execute(&mut conn)
            .unwrap();
        let rows: Vec<(String, String)> = users::table
            .select((users::id, users::email))
            .load(&mut conn)
            .unwrap();
        assert_eq!(rows[0].0, "user_999");
        assert_eq!(rows[0].1, "test@domain.com");
    }
}
