use derive_more::Display;
use functora_tagged::{
    InfallibleInto, ParseError, Refine, Tagged,
};
use std::collections::hash_map::DefaultHasher;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

fn ok<T, E>(result: Result<T, E>) -> T
where
    E: Debug,
{
    match result {
        Ok(value) => value,
        Err(error) => panic!("expected Ok, got: {error:?}"),
    }
}

fn err<T, E>(result: Result<T, E>) -> E
where
    T: Debug,
{
    match result {
        Err(error) => error,
        Ok(value) => panic!("expected Err, got: {value:?}"),
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Debug)]
pub enum NonEmptyTag {}
pub type NonEmpty<T> = Tagged<T, NonEmptyTag, NonEmptyTag>;

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Debug)]
pub enum UserIdTag {}
pub type UserId =
    Tagged<NonEmpty<String>, UserIdTag, UserIdTag>;

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Debug)]
pub enum EmailTag {}
pub type Email =
    Tagged<NonEmpty<String>, EmailTag, EmailTag>;

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Debug)]
pub enum UpperTag {}
pub type UpperString = Tagged<String, UpperTag, UpperTag>;

#[derive(
    Eq, PartialEq, Ord, PartialOrd, Clone, Debug, Display,
)]
pub struct NonEmptyError;
impl Error for NonEmptyError {}

impl Refine<String> for NonEmptyTag {
    type RefineError = NonEmptyError;
    fn refine(
        rep: String,
    ) -> Result<String, Self::RefineError> {
        if rep.is_empty() {
            Err(NonEmptyError)
        } else {
            Ok(rep)
        }
    }
}

#[derive(
    Eq, PartialEq, Ord, PartialOrd, Debug, Display,
)]
pub struct UserIdError;
impl Error for UserIdError {}

impl Refine<NonEmpty<String>> for UserIdTag {
    type RefineError = UserIdError;
    fn refine(
        rep: NonEmpty<String>,
    ) -> Result<NonEmpty<String>, Self::RefineError> {
        let txt = rep.rep();
        if txt.starts_with("user_") && txt.len() > 5 {
            Ok(rep)
        } else {
            Err(UserIdError)
        }
    }
}

#[derive(
    Eq, PartialEq, Ord, PartialOrd, Debug, Display,
)]
pub struct EmailError(usize);
impl Error for EmailError {}

impl Refine<NonEmpty<String>> for EmailTag {
    type RefineError = EmailError;
    fn refine(
        rep: NonEmpty<String>,
    ) -> Result<NonEmpty<String>, Self::RefineError> {
        let len = rep.clone().rep().len();
        if len < 3 {
            Err(EmailError(len))
        } else {
            Ok(rep)
        }
    }
}

impl Refine<String> for UpperTag {
    type RefineError = Infallible;
    fn refine(
        rep: String,
    ) -> Result<String, Self::RefineError> {
        Ok(rep.to_uppercase())
    }
}

#[test]
fn test_non_empty_from_str_success() {
    let ne: NonEmpty<String> = ok("hello".parse());
    assert_eq!(ne.rep(), "hello");
}

#[test]
fn test_non_empty_from_str_refine_error() {
    let err: Result<NonEmpty<String>, _> = "".parse();
    assert!(matches!(err, Err(ParseError::Refine(..))));
}

#[test]
fn test_user_id_success() {
    let inner = ok("user_123".parse::<NonEmpty<String>>());
    let uid = ok(UserId::new(inner));
    assert_eq!(uid.rep().rep(), "user_123");
}

#[test]
fn test_user_id_refine_failure() {
    let inner = ok("invalid".parse::<NonEmpty<String>>());
    let err = err(UserId::new(inner));
    assert_eq!(err, UserIdError);
}

#[test]
fn test_user_id_from_str_success() {
    let uid: UserId = ok("user_123".parse());
    assert_eq!(uid.rep().rep(), "user_123");
}

#[test]
fn test_user_id_from_str_refine_failure() {
    let err: Result<UserId, _> = "invalid".parse();
    assert!(matches!(err, Err(ParseError::Refine(..))));
}

#[test]
fn test_email_success() {
    let email: Email = ok("a@b.com".parse());
    assert_eq!(email.rep().rep(), "a@b.com");
}

#[test]
fn test_email_refine_failure() {
    let inner = ok("ab".parse::<NonEmpty<String>>());
    let err = err(Email::new(inner));
    assert_eq!(err, EmailError(2));
}

#[test]
fn test_email_from_str_refine_failure() {
    let err: Result<Email, _> = "ab".parse();
    assert!(matches!(err, Err(ParseError::Refine(..))));
}

#[test]
fn test_tagged_eq_ord() {
    let a: NonEmpty<String> = ok("abc".parse());
    let b: NonEmpty<String> = ok("abc".parse());
    let c: NonEmpty<String> = ok("def".parse());
    assert_eq!(a, b);
    assert!(a < c);
}

#[test]
fn test_tagged_clone_debug() {
    let tagged: NonEmpty<String> = ok("test".parse());
    let cloned = tagged.clone();
    assert_eq!(tagged, cloned);
    let dbg = format!("{tagged:?}");
    assert!(dbg.contains("Tagged"));
    assert!(dbg.contains("PhantomData"));
}

#[test]
fn test_upper_string_infallible() {
    let tagged: UpperString =
        UpperString::new("test".into()).infallible();
    assert_eq!(tagged.rep(), "TEST");
}

#[test]
fn test_tagged_display() {
    let tagged: NonEmpty<String> =
        ok("display_test".parse());
    let display_str = format!("{tagged}");
    assert_eq!(display_str, "display_test");
}

#[test]
fn test_tagged_hash() {
    let tagged1: NonEmpty<String> = ok("hash_test".parse());
    let tagged2: NonEmpty<String> = ok("hash_test".parse());
    let tagged3: NonEmpty<String> =
        ok("another_hash_test".parse());

    let mut hasher1 = DefaultHasher::new();
    tagged1.hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = DefaultHasher::new();
    tagged2.hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = DefaultHasher::new();
    tagged3.hash(&mut hasher3);
    let hash3 = hasher3.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
}

#[test]
fn test_tagged_deref() {
    let tagged: UpperString = UpperString::new(
        "deref_test".into(),
    )
    .expect(
        "This should not fail as Infallible cannot fail",
    );
    assert_eq!(tagged.to_uppercase(), "DEREF_TEST");
    assert_eq!(tagged.len(), 10);
}

#[cfg(feature = "serde")]
mod serde_tests {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[test]
    fn test_serde_user_id_roundtrip() {
        #[derive(
            Serialize, Deserialize, PartialEq, Debug,
        )]
        struct Wrapper {
            user_id: UserId,
        }
        let original = Wrapper {
            user_id: ok("user_456".parse()),
        };
        let toml = ok(toml::to_string(&original));
        let deserialized: Wrapper =
            ok(toml::from_str(&toml));
        assert_eq!(original, deserialized);
        assert_eq!(
            deserialized.user_id.rep().rep(),
            "user_456"
        );
    }

    #[test]
    fn test_serde_user_id_invalid_refine() {
        #[derive(Deserialize, Debug)]
        struct Wrapper {
            user_id: UserId,
        }
        let toml_invalid = r#"user_id = "bad""#;
        let err =
            err(toml::from_str::<Wrapper>(toml_invalid));
        assert!(
            err.to_string().contains("UserIdError"),
            "Unexpected failure: {err}"
        );
        let toml_valid = r#"user_id = "user_123""#;
        let wrapper: Wrapper =
            ok(toml::from_str(toml_valid));
        assert_eq!(wrapper.user_id.rep().rep(), "user_123");
    }

    #[test]
    fn test_serde_email_roundtrip() {
        #[derive(
            Serialize, Deserialize, PartialEq, Debug,
        )]
        struct Wrapper {
            email: Email,
        }
        let original = Wrapper {
            email: ok("hello@example.com".parse()),
        };
        let toml = ok(toml::to_string(&original));
        let deserialized: Wrapper =
            ok(toml::from_str(&toml));
        assert_eq!(original, deserialized);
        assert_eq!(
            deserialized.email.rep().rep(),
            "hello@example.com"
        );
    }
}

#[cfg(feature = "diesel")]
mod diesel_integration_tests {
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
        #[allow(clippy::unwrap_used)]
        let _ = sql_query(
            "CREATE TABLE users (id TEXT NOT NULL, email TEXT NOT NULL);",
        )
        .execute(&mut conn)
        .unwrap();
        conn
    }

    #[test]
    fn test_diesel_queryable_success() {
        let mut conn = memory_db();
        let _ = ok(insert_into(users::table)
            .values((
                users::id.eq("user_789"),
                users::email.eq("hello@example.com"),
            ))
            .execute(&mut conn));
        let rows: Vec<UserRow> =
            ok(sql_query("SELECT id, email FROM users")
                .load(&mut conn));
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
        let _ = ok(insert_into(users::table)
            .values((
                users::id.eq("bad_id"),
                users::email.eq("ab"),
            ))
            .execute(&mut conn));
        let err =
            err(sql_query("SELECT id, email FROM users")
                .load::<UserRow>(&mut conn));
        assert!(err.to_string().contains("UserIdError"));
    }

    #[test]
    fn test_diesel_to_sql() {
        let mut conn = memory_db();
        let uid: UserId = ok("user_999".parse());
        let email: Email = ok("test@domain.com".parse());
        let _ = ok(insert_into(users::table)
            .values((
                users::id.eq(&uid),
                users::email.eq(&email),
            ))
            .execute(&mut conn));
        let rows: Vec<(String, String)> = ok(users::table
            .select((users::id, users::email))
            .load(&mut conn));
        assert_eq!(rows[0].0, "user_999");
        assert_eq!(rows[0].1, "test@domain.com");
    }
}
