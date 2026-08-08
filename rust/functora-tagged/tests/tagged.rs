use derive_more::Display;
use functora_tagged::*;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

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

#[cfg(feature = "diesel")]
use diesel::ExpressionMethods;
#[cfg(feature = "diesel")]
use diesel::prelude::*;

#[derive(Debug, Display, PartialEq, Eq, Clone)]
struct MyRefineError;

impl std::error::Error for MyRefineError {}

#[derive(Debug)]
enum MyTag {}

impl Refine<i32> for MyTag {
    type RefineError = MyRefineError;

    fn refine(rep: i32) -> Result<i32, Self::RefineError> {
        if rep >= 0 {
            Ok(rep)
        } else {
            Err(MyRefineError)
        }
    }
}

type TestTagged = Tagged<i32, MyTag, MyTag>;
type TestParseError = ParseError<i32, MyTag, MyTag>;

#[test]
fn test_tagged_new() {
    let rep_value = 10;
    let tagged_instance = TestTagged::new(rep_value);
    assert!(tagged_instance.is_ok());
    assert_eq!(ok(tagged_instance).rep(), &rep_value);

    let negative_rep_value = -5;
    let tagged_instance_err =
        TestTagged::new(negative_rep_value);
    assert!(tagged_instance_err.is_err());
    assert_eq!(err(tagged_instance_err), MyRefineError);
}

#[test]
fn test_tagged_rep() {
    let rep_value = 20;
    let tagged_instance = ok(TestTagged::new(rep_value));
    assert_eq!(tagged_instance.rep(), &rep_value);
}

#[test]
fn test_tagged_eq_partial_eq() {
    let tagged1 = ok(TestTagged::new(30));
    let tagged2 = ok(TestTagged::new(30));
    let tagged3 = ok(TestTagged::new(40));

    assert_eq!(tagged1, tagged2);
    assert_ne!(tagged1, tagged3);
}

#[test]
fn test_tagged_ord_partial_ord() {
    let tagged1 = ok(TestTagged::new(50));
    let tagged2 = ok(TestTagged::new(50));
    let tagged3 = ok(TestTagged::new(60));
    let tagged4 = ok(TestTagged::new(40));

    assert_eq!(
        tagged1.cmp(&tagged2),
        std::cmp::Ordering::Equal
    );
    assert_eq!(
        tagged1.partial_cmp(&tagged2),
        Some(std::cmp::Ordering::Equal)
    );

    assert_eq!(
        tagged1.cmp(&tagged3),
        std::cmp::Ordering::Less
    );
    assert_eq!(
        tagged1.partial_cmp(&tagged3),
        Some(std::cmp::Ordering::Less)
    );

    assert_eq!(
        tagged1.cmp(&tagged4),
        std::cmp::Ordering::Greater
    );
    assert_eq!(
        tagged1.partial_cmp(&tagged4),
        Some(std::cmp::Ordering::Greater)
    );
}

#[test]
fn test_tagged_clone() {
    let tagged1 = ok(TestTagged::new(70));
    let tagged2 = tagged1;

    assert_eq!(tagged1, tagged2);
    assert_eq!(tagged1.rep(), tagged2.rep());
}

#[test]
fn test_tagged_display() {
    let tagged_instance = ok(TestTagged::new(80));
    assert_eq!(tagged_instance.to_string(), "80");
}

#[test]
fn test_tagged_hash() {
    let tagged1 = ok(TestTagged::new(90));
    let tagged2 = ok(TestTagged::new(90));
    let tagged3 = ok(TestTagged::new(100));

    let mut hasher1 =
        std::collections::hash_map::DefaultHasher::new();
    tagged1.hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 =
        std::collections::hash_map::DefaultHasher::new();
    tagged2.hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 =
        std::collections::hash_map::DefaultHasher::new();
    tagged3.hash(&mut hasher3);
    let hash3 = hasher3.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
}

#[test]
fn test_tagged_deref() {
    let rep_value: i32 = 110;
    let tagged_instance = ok(TestTagged::new(rep_value));
    assert_eq!(*tagged_instance, rep_value);
    assert_eq!(tagged_instance.abs(), rep_value.abs());
}

#[test]
fn test_tagged_from_str() {
    let s_ok = "120";
    let tagged_ok = ok(TestTagged::from_str(s_ok));
    assert_eq!(tagged_ok.rep(), &120);

    let s_decode_err = "abc";
    let parse_result_decode: Result<
        TestTagged,
        TestParseError,
    > = FromStr::from_str(s_decode_err);
    assert!(parse_result_decode.is_err());
    match err(parse_result_decode) {
        TestParseError::Decode(..) => {}
        TestParseError::Refine(..) => {
            panic!("Expected Decode error")
        }
    }

    let s_refine_err = "-10";
    let parse_result_refine: Result<
        TestTagged,
        TestParseError,
    > = FromStr::from_str(s_refine_err);
    assert!(parse_result_refine.is_err());
    match err(parse_result_refine) {
        TestParseError::Refine(..) => {}
        TestParseError::Decode(..) => {
            panic!("Expected Refine error")
        }
    }
}

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "serde")]
mod serde_tests {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[test]
    fn test_serde_tagged_roundtrip() {
        #[derive(
            Serialize, Deserialize, PartialEq, Debug,
        )]
        struct Wrapper {
            tagged_value: TestTagged,
        }
        let original = Wrapper {
            tagged_value: ok(TestTagged::new(100)),
        };
        let toml = ok(toml::to_string(&original));
        let deserialized: Wrapper =
            ok(toml::from_str(&toml));
        assert_eq!(original, deserialized);
        assert_eq!(deserialized.tagged_value.rep(), &100);
    }

    #[test]
    fn test_serde_tagged_invalid_refine() {
        #[derive(Deserialize, Debug)]
        struct Wrapper {
            tagged_value: TestTagged,
        }

        let toml = r"tagged_value = -1";
        let err = err(toml::from_str::<Wrapper>(toml));
        assert!(
            err.to_string().contains("MyRefineError"),
            "Unexpected failure: {err}"
        );

        let toml_valid = r"tagged_value = 50";
        let wrapper: Wrapper =
            ok(toml::from_str(toml_valid));
        assert_eq!(wrapper.tagged_value.rep(), &50);
    }
}

#[cfg(feature = "diesel")]
mod tagged_diesel_tests {
    use super::*;
    use diesel::Connection;
    use diesel::ExpressionMethods;
    use diesel::QueryDsl;
    use diesel::QueryableByName;
    use diesel::RunQueryDsl;
    use diesel::insert_into;
    use diesel::sql_query;
    use diesel::sql_types::Integer;
    use diesel::sqlite::SqliteConnection;
    use diesel::table;

    table! {
        tagged_values (id) {
            id -> Integer,
            value -> Integer,
        }
    }

    #[derive(QueryableByName, PartialEq, Debug)]
    pub struct TaggedRow {
        #[diesel(sql_type = Integer)]
        id: i32,
        #[diesel(sql_type = Integer)]
        pub value: TestTagged,
    }

    pub fn memory_db() -> SqliteConnection {
        let mut conn =
            SqliteConnection::establish(":memory:")
                .unwrap_or_else(|_| {
                    panic!("cannot create in-memory DB")
                });
        #[allow(clippy::unwrap_used)]
        let _ = sql_query(
            "CREATE TABLE tagged_values (id INTEGER PRIMARY KEY AUTOINCREMENT, value INTEGER NOT NULL);",
        )
        .execute(&mut conn)
        .unwrap();
        conn
    }

    #[test]
    fn test_diesel_tagged_queryable_success() {
        let mut conn = memory_db();
        let valid_tagged_value = ok(TestTagged::new(100));

        let _ = ok(insert_into(tagged_values::table)
            .values((tagged_values::value
                .eq(&valid_tagged_value),))
            .execute(&mut conn));

        let rows: Vec<TaggedRow> = ok(sql_query(
            "SELECT id, value FROM tagged_values",
        )
        .load(&mut conn));

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].value, valid_tagged_value);
        assert_eq!(rows[0].value.rep(), &100);
    }

    #[test]
    fn test_diesel_tagged_queryable_refine_failure() {
        let mut conn = memory_db();

        let insert_result = sql_query(
            "INSERT INTO tagged_values (value) VALUES (?)",
        )
        .bind::<Integer, _>(-5i32)
        .execute(&mut conn);
        assert!(
            insert_result.is_ok(),
            "Insert statement failed unexpectedly"
        );

        let err = err(sql_query(
            "SELECT id, value FROM tagged_values",
        )
        .load::<TaggedRow>(&mut conn));

        let err_msg = err.to_string();
        assert!(
            err_msg.contains("MyRefineError"),
            "Expected MyRefineError, but got: {err_msg}"
        );
    }

    #[test]
    fn test_diesel_tagged_to_sql() {
        let mut conn = memory_db();
        let tagged_value = ok(TestTagged::new(150));

        let _ = ok(insert_into(tagged_values::table)
            .values((
                tagged_values::value.eq(&tagged_value),
            ))
            .execute(&mut conn));

        let rows: Vec<(i32, i32)> =
            ok(tagged_values::table
                .select((
                    tagged_values::id,
                    tagged_values::value,
                ))
                .load(&mut conn));

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].1, 150);
    }
}

#[test]
fn test_tagged_eq_partial_eq_explicit() {
    let tagged1 = ok(TestTagged::new(30));
    let tagged2 = ok(TestTagged::new(30));
    let tagged3 = ok(TestTagged::new(40));

    assert!(tagged1 == tagged2);
    assert!(tagged1 != tagged3);
    assert!(tagged1.eq(&tagged2));
    assert!(!tagged1.eq(&tagged3));
}

#[test]
fn test_tagged_ord_partial_ord_explicit() {
    let tagged1 = ok(TestTagged::new(50));
    let tagged2 = ok(TestTagged::new(50));
    let tagged3 = ok(TestTagged::new(60));

    assert_eq!(
        tagged1.cmp(&tagged2),
        std::cmp::Ordering::Equal
    );
    assert_eq!(
        tagged1.partial_cmp(&tagged2),
        Some(std::cmp::Ordering::Equal)
    );
    assert_eq!(
        tagged1.cmp(&tagged3),
        std::cmp::Ordering::Less
    );
    assert_eq!(
        tagged1.partial_cmp(&tagged3),
        Some(std::cmp::Ordering::Less)
    );
}

#[test]
fn test_tagged_clone_explicit() {
    let tagged1 = ok(TestTagged::new(70));
    let tagged2 = tagged1;
    assert_eq!(tagged1, tagged2);
}

#[test]
fn test_tagged_display_explicit() {
    let tagged_instance = ok(TestTagged::new(80));
    assert_eq!(tagged_instance.to_string(), "80");
}

#[test]
fn test_tagged_hash_explicit() {
    let tagged1 = ok(TestTagged::new(90));
    let tagged2 = ok(TestTagged::new(90));

    let mut hasher1 =
        std::collections::hash_map::DefaultHasher::new();
    tagged1.hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 =
        std::collections::hash_map::DefaultHasher::new();
    tagged2.hash(&mut hasher2);
    let hash2 = hasher2.finish();

    assert_eq!(hash1, hash2);
}

#[test]
fn test_tagged_deref_explicit() {
    let tagged_instance = ok(TestTagged::new(110));
    assert_eq!(*tagged_instance, 110);
}

#[test]
fn test_tagged_from_str_explicit() {
    let s_ok = "120";
    let tagged_ok = ok(TestTagged::from_str(s_ok));
    assert_eq!(tagged_ok.rep(), &120);
}

#[cfg(feature = "serde")]
#[test]
fn test_serde_tagged_roundtrip_explicit() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Wrapper {
        tagged_value: TestTagged,
    }
    let original = Wrapper {
        tagged_value: ok(TestTagged::new(100)),
    };
    let toml = ok(toml::to_string(&original));
    let deserialized: Wrapper = ok(toml::from_str(&toml));
    assert_eq!(original, deserialized);
}

#[cfg(feature = "diesel")]
#[test]
fn test_diesel_tagged_queryable_success_explicit() {
    use crate::tagged_diesel_tests::memory_db;
    use diesel::sql_query;

    let mut conn = memory_db();
    let valid_tagged_value = ok(TestTagged::new(100));

    let _ = ok(diesel::insert_into(
        crate::tagged_diesel_tests::tagged_values::table,
    )
    .values((
        crate::tagged_diesel_tests::tagged_values::value
            .eq(&valid_tagged_value),
    ))
    .execute(&mut conn));

    let rows: Vec<crate::tagged_diesel_tests::TaggedRow> =
        ok(sql_query(
            "SELECT id, value FROM tagged_values",
        )
        .load(&mut conn));

    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].value, valid_tagged_value);
}
#[test]
fn test_tagged_copy() {
    let tagged1 = ok(TestTagged::new(100));
    let tagged2 = tagged1;
    assert_eq!(tagged1, tagged2);
    assert_eq!(tagged1.rep(), tagged2.rep());
}

#[test]
fn test_tagged_untag() {
    let rep_value = 110;
    let tagged_instance = ok(TestTagged::new(rep_value));
    let untagged_value = tagged_instance.untag();
    assert_eq!(untagged_value, rep_value);
}
