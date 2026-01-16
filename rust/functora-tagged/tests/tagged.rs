use derive_more::Display;
use functora_tagged::parse_error::ParseError;
use functora_tagged::refine::Refine;
use functora_tagged::tagged::Tagged;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

#[cfg(feature = "diesel")]
use diesel::ExpressionMethods;
#[cfg(feature = "diesel")]
use diesel::prelude::*;

#[derive(Debug, Display, PartialEq, Eq, Clone)]
struct MyRefineError;

impl std::error::Error for MyRefineError {}

#[derive(Debug)]
struct MyTag;

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

type TestTagged = Tagged<i32, MyTag>;
type TestParseError = ParseError<i32, MyTag>;

#[test]
fn test_tagged_new() {
    let rep_value = 10;
    let tagged_instance = TestTagged::new(rep_value);
    assert!(tagged_instance.is_ok());
    assert_eq!(tagged_instance.unwrap().rep(), &rep_value);

    let negative_rep_value = -5;
    let tagged_instance_err =
        TestTagged::new(negative_rep_value);
    assert!(tagged_instance_err.is_err());
    assert_eq!(
        tagged_instance_err.unwrap_err(),
        MyRefineError
    );
}

#[test]
fn test_tagged_rep() {
    let rep_value = 20;
    let tagged_instance =
        TestTagged::new(rep_value).unwrap();
    assert_eq!(tagged_instance.rep(), &rep_value);
}

#[test]
fn test_tagged_eq_partial_eq() {
    let tagged1 = TestTagged::new(30).unwrap();
    let tagged2 = TestTagged::new(30).unwrap();
    let tagged3 = TestTagged::new(40).unwrap();

    assert_eq!(tagged1, tagged2);
    assert_ne!(tagged1, tagged3);
}

#[test]
fn test_tagged_ord_partial_ord() {
    let tagged1 = TestTagged::new(50).unwrap();
    let tagged2 = TestTagged::new(50).unwrap();
    let tagged3 = TestTagged::new(60).unwrap();
    let tagged4 = TestTagged::new(40).unwrap();

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
    let tagged1 = TestTagged::new(70).unwrap();
    let tagged2 = tagged1.clone();

    assert_eq!(tagged1, tagged2);
    assert_eq!(tagged1.rep(), tagged2.rep());
}

#[test]
fn test_tagged_display() {
    let tagged_instance = TestTagged::new(80).unwrap();
    assert_eq!(tagged_instance.to_string(), "80");
}

#[test]
fn test_tagged_hash() {
    let tagged1 = TestTagged::new(90).unwrap();
    let tagged2 = TestTagged::new(90).unwrap();
    let tagged3 = TestTagged::new(100).unwrap();

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
    let rep_value = 110;
    let tagged_instance =
        TestTagged::new(rep_value).unwrap();
    assert_eq!(*tagged_instance, rep_value);
    assert_eq!(tagged_instance.abs(), rep_value.abs());
}

#[test]
fn test_tagged_from_str() {
    let s_ok = "120";
    let tagged_ok = TestTagged::from_str(s_ok).unwrap();
    assert_eq!(tagged_ok.rep(), &120);

    let s_decode_err = "abc";
    let parse_result_decode: Result<
        TestTagged,
        TestParseError,
    > = FromStr::from_str(s_decode_err);
    assert!(parse_result_decode.is_err());
    match parse_result_decode.unwrap_err() {
        TestParseError::Decode(_) => {}
        _ => panic!("Expected Decode error"),
    }

    let s_refine_err = "-10";
    let parse_result_refine: Result<
        TestTagged,
        TestParseError,
    > = FromStr::from_str(s_refine_err);
    assert!(parse_result_refine.is_err());
    match parse_result_refine.unwrap_err() {
        TestParseError::Refine(_) => {}
        _ => panic!("Expected Refine error"),
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
            tagged_value: TestTagged::new(100).unwrap(),
        };
        let toml = toml::to_string(&original).unwrap();
        let deserialized: Wrapper =
            toml::from_str(&toml).unwrap();
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
        let err =
            toml::from_str::<Wrapper>(toml).unwrap_err();
        assert!(
            err.to_string().contains("MyRefineError"),
            "Unexpected failure: {err}"
        );

        let toml_valid = r"tagged_value = 50";
        let wrapper: Wrapper =
            toml::from_str(toml_valid).unwrap();
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
        sql_query(
            "CREATE TABLE tagged_values (id INTEGER PRIMARY KEY AUTOINCREMENT, value INTEGER NOT NULL);",
        )
        .execute(&mut conn)
        .expect("failed to create table");
        conn
    }

    #[test]
    fn test_diesel_tagged_queryable_success() {
        let mut conn = memory_db();
        let valid_tagged_value =
            TestTagged::new(100).unwrap();

        insert_into(tagged_values::table)
            .values((tagged_values::value
                .eq(&valid_tagged_value),))
            .execute(&mut conn)
            .unwrap();

        let rows: Vec<TaggedRow> = sql_query(
            "SELECT id, value FROM tagged_values",
        )
        .load(&mut conn)
        .unwrap();

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

        let err = sql_query(
            "SELECT id, value FROM tagged_values",
        )
        .load::<TaggedRow>(&mut conn)
        .unwrap_err();

        let err_msg = err.to_string();
        assert!(
            err_msg.contains("MyRefineError"),
            "Expected MyRefineError, but got: {err_msg}"
        );
    }

    #[test]
    fn test_diesel_tagged_to_sql() {
        let mut conn = memory_db();
        let tagged_value = TestTagged::new(150).unwrap();

        insert_into(tagged_values::table)
            .values((
                tagged_values::value.eq(&tagged_value),
            ))
            .execute(&mut conn)
            .unwrap();

        let rows: Vec<(i32, i32)> = tagged_values::table
            .select((
                tagged_values::id,
                tagged_values::value,
            ))
            .load(&mut conn)
            .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].1, 150);
    }
}

#[test]
fn test_tagged_eq_partial_eq_explicit() {
    let tagged1 = TestTagged::new(30).unwrap();
    let tagged2 = TestTagged::new(30).unwrap();
    let tagged3 = TestTagged::new(40).unwrap();

    assert!(tagged1 == tagged2);
    assert!(tagged1 != tagged3);
    assert!(tagged1.eq(&tagged2));
    assert!(!tagged1.eq(&tagged3));
}

#[test]
fn test_tagged_ord_partial_ord_explicit() {
    let tagged1 = TestTagged::new(50).unwrap();
    let tagged2 = TestTagged::new(50).unwrap();
    let tagged3 = TestTagged::new(60).unwrap();

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
    let tagged1 = TestTagged::new(70).unwrap();
    let tagged2 = tagged1.clone();
    assert_eq!(tagged1, tagged2);
}

#[test]
fn test_tagged_display_explicit() {
    let tagged_instance = TestTagged::new(80).unwrap();
    assert_eq!(tagged_instance.to_string(), "80");
}

#[test]
fn test_tagged_hash_explicit() {
    let tagged1 = TestTagged::new(90).unwrap();
    let tagged2 = TestTagged::new(90).unwrap();

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
    let tagged_instance = TestTagged::new(110).unwrap();
    assert_eq!(*tagged_instance, 110);
}

#[test]
fn test_tagged_from_str_explicit() {
    let s_ok = "120";
    let tagged_ok = TestTagged::from_str(s_ok).unwrap();
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
        tagged_value: TestTagged::new(100).unwrap(),
    };
    let toml = toml::to_string(&original).unwrap();
    let deserialized: Wrapper =
        toml::from_str(&toml).unwrap();
    assert_eq!(original, deserialized);
}

#[cfg(feature = "diesel")]
#[test]
fn test_diesel_tagged_queryable_success_explicit() {
    use crate::tagged_diesel_tests::memory_db;
    use diesel::sql_query;

    let mut conn = memory_db();
    let valid_tagged_value = TestTagged::new(100).unwrap();

    diesel::insert_into(
        crate::tagged_diesel_tests::tagged_values::table,
    )
    .values((
        crate::tagged_diesel_tests::tagged_values::value
            .eq(&valid_tagged_value),
    ))
    .execute(&mut conn)
    .unwrap();

    let rows: Vec<crate::tagged_diesel_tests::TaggedRow> =
        sql_query("SELECT id, value FROM tagged_values")
            .load(&mut conn)
            .unwrap();

    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].value, valid_tagged_value);
}
#[test]
fn test_tagged_copy() {
    let tagged1 = TestTagged::new(100).unwrap();
    let tagged2 = tagged1;
    assert_eq!(tagged1, tagged2);
    assert_eq!(tagged1.rep(), tagged2.rep());
}

#[test]
fn test_tagged_untag() {
    let rep_value = 110;
    let tagged_instance =
        TestTagged::new(rep_value).unwrap();
    let untagged_value = tagged_instance.untag();
    assert_eq!(untagged_value, rep_value);
}
