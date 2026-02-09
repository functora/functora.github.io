use derive_more::Display;
use functora_tagged::parse_error::ParseError;
use functora_tagged::refine::Refine;
use functora_tagged::via_string::ViaString;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

#[derive(Debug, Display, PartialEq, Eq, Clone)]
struct MyRefineError;

impl std::error::Error for MyRefineError {}

#[derive(Debug, Display)]
struct MyTag;

impl Refine<String> for MyTag {
    type RefineError = MyRefineError;

    fn refine(
        rep: String,
    ) -> Result<String, Self::RefineError> {
        if rep.starts_with("valid_") {
            Ok(rep)
        } else {
            Err(MyRefineError)
        }
    }
}

type TestViaString = ViaString<String, MyTag, MyTag>;
type TestParseError = ParseError<String, MyTag, MyTag>;

#[test]
fn test_via_string_new() {
    let rep_value = String::from("valid_string");
    let via_string_instance =
        TestViaString::new(rep_value.clone());
    assert!(via_string_instance.is_ok());
    assert_eq!(
        via_string_instance.unwrap().rep(),
        &rep_value
    );

    let invalid_rep_value = String::from("invalid_string");
    let via_string_instance_err =
        TestViaString::new(invalid_rep_value);
    assert!(via_string_instance_err.is_err());
    assert_eq!(
        via_string_instance_err.unwrap_err(),
        MyRefineError
    );
}

#[test]
fn test_via_string_rep() {
    let rep_value = String::from("valid_rep");
    let via_string_instance =
        TestViaString::new(rep_value.clone()).unwrap();
    assert_eq!(via_string_instance.rep(), &rep_value);
}

#[test]
fn test_via_string_eq_partial_eq() {
    let vs1 =
        TestViaString::new(String::from("valid_eq_test"))
            .unwrap();
    let vs2 =
        TestViaString::new(String::from("valid_eq_test"))
            .unwrap();
    let vs3 =
        TestViaString::new(String::from("valid_neq_test"))
            .unwrap();

    assert_eq!(vs1, vs2);
    assert_ne!(vs1, vs3);
}

#[test]
fn test_via_string_ord_partial_ord() {
    let vs1 =
        TestViaString::new(String::from("valid_ord_a"))
            .unwrap();
    let vs2 =
        TestViaString::new(String::from("valid_ord_a"))
            .unwrap();
    let vs3 =
        TestViaString::new(String::from("valid_ord_b"))
            .unwrap();
    let vs4 =
        TestViaString::new(String::from("valid_ord_z"))
            .unwrap();

    assert_eq!(vs1.cmp(&vs2), std::cmp::Ordering::Equal);
    assert_eq!(
        vs1.partial_cmp(&vs2),
        Some(std::cmp::Ordering::Equal)
    );

    assert_eq!(vs1.cmp(&vs3), std::cmp::Ordering::Less);
    assert_eq!(
        vs1.partial_cmp(&vs3),
        Some(std::cmp::Ordering::Less)
    );

    assert_eq!(vs3.cmp(&vs4), std::cmp::Ordering::Less);
    assert_eq!(
        vs3.partial_cmp(&vs4),
        Some(std::cmp::Ordering::Less)
    );
}

#[test]
fn test_via_string_clone() {
    let vs1 = TestViaString::new(String::from(
        "valid_clone_test",
    ))
    .unwrap();
    let vs2 = vs1.clone();

    assert_eq!(vs1, vs2);
    assert_eq!(vs1.rep(), vs2.rep());
}

#[test]
fn test_via_string_display() {
    let vs_instance = TestViaString::new(String::from(
        "valid_display_test",
    ))
    .unwrap();
    assert_eq!(
        vs_instance.to_string(),
        "valid_display_test"
    );
}

#[test]
fn test_via_string_hash() {
    let vs1 =
        TestViaString::new(String::from("valid_hash_test"))
            .unwrap();
    let vs2 =
        TestViaString::new(String::from("valid_hash_test"))
            .unwrap();
    let vs3 = TestViaString::new(String::from(
        "valid_another_hash_test",
    ))
    .unwrap();

    let mut hasher1 =
        std::collections::hash_map::DefaultHasher::new();
    vs1.hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 =
        std::collections::hash_map::DefaultHasher::new();
    vs2.hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 =
        std::collections::hash_map::DefaultHasher::new();
    vs3.hash(&mut hasher3);
    let hash3 = hasher3.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
}

#[test]
fn test_via_string_deref() {
    let rep_value = String::from("valid_deref_test");
    let vs_instance =
        TestViaString::new(rep_value.clone()).unwrap();
    assert_eq!(*vs_instance, rep_value);
    assert_eq!(vs_instance.len(), rep_value.len());
}

#[test]
fn test_via_string_from_str() {
    let s_ok = "valid_from_str";
    let vs_ok = TestViaString::from_str(s_ok).unwrap();
    assert_eq!(
        vs_ok.rep(),
        &String::from("valid_from_str")
    );
    let s_refine_err = "invalid_from_str";
    let parse_result_refine: Result<
        TestViaString,
        TestParseError,
    > = FromStr::from_str(s_refine_err);
    assert!(parse_result_refine.is_err());
    match parse_result_refine.unwrap_err() {
        TestParseError::Refine(..) => {}
        _ => panic!("Expected Refine error"),
    }
}

#[cfg(feature = "serde")]
mod serde_tests {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[test]
    fn test_serde_via_string_roundtrip() {
        #[derive(
            Serialize, Deserialize, PartialEq, Debug,
        )]
        struct Wrapper {
            via_string_value: TestViaString,
        }
        let original = Wrapper {
            via_string_value: TestViaString::new(
                "valid_serde_test".to_string(),
            )
            .unwrap(),
        };
        let toml = toml::to_string(&original).unwrap();
        let deserialized: Wrapper =
            toml::from_str(&toml).unwrap();
        assert_eq!(original, deserialized);
        assert_eq!(
            deserialized.via_string_value.rep(),
            "valid_serde_test"
        );
    }

    #[test]
    fn test_serde_via_string_invalid_refine() {
        #[derive(Deserialize, Debug)]
        struct Wrapper {
            via_string_value: TestViaString,
        }

        let toml =
            r##"via_string_value = "invalid_string""##;
        let err =
            toml::from_str::<Wrapper>(toml).unwrap_err();
        assert!(
            err.to_string().contains("MyRefineError"),
            "Unexpected failure: {err}"
        );

        let toml_valid =
            r##"via_string_value = "valid_serde_refine""##;
        let wrapper: Wrapper =
            toml::from_str(toml_valid).unwrap();
        assert_eq!(
            wrapper.via_string_value.rep(),
            "valid_serde_refine"
        );
    }

    #[test]
    fn test_serde_via_string_roundtrip_explicit() {
        #[derive(
            Serialize, Deserialize, PartialEq, Debug,
        )]
        struct Wrapper {
            via_string_value: TestViaString,
        }
        let original = Wrapper {
            via_string_value: TestViaString::new(
                "valid_serde_test".to_string(),
            )
            .unwrap(),
        };
        let toml = toml::to_string(&original).unwrap();
        let deserialized: Wrapper =
            toml::from_str(&toml).unwrap();
        assert_eq!(original, deserialized);
    }
}

#[cfg(feature = "diesel")]
mod via_string_diesel_tests_inline {
    use super::*;
    use diesel::Connection;
    use diesel::ExpressionMethods;
    use diesel::QueryDsl;
    use diesel::QueryableByName;
    use diesel::RunQueryDsl;
    use diesel::SqliteConnection;
    use diesel::insert_into;
    use diesel::sql_query;
    use diesel::sql_types::Integer;
    use diesel::sql_types::Text;
    use diesel::table;

    table! {
        via_string_values (id) {
            id -> Integer,
            value -> Text,
        }
    }

    #[derive(QueryableByName, PartialEq, Debug)]
    pub struct ViaStringRow {
        #[diesel(sql_type = Integer)]
        id: i32,
        #[diesel(sql_type = Text)]
        pub value: TestViaString,
    }

    pub fn memory_db() -> SqliteConnection {
        let mut conn =
            SqliteConnection::establish(":memory:")
                .unwrap_or_else(|_| {
                    panic!("cannot create in-memory DB")
                });
        sql_query(
            "CREATE TABLE via_string_values (id INTEGER PRIMARY KEY AUTOINCREMENT, value TEXT NOT NULL);",
        )
        .execute(&mut conn)
        .expect("failed to create table");
        conn
    }

    #[test]
    fn test_diesel_via_string_queryable_success() {
        let mut conn = memory_db();
        let valid_via_string_value = TestViaString::new(
            "valid_diesel_test".to_string(),
        )
        .unwrap();

        insert_into(via_string_values::table)
            .values((via_string_values::value
                .eq(&valid_via_string_value),))
            .execute(&mut conn)
            .unwrap();

        let rows: Vec<ViaStringRow> = sql_query(
            "SELECT id, value FROM via_string_values",
        )
        .load(&mut conn)
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].value, valid_via_string_value);
        assert_eq!(
            rows[0].value.rep(),
            "valid_diesel_test"
        );
    }

    #[test]
    fn test_diesel_via_string_queryable_refine_failure() {
        let mut conn = memory_db();

        let insert_result = sql_query(
            "INSERT INTO via_string_values (value) VALUES (?)",
        )
        .bind::<Text, _>("invalid_string")
        .execute(&mut conn);
        assert!(
            insert_result.is_ok(),
            "Insert statement failed unexpectedly"
        );

        let err = sql_query(
            "SELECT id, value FROM via_string_values",
        )
        .load::<ViaStringRow>(&mut conn)
        .unwrap_err();

        let err_msg = err.to_string();
        assert!(
            err_msg.contains("MyRefineError"),
            "Expected MyRefineError, but got: {err_msg}"
        );
    }

    #[test]
    fn test_diesel_via_string_to_sql() {
        let mut conn = memory_db();
        let via_string_value = TestViaString::new(
            "valid_to_sql_test".to_string(),
        )
        .unwrap();

        insert_into(via_string_values::table)
            .values((via_string_values::value
                .eq(&via_string_value),))
            .execute(&mut conn)
            .unwrap();

        let rows: Vec<(i32, String)> =
            via_string_values::table
                .select((
                    via_string_values::id,
                    via_string_values::value,
                ))
                .load(&mut conn)
                .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].1, "valid_to_sql_test");
    }
}

#[test]
fn test_via_string_eq_partial_eq_explicit() {
    let vs1 =
        TestViaString::new(String::from("valid_eq_test"))
            .unwrap();
    let vs2 =
        TestViaString::new(String::from("valid_eq_test"))
            .unwrap();
    let vs3 =
        TestViaString::new(String::from("valid_neq_test"))
            .unwrap();

    assert!(vs1 == vs2);
    assert!(vs1 != vs3);
    assert!(vs1.eq(&vs2));
    assert!(!vs1.eq(&vs3));
}

#[test]
fn test_via_string_ord_partial_ord_explicit() {
    let vs1 =
        TestViaString::new(String::from("valid_ord_a"))
            .unwrap();
    let vs2 =
        TestViaString::new(String::from("valid_ord_a"))
            .unwrap();
    let vs3 =
        TestViaString::new(String::from("valid_ord_b"))
            .unwrap();

    assert_eq!(vs1.cmp(&vs2), std::cmp::Ordering::Equal);
    assert_eq!(
        vs1.partial_cmp(&vs2),
        Some(std::cmp::Ordering::Equal)
    );
    assert_eq!(vs1.cmp(&vs3), std::cmp::Ordering::Less);
    assert_eq!(
        vs1.partial_cmp(&vs3),
        Some(std::cmp::Ordering::Less)
    );
}

#[test]
fn test_via_string_clone_explicit() {
    let vs1 = TestViaString::new(String::from(
        "valid_clone_test",
    ))
    .unwrap();
    let vs2 = vs1.clone();
    assert_eq!(vs1, vs2);
}

#[test]
fn test_via_string_display_explicit() {
    let vs_instance = TestViaString::new(String::from(
        "valid_display_test",
    ))
    .unwrap();
    assert_eq!(
        vs_instance.to_string(),
        "valid_display_test"
    );
}

#[test]
fn test_via_string_hash_explicit() {
    let vs1 =
        TestViaString::new(String::from("valid_hash_test"))
            .unwrap();
    let vs2 =
        TestViaString::new(String::from("valid_hash_test"))
            .unwrap();

    let mut hasher1 =
        std::collections::hash_map::DefaultHasher::new();
    vs1.hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 =
        std::collections::hash_map::DefaultHasher::new();
    vs2.hash(&mut hasher2);
    let hash2 = hasher2.finish();

    assert_eq!(hash1, hash2);
}

#[test]
fn test_via_string_deref_explicit() {
    let vs_instance = TestViaString::new(String::from(
        "valid_deref_test",
    ))
    .unwrap();
    assert_eq!(*vs_instance, "valid_deref_test");
}

#[test]
fn test_via_string_from_str_explicit() {
    let s_ok = "valid_from_str";
    let vs_ok = TestViaString::from_str(s_ok).unwrap();
    assert_eq!(
        vs_ok.rep(),
        &String::from("valid_from_str")
    );
}

#[test]
fn test_via_string_copy() {
    #[derive(Debug)]
    struct CopyTag;
    impl Refine<i32> for CopyTag {
        type RefineError = MyRefineError;
        fn refine(
            rep: i32,
        ) -> Result<i32, Self::RefineError> {
            Ok(rep)
        }
    }

    type TestViaStringCopy =
        ViaString<i32, CopyTag, CopyTag>;

    let vs1 = TestViaStringCopy::new(123).unwrap();
    let vs2 = vs1;
    assert_eq!(vs1, vs2);
    assert_eq!(vs1.rep(), vs2.rep());
}
