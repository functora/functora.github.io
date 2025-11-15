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

type TestViaString = ViaString<String, MyTag>;
type TestParseError = ParseError<String, MyTag>;

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
        TestParseError::Refine(_) => {}
        _ => panic!("Expected Refine error"),
    }
}
