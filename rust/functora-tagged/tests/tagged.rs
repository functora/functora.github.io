use derive_more::Display;
use functora_tagged::parse_error::ParseError;
use functora_tagged::refine::Refine;
use functora_tagged::tagged::Tagged;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

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
