use derive_more::Display;
use functora_tagged::refine::Refine;
use std::error::Error;
use std::fmt::Debug;

#[derive(Debug, Display, PartialEq, Eq, Clone)]
struct MyRefineError;

impl Error for MyRefineError {}

#[derive(Debug)]
struct MyTag;

impl Refine<String> for MyTag {
    type RefineError = MyRefineError;

    fn refine(
        rep: String,
    ) -> Result<String, Self::RefineError> {
        Ok(rep)
    }
}

#[test]
fn test_refine_my_tag_implementation() {
    let rep_value = String::from("test_string");
    let refined_rep = MyTag::refine(rep_value.clone());

    assert!(refined_rep.is_ok());
    assert_eq!(refined_rep.unwrap(), rep_value);
}

struct StrictTag;

impl Refine<String> for StrictTag {
    type RefineError = String;

    fn refine(
        rep: String,
    ) -> Result<String, Self::RefineError> {
        if rep.starts_with("strict_") {
            Ok(rep)
        } else {
            Err(format!(
                "String must start with 'strict_': {}",
                rep
            ))
        }
    }
}

#[test]
fn test_refine_strict_tag_implementation() {
    let strict_value = String::from("strict_value");
    let refined_strict =
        StrictTag::refine(strict_value.clone());
    assert!(refined_strict.is_ok());
    assert_eq!(refined_strict.unwrap(), strict_value);

    let non_strict_value = String::from("non_strict_value");
    let refined_non_strict =
        StrictTag::refine(non_strict_value.clone());
    assert!(refined_non_strict.is_err());
    assert_eq!(
        refined_non_strict.unwrap_err(),
        format!(
            "String must start with 'strict_': {}",
            non_strict_value
        )
    );
}

#[derive(Debug)]
struct DefaultRefineTag;

impl Refine<String> for DefaultRefineTag {
    type RefineError = String;
}

#[test]
fn test_refine_default_implementation() {
    let rep_value = String::from("test_default_refine");

    let refined_rep =
        DefaultRefineTag::refine(rep_value.clone());

    assert!(refined_rep.is_ok());
    assert_eq!(refined_rep.unwrap(), rep_value);
}
