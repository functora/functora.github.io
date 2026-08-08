use derive_more::Display;
use functora_tagged::refine::Refine;
use std::error::Error;
use std::fmt::Debug;

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

#[derive(Debug, Display, PartialEq, Eq, Clone)]
struct TestRefineError;

impl Error for TestRefineError {}

#[derive(Debug)]
enum FTest {}

impl Refine<String> for FTest {
    type RefineError = TestRefineError;

    fn refine(
        rep: String,
    ) -> Result<String, Self::RefineError> {
        Ok(rep)
    }
}

#[test]
fn test_refine_my_tag_implementation() {
    let rep_value = String::from("test_string");
    let refined_rep = FTest::refine(rep_value.clone());

    assert!(refined_rep.is_ok());
    assert_eq!(ok(refined_rep), rep_value);
}

enum FStrict {}

impl Refine<String> for FStrict {
    type RefineError = String;

    fn refine(
        rep: String,
    ) -> Result<String, Self::RefineError> {
        if rep.starts_with("strict_") {
            Ok(rep)
        } else {
            Err(format!(
                "String must start with 'strict_': {rep}"
            ))
        }
    }
}

#[test]
fn test_refine_strict_tag_implementation() {
    let strict_value = String::from("strict_value");
    let refined_strict =
        FStrict::refine(strict_value.clone());
    assert!(refined_strict.is_ok());
    assert_eq!(ok(refined_strict), strict_value);

    let non_strict_value = String::from("non_strict_value");
    let refined_non_strict =
        FStrict::refine(non_strict_value.clone());
    assert!(refined_non_strict.is_err());
    assert_eq!(
        err(refined_non_strict),
        format!(
            "String must start with 'strict_': {non_strict_value}"
        )
    );
}

#[derive(Debug)]
enum FDefault {}

impl Refine<String> for FDefault {
    type RefineError = String;
}

#[test]
fn test_refine_default_implementation() {
    let rep_value = String::from("test_default_refine");

    let refined_rep = FDefault::refine(rep_value.clone());

    assert!(refined_rep.is_ok());
    assert_eq!(ok(refined_rep), rep_value);
}
