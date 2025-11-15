use derive_more::Display;
use functora_tagged::parse_error::ParseError;
use functora_tagged::refine::Refine;
use std::error::Error;
use std::fmt::Debug;

#[derive(Debug, Display, PartialEq, Eq, Clone)]
struct MyRefineError;

impl Error for MyRefineError {}

#[derive(Debug, Display)]
struct MyTag;

impl Refine<i32> for MyTag {
    type RefineError = MyRefineError;

    fn refine(rep: i32) -> Result<i32, Self::RefineError> {
        if rep > 0 { Ok(rep) } else { Err(MyRefineError) }
    }
}

type TestParseError = ParseError<i32, MyTag>;

#[test]
fn test_parse_error_decode() {
    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error =
        TestParseError::Decode(decode_err.clone());

    let formatted_error = format!("{}", parse_error);
    assert!(
        formatted_error.contains(&decode_err.to_string())
    );

    let debug_formatted_error =
        format!("{:?}", parse_error);
    assert_eq!(
        debug_formatted_error,
        "Decode(ParseIntError { kind: InvalidDigit })"
    );

    assert_eq!(
        parse_error,
        TestParseError::Decode(decode_err)
    );
    assert_ne!(
        parse_error,
        TestParseError::Refine(MyRefineError)
    );
    assert_eq!(parse_error.clone(), parse_error);
}

#[test]
fn test_parse_error_refine() {
    let refine_err = MyRefineError;
    let parse_error =
        TestParseError::Refine(refine_err.clone());

    let formatted_error = format!("{}", parse_error);
    assert!(
        formatted_error.contains(&refine_err.to_string())
    );

    let debug_formatted_error =
        format!("{:?}", parse_error);
    assert_eq!(
        debug_formatted_error,
        "Refine(MyRefineError)"
    );

    assert_eq!(
        parse_error,
        TestParseError::Refine(refine_err)
    );
    let decode_err_for_neq =
        "abc".parse::<i32>().unwrap_err();
    assert_ne!(
        parse_error,
        TestParseError::Decode(decode_err_for_neq)
    );
    assert_eq!(parse_error.clone(), parse_error);
}

#[test]
fn test_parse_error_eq() {
    let decode_err1 = "abc".parse::<i32>().unwrap_err();
    let decode_err2 = "abc".parse::<i32>().unwrap_err();
    let refine_err1 = MyRefineError;

    let err_decode1 =
        TestParseError::Decode(decode_err1.clone());
    let err_decode2 =
        TestParseError::Decode(decode_err2.clone());

    let err_refine1 =
        TestParseError::Refine(refine_err1.clone());
    let err_refine2 =
        TestParseError::Refine(refine_err1.clone());

    assert_eq!(err_decode1, err_decode2);
    assert_eq!(err_refine1, err_refine2);
    assert_ne!(err_decode1, err_refine1);
}
