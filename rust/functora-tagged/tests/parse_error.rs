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

#[test]
fn test_parse_error_error_trait() {
    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode =
        TestParseError::Decode(decode_err.clone());
    if let Some(source_err) = parse_error_decode.source() {
        if let Some(parsed_int_err) =
            source_err
                .downcast_ref::<std::num::ParseIntError>()
        {
            assert_eq!(parsed_int_err, &decode_err);
        } else {
            panic!("Source error was not a ParseIntError");
        }
    } else {
        panic!("Source was None");
    }

    let refine_err = MyRefineError;
    let parse_error_refine =
        TestParseError::Refine(refine_err.clone());
    if let Some(source_err) = parse_error_refine.source() {
        if let Some(my_refine_err) =
            source_err.downcast_ref::<MyRefineError>()
        {
            assert_eq!(my_refine_err, &refine_err);
        } else {
            panic!("Source error was not a MyRefineError");
        }
    } else {
        panic!("Source was None");
    }
}

#[test]
fn test_parse_error_eq_trait() {
    let decode_err1 = "abc".parse::<i32>().unwrap_err();
    let refine_err1 = MyRefineError;

    let err_decode1 =
        TestParseError::Decode(decode_err1.clone());

    let err_refine1 =
        TestParseError::Refine(refine_err1.clone());

    assert_eq!(err_decode1, err_decode1);
    assert_eq!(err_refine1, err_refine1);

    assert_ne!(err_decode1, err_refine1);
}

#[test]
fn test_parse_error_clone_variants() {
    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode =
        TestParseError::Decode(decode_err.clone());
    let cloned_decode = parse_error_decode.clone();
    assert_eq!(parse_error_decode, cloned_decode);

    let refine_err = MyRefineError;
    let parse_error_refine =
        TestParseError::Refine(refine_err.clone());
    let cloned_refine = parse_error_refine.clone();
    assert_eq!(parse_error_refine, cloned_refine);
}
