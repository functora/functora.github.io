use derive_more::Display;
use functora_tagged::parse_error::ParseError;
use functora_tagged::refine::Refine;
use std::error::Error;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::str::FromStr;

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

type TestParseError = ParseError<i32, MyTag, MyTag>;

#[test]
fn test_parse_error_decode() {
    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error = TestParseError::Decode(
        decode_err.clone(),
        PhantomData,
    );

    let formatted_error = format!("{}", parse_error);
    assert!(
        formatted_error
            .contains(&format!("{:?}", decode_err))
    );

    let debug_formatted_error =
        format!("{:?}", parse_error);
    assert_eq!(
        debug_formatted_error,
        "Decode(ParseIntError { kind: InvalidDigit }, PhantomData<(parse_error::MyTag, parse_error::MyTag)>)"
    );

    assert_eq!(
        parse_error,
        TestParseError::Decode(decode_err, PhantomData)
    );
    assert_ne!(
        parse_error,
        TestParseError::Refine(MyRefineError, PhantomData)
    );
    assert_eq!(parse_error.clone(), parse_error);
}

#[test]
fn test_parse_error_refine() {
    let refine_err = MyRefineError;
    let parse_error = TestParseError::Refine(
        refine_err.clone(),
        PhantomData,
    );

    let formatted_error = format!("{}", parse_error);
    assert!(
        formatted_error.contains(&refine_err.to_string())
    );

    let debug_formatted_error =
        format!("{:?}", parse_error);
    assert_eq!(
        debug_formatted_error,
        "Refine(MyRefineError, PhantomData<(parse_error::MyTag, parse_error::MyTag)>)"
    );

    assert_eq!(
        parse_error,
        TestParseError::Refine(refine_err, PhantomData)
    );
    let decode_err_for_neq =
        "abc".parse::<i32>().unwrap_err();
    assert_ne!(
        parse_error,
        TestParseError::Decode(
            decode_err_for_neq,
            PhantomData
        )
    );
    assert_eq!(parse_error.clone(), parse_error);
}

#[test]
fn test_parse_error_eq() {
    let decode_err1 = "abc".parse::<i32>().unwrap_err();
    let decode_err2 = "abc".parse::<i32>().unwrap_err();
    let refine_err1 = MyRefineError;

    let err_decode1 = TestParseError::Decode(
        decode_err1.clone(),
        PhantomData,
    );
    let err_decode2 = TestParseError::Decode(
        decode_err2.clone(),
        PhantomData,
    );

    let err_refine1 = TestParseError::Refine(
        refine_err1.clone(),
        PhantomData,
    );
    let err_refine2 = TestParseError::Refine(
        refine_err1.clone(),
        PhantomData,
    );

    assert_eq!(err_decode1, err_decode2);
    assert_eq!(err_refine1, err_refine2);
    assert_ne!(err_decode1, err_refine1);
}

#[test]
fn test_parse_error_error_trait() {
    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode = TestParseError::Decode(
        decode_err.clone(),
        PhantomData,
    );
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
    let parse_error_refine = TestParseError::Refine(
        refine_err.clone(),
        PhantomData,
    );
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

    let err_decode1 = TestParseError::Decode(
        decode_err1.clone(),
        PhantomData,
    );

    let err_refine1 = TestParseError::Refine(
        refine_err1.clone(),
        PhantomData,
    );

    assert_eq!(err_decode1, err_decode1);
    assert_eq!(err_refine1, err_refine1);

    assert_ne!(err_decode1, err_refine1);
}

#[test]
fn test_parse_error_clone_variants() {
    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode = TestParseError::Decode(
        decode_err.clone(),
        PhantomData,
    );
    let cloned_decode = parse_error_decode.clone();
    assert_eq!(parse_error_decode, cloned_decode);

    let refine_err = MyRefineError;
    let parse_error_refine = TestParseError::Refine(
        refine_err.clone(),
        PhantomData,
    );
    let cloned_refine = parse_error_refine.clone();
    assert_eq!(parse_error_refine, cloned_refine);
}

#[test]
fn test_parse_error_decode_clone_refine_eq() {
    let decode_err_orig = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode = TestParseError::Decode(
        decode_err_orig.clone(),
        PhantomData,
    );
    let cloned_decode = parse_error_decode.clone();
    let refine_err_orig = MyRefineError;
    let parse_error_refine = TestParseError::Refine(
        refine_err_orig,
        PhantomData,
    );

    assert_eq!(parse_error_decode, cloned_decode);

    assert_ne!(cloned_decode, parse_error_refine);
}

#[test]
fn test_parse_error_refine_clone_decode_eq() {
    let refine_err_orig = MyRefineError;
    let parse_error_refine = TestParseError::Refine(
        refine_err_orig.clone(),
        PhantomData,
    );
    let cloned_refine = parse_error_refine.clone();

    let decode_err_orig = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode = TestParseError::Decode(
        decode_err_orig,
        PhantomData,
    );

    assert_eq!(parse_error_refine, cloned_refine);

    assert_ne!(cloned_refine, parse_error_decode);
}

#[test]
fn test_parse_error_debug_formatting() {
    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode = TestParseError::Decode(
        decode_err.clone(),
        PhantomData,
    );
    assert_eq!(
        format!("{:?}", parse_error_decode),
        "Decode(ParseIntError { kind: InvalidDigit }, PhantomData<(parse_error::MyTag, parse_error::MyTag)>)"
    );

    let refine_err = MyRefineError;
    let parse_error_refine = TestParseError::Refine(
        refine_err.clone(),
        PhantomData,
    );
    assert_eq!(
        format!("{:?}", parse_error_refine),
        "Refine(MyRefineError, PhantomData<(parse_error::MyTag, parse_error::MyTag)>)"
    );
}

#[test]
fn test_parse_error_display_formatting() {
    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode = TestParseError::Decode(
        decode_err.clone(),
        PhantomData,
    );
    assert!(
        format!("{}", parse_error_decode)
            .contains(&format!("{:?}", decode_err))
    );

    let refine_err = MyRefineError;
    let parse_error_refine = TestParseError::Refine(
        refine_err.clone(),
        PhantomData,
    );
    assert!(
        format!("{}", parse_error_refine)
            .contains(&refine_err.to_string())
    );
}

#[test]
fn test_parse_error_source_method_downcasting() {
    let decode_err_orig = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode = TestParseError::Decode(
        decode_err_orig.clone(),
        PhantomData,
    );
    if let Some(source) = parse_error_decode.source() {
        assert!(source.is::<std::num::ParseIntError>());
        assert_eq!(
            source
                .downcast_ref::<std::num::ParseIntError>(),
            Some(&decode_err_orig)
        );
    } else {
        panic!(
            "Source should not be None for Decode variant"
        );
    }

    let refine_err_orig = MyRefineError;
    let parse_error_refine = TestParseError::Refine(
        refine_err_orig.clone(),
        PhantomData,
    );
    if let Some(source) = parse_error_refine.source() {
        assert!(source.is::<MyRefineError>());
        assert_eq!(
            source.downcast_ref::<MyRefineError>(),
            Some(&refine_err_orig)
        );
    } else {
        panic!(
            "Source should not be None for Refine variant"
        );
    }
}

#[test]
fn test_parse_error_partial_eq_different_variants() {
    let parse_int_err: std::num::ParseIntError =
        "abc".parse::<i32>().unwrap_err();
    let parse_error_decode =
        TestParseError::Decode(parse_int_err, PhantomData);

    let refine_err = MyRefineError;
    let parse_error_refine =
        TestParseError::Refine(refine_err, PhantomData);

    assert_ne!(parse_error_decode, parse_error_refine);
}

#[test]
fn test_parse_error_eq_with_different_types() {
    #[derive(Debug, PartialEq, Eq, Clone, Display)]
    struct AnotherRefineError;
    impl Error for AnotherRefineError {}

    #[derive(Debug, Display)]
    struct AnotherTag;
    impl Refine<i32> for AnotherTag {
        type RefineError = AnotherRefineError;
        fn refine(
            rep: i32,
        ) -> Result<i32, Self::RefineError> {
            if rep > 100 {
                Ok(rep)
            } else {
                Err(AnotherRefineError)
            }
        }
    }

    type AnotherParseError =
        ParseError<i32, AnotherTag, AnotherTag>;

    let decode_err1 = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode1 =
        AnotherParseError::Decode(decode_err1, PhantomData);

    let refine_err1 = AnotherRefineError;
    let parse_error_refine1 =
        AnotherParseError::Refine(refine_err1, PhantomData);

    assert_ne!(parse_error_decode1, parse_error_refine1);
}

#[test]
fn test_parse_error_partial_eq_with_different_types() {
    #[derive(Debug, PartialEq, Eq, Clone, Display)]
    struct YetAnotherRefineError;
    impl Error for YetAnotherRefineError {}

    #[derive(Debug, Display)]
    struct YetAnotherTag;
    impl Refine<i32> for YetAnotherTag {
        type RefineError = YetAnotherRefineError;
        fn refine(
            rep: i32,
        ) -> Result<i32, Self::RefineError> {
            if rep > 200 {
                Ok(rep)
            } else {
                Err(YetAnotherRefineError)
            }
        }
    }

    type YetAnotherParseError =
        ParseError<i32, YetAnotherTag, YetAnotherTag>;

    let decode_err1 = "xyz".parse::<i32>().unwrap_err();
    let parse_error_decode1 = YetAnotherParseError::Decode(
        decode_err1,
        PhantomData,
    );

    let refine_err1 = YetAnotherRefineError;
    let parse_error_refine1 = YetAnotherParseError::Refine(
        refine_err1,
        PhantomData,
    );

    assert_ne!(parse_error_decode1, parse_error_refine1);
}

#[test]
fn test_parse_error_clone_with_different_types() {
    #[derive(Debug, PartialEq, Eq, Clone, Display)]
    struct CloneRefineError;
    impl Error for CloneRefineError {}

    #[derive(Debug, Display)]
    struct CloneTag;
    impl Refine<i32> for CloneTag {
        type RefineError = CloneRefineError;
        fn refine(
            rep: i32,
        ) -> Result<i32, Self::RefineError> {
            if rep > 300 {
                Ok(rep)
            } else {
                Err(CloneRefineError)
            }
        }
    }

    type CloneParseError =
        ParseError<i32, CloneTag, CloneTag>;

    let decode_err_orig = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode = CloneParseError::Decode(
        decode_err_orig.clone(),
        PhantomData,
    );
    let cloned_decode = parse_error_decode.clone();
    assert_eq!(parse_error_decode, cloned_decode);

    let refine_err_orig = CloneRefineError;
    let parse_error_refine = CloneParseError::Refine(
        refine_err_orig.clone(),
        PhantomData,
    );
    let cloned_refine = parse_error_refine.clone();
    assert_eq!(parse_error_refine, cloned_refine);
}

#[test]
fn test_parse_error_impl_error_where_clause() {
    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode =
        TestParseError::Decode(decode_err, PhantomData);
    assert!(parse_error_decode.source().is_some());

    let refine_err = MyRefineError;
    let parse_error_refine =
        TestParseError::Refine(refine_err, PhantomData);
    assert!(parse_error_refine.source().is_some());
}

#[test]
fn test_parse_error_impl_error_with_different_types() {
    #[derive(Debug, PartialEq, Eq, Clone, Display)]
    struct DifferentRefineError;
    impl Error for DifferentRefineError {}

    #[derive(Debug, Display)]
    struct DifferentTag;
    impl Refine<i32> for DifferentTag {
        type RefineError = DifferentRefineError;
        fn refine(
            rep: i32,
        ) -> Result<i32, Self::RefineError> {
            if rep > 100 {
                Ok(rep)
            } else {
                Err(DifferentRefineError)
            }
        }
    }

    type DifferentParseError =
        ParseError<i32, DifferentTag, DifferentTag>;

    let decode_err = "abc".parse::<i32>().unwrap_err();
    let parse_error_decode = DifferentParseError::Decode(
        decode_err,
        PhantomData,
    );
    assert!(parse_error_decode.source().is_some());

    let refine_err = DifferentRefineError;
    let parse_error_refine = DifferentParseError::Refine(
        refine_err,
        PhantomData,
    );
    assert!(parse_error_refine.source().is_some());
}

#[derive(Debug, PartialEq, Eq, Clone, Display)]
struct CommonError;
impl Error for CommonError {}

#[derive(Debug, Display)]
struct CommonTag;
impl Refine<i32> for CommonTag {
    type RefineError = CommonError;
    fn refine(rep: i32) -> Result<i32, Self::RefineError> {
        if rep > 0 { Ok(rep) } else { Err(CommonError) }
    }
}

#[derive(Debug, Display)]
struct DummyRepWithCommonErr(i32);
impl FromStr for DummyRepWithCommonErr {
    type Err = CommonError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "valid" {
            Ok(DummyRepWithCommonErr(1))
        } else {
            Err(CommonError)
        }
    }
}

impl PartialEq for DummyRepWithCommonErr {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for DummyRepWithCommonErr {}
impl Clone for DummyRepWithCommonErr {
    fn clone(&self) -> Self {
        DummyRepWithCommonErr(self.0)
    }
}

impl Refine<DummyRepWithCommonErr> for CommonTag {
    type RefineError = CommonError;
    fn refine(
        rep: DummyRepWithCommonErr,
    ) -> Result<DummyRepWithCommonErr, Self::RefineError>
    {
        Ok(rep)
    }
}

type CommonParseError =
    ParseError<DummyRepWithCommonErr, CommonTag, CommonTag>;

#[test]
fn test_parse_error_partial_eq_same_error_type_different_variants()
 {
    let decode_err = CommonError;
    let parse_error_decode =
        CommonParseError::Decode(decode_err, PhantomData);

    let refine_err = CommonError;
    let parse_error_refine =
        CommonParseError::Refine(refine_err, PhantomData);

    assert_ne!(parse_error_decode, parse_error_refine);
}
