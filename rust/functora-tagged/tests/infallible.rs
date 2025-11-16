use functora_tagged::infallible::InfallibleInto;
use std::convert::Infallible;

#[test]
fn test_infallible_into() {
    let result_ok: Result<i32, Infallible> = Ok(10);
    assert_eq!(result_ok.infallible(), 10);
}

#[test]
fn test_infallible_explicit_call() {
    let my_result: Result<u32, Infallible> = Ok(42);
    assert_eq!(my_result.infallible(), 42);
}

#[test]
fn test_infallible_trait_usage() {
    fn takes_infallible_into<R: InfallibleInto<i32>>(
        val: R,
    ) -> i32 {
        val.infallible()
    }
    let ok_result: Result<i32, Infallible> = Ok(100);
    assert_eq!(takes_infallible_into(ok_result), 100);
}

#[test]
fn test_infallible_result_construction() {
    let res: Result<String, Infallible> =
        Ok("hello".to_string());
    assert!(res.is_ok());
    assert_eq!(res.infallible(), "hello");
}

#[test]
fn test_infallible_on_ok_result() {
    let ok_result: Result<String, Infallible> =
        Ok("test".to_string());
    assert_eq!(ok_result.infallible(), "test");
}

#[test]
fn test_infallible_direct_call_on_ok() {
    let result: Result<i64, Infallible> = Ok(12345);

    assert_eq!(result.infallible(), 12345);
}

#[test]
fn test_infallible_trait_implementation() {
    let value: i32 = 50;
    let result: Result<i32, Infallible> = Ok(value);

    assert_eq!(result.infallible(), value);
}

#[test]
fn test_infallible_trait_definition_usage() {
    fn requires_infallible_into<T: InfallibleInto<i32>>(
        val: T,
    ) -> i32 {
        val.infallible()
    }

    let ok_result: Result<i32, Infallible> = Ok(200);
    assert_eq!(requires_infallible_into(ok_result), 200);
}
