use functora_tagged::infallible::InfallibleInto;
use std::convert::Infallible;

#[test]
fn test_infallible_into() {
    let result_ok: Result<i32, Infallible> = Ok(10);
    assert_eq!(result_ok.infallible(), 10);
}
