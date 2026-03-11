use functora_tagged::*;
use rust_decimal_macros::dec;

#[derive(Debug)]
struct D;

#[test]
fn test_crude_zero() {
    let x = Tagged::<i32, D, FCrude>::zero();
    assert_eq!(*x, 0);

    let y = Tagged::<f64, D, FCrude>::zero();
    assert_eq!(*y, 0.0);
}

#[test]
fn test_crude_one() {
    let x = Tagged::<i32, D, FCrude>::one();
    assert_eq!(*x, 1);

    let y = Tagged::<f64, D, FCrude>::one();
    assert_eq!(*y, 1.0);
}

#[test]
fn test_non_neg_zero() {
    let x = Tagged::<i32, D, FNonNeg>::zero();
    assert_eq!(*x, 0);

    let y =
        Tagged::<rust_decimal::Decimal, D, FNonNeg>::zero();
    assert_eq!(*y, dec!(0));
}

#[test]
fn test_non_neg_one() {
    let x = Tagged::<i32, D, FNonNeg>::one();
    assert_eq!(*x, 1);

    let y =
        Tagged::<rust_decimal::Decimal, D, FNonNeg>::one();
    assert_eq!(*y, dec!(1));
}
