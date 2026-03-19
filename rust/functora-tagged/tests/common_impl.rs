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
fn test_positive_one() {
    let x = Tagged::<i32, D, FPositive>::one();
    assert_eq!(*x, 1);

    let y = Tagged::<f64, D, FPositive>::one();
    assert_eq!(*y, 1.0);

    assert!(Tagged::<i32, D, FPositive>::new(0).is_err());
    assert!(Tagged::<i32, D, FPositive>::new(-1).is_err());
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

type NeVec = Tagged<Vec<i32>, D, FNonEmpty>;

#[test]
fn test_non_empty_first() {
    let xs = NeVec::new(vec![10, 20, 30]).unwrap();
    assert_eq!(xs.first(), &10);
}

#[test]
fn test_non_empty_last() {
    let xs = NeVec::new(vec![10, 20, 30]).unwrap();
    assert_eq!(xs.last(), &30);
}

#[test]
fn test_non_empty_min_by_key() {
    let xs = NeVec::new(vec![10, 5, 20]).unwrap();
    assert_eq!(xs.min_by_key(|&&x| x), &5);
}

#[test]
fn test_non_empty_max_by_key() {
    let xs = NeVec::new(vec![10, 5, 20]).unwrap();
    assert_eq!(xs.max_by_key(|&&x| x), &20);
}

#[test]
fn test_non_empty_map() {
    let xs = NeVec::new(vec![1, 2, 3]).unwrap();
    let ys: Tagged<Vec<i32>, D, FNonEmpty> =
        xs.map(|x| x * 2);
    assert_eq!(*ys, vec![2, 4, 6]);
}

#[test]
fn test_non_empty_min_max() {
    let xs = NeVec::new(vec![10, 5, 20]).unwrap();
    assert_eq!(xs.minimum(), &5);
    assert_eq!(xs.maximum(), &20);
}

#[test]
fn test_non_empty_min_max_by() {
    let xs = NeVec::new(vec![10, 5, 20]).unwrap();
    assert_eq!(xs.min_by(|a, b| a.cmp(b)), &5);
    assert_eq!(xs.max_by(|a, b| a.cmp(b)), &20);
}

#[test]
fn test_non_empty_reduce() {
    let xs = NeVec::new(vec![1, 2, 3, 4]).unwrap();
    let sum = xs.reduce(|a, b| a + b);
    assert_eq!(sum, 10);
}

#[test]
fn test_non_empty_rev() {
    let xs = NeVec::new(vec![1, 2, 3]).unwrap();
    let ys: Tagged<Vec<i32>, D, FNonEmpty> = xs.rev();
    assert_eq!(*ys, vec![3, 2, 1]);
}

#[test]
fn test_non_empty_sorted() {
    let xs = NeVec::new(vec![3, 1, 2]).unwrap();
    let ys: Tagged<Vec<i32>, D, FNonEmpty> = xs.sort();
    assert_eq!(*ys, vec![1, 2, 3]);
}

#[test]
fn test_non_empty_sorted_by_key() {
    let xs = NeVec::new(vec![1, 2, 3]).unwrap();
    let ys: Tagged<Vec<i32>, D, FNonEmpty> =
        xs.sort_by_key(|x| -x);
    assert_eq!(*ys, vec![3, 2, 1]);
}

#[test]
fn test_non_empty_dedup() {
    let xs = NeVec::new(vec![1, 1, 2, 2, 3, 1]).unwrap();
    let ys: Tagged<Vec<i32>, D, FNonEmpty> = xs.dedup();
    assert_eq!(*ys, vec![1, 2, 3, 1]);
}

#[test]
fn test_zero_excl_to_one_excl() {
    assert!(
        Tagged::<f64, D, FZeroExclToOneExcl>::new(0.5)
            .is_ok()
    );
    assert!(
        Tagged::<f64, D, FZeroExclToOneExcl>::new(0.0)
            .is_err()
    );
    assert!(
        Tagged::<f64, D, FZeroExclToOneExcl>::new(1.0)
            .is_err()
    );
    assert!(
        Tagged::<f64, D, FZeroExclToOneExcl>::new(-0.1)
            .is_err()
    );
    assert!(
        Tagged::<f64, D, FZeroExclToOneExcl>::new(1.1)
            .is_err()
    );
}

#[test]
fn test_zero_incl_to_one_excl() {
    assert!(
        Tagged::<f64, D, FZeroInclToOneExcl>::new(0.0)
            .is_ok()
    );
    assert!(
        Tagged::<f64, D, FZeroInclToOneExcl>::new(0.5)
            .is_ok()
    );
    assert!(
        Tagged::<f64, D, FZeroInclToOneExcl>::new(1.0)
            .is_err()
    );
    assert!(
        Tagged::<f64, D, FZeroInclToOneExcl>::new(-0.1)
            .is_err()
    );
    assert!(
        Tagged::<f64, D, FZeroInclToOneExcl>::new(1.1)
            .is_err()
    );
}

#[test]
fn test_zero_excl_to_one_incl() {
    assert!(
        Tagged::<f64, D, FZeroExclToOneIncl>::new(1.0)
            .is_ok()
    );
    assert!(
        Tagged::<f64, D, FZeroExclToOneIncl>::new(0.5)
            .is_ok()
    );
    assert!(
        Tagged::<f64, D, FZeroExclToOneIncl>::new(0.0)
            .is_err()
    );
    assert!(
        Tagged::<f64, D, FZeroExclToOneIncl>::new(-0.1)
            .is_err()
    );
    assert!(
        Tagged::<f64, D, FZeroExclToOneIncl>::new(1.1)
            .is_err()
    );
}

#[test]
fn test_zero_incl_to_one_incl() {
    assert!(
        Tagged::<f64, D, FZeroInclToOneIncl>::new(0.0)
            .is_ok()
    );
    assert!(
        Tagged::<f64, D, FZeroInclToOneIncl>::new(1.0)
            .is_ok()
    );
    assert!(
        Tagged::<f64, D, FZeroInclToOneIncl>::new(0.5)
            .is_ok()
    );
    assert!(
        Tagged::<f64, D, FZeroInclToOneIncl>::new(-0.1)
            .is_err()
    );
    assert!(
        Tagged::<f64, D, FZeroInclToOneIncl>::new(1.1)
            .is_err()
    );
}
