use functora_tagged::*;
use rust_decimal_macros::dec;
use std::collections::{
    BTreeMap, BTreeSet, HashMap, HashSet, VecDeque,
};

#[derive(Debug)]
struct D;

#[test]
fn test_crude_zero() {
    let x = Tagged::<i32, D, FCrude>::zero();
    assert_eq!(*x, 0);

    let y = Tagged::<f64, D, FCrude>::zero();
    assert!((*y - 0.0).abs() < f64::EPSILON);
}

#[test]
fn test_crude_one() {
    let x = Tagged::<i32, D, FCrude>::one();
    assert_eq!(*x, 1);

    let y = Tagged::<f64, D, FCrude>::one();
    assert!((*y - 1.0).abs() < f64::EPSILON);
}

#[test]
fn test_positive_one() {
    let x = Tagged::<i32, D, FPositive>::one();
    assert_eq!(*x, 1);

    let y = Tagged::<f64, D, FPositive>::one();
    assert!((*y - 1.0).abs() < f64::EPSILON);

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
    assert_eq!(xs.min_by_key(|&x| x), &5);
}

#[test]
fn test_non_empty_max_by_key() {
    let xs = NeVec::new(vec![10, 5, 20]).unwrap();
    assert_eq!(xs.max_by_key(|&x| x), &20);
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
    assert_eq!(xs.min_by(Ord::cmp), &5);
    assert_eq!(xs.max_by(Ord::cmp), &20);
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

#[test]
fn test_zero_incl_to_one_excl_zero() {
    let x = Tagged::<f64, D, FZeroInclToOneExcl>::zero();
    assert!((*x - 0.0).abs() < f64::EPSILON);
}

#[test]
fn test_zero_excl_to_one_incl_one() {
    let x = Tagged::<f64, D, FZeroExclToOneIncl>::one();
    assert!((*x - 1.0).abs() < f64::EPSILON);
}

#[test]
fn test_zero_incl_to_one_incl_zero_one() {
    let x = Tagged::<f64, D, FZeroInclToOneIncl>::zero();
    assert!((*x - 0.0).abs() < f64::EPSILON);

    let y = Tagged::<f64, D, FZeroInclToOneIncl>::one();
    assert!((*y - 1.0).abs() < f64::EPSILON);
}

//
// Non-Empty HashMap tests
//

type NeHashMap =
    Tagged<HashMap<i32, &'static str>, D, FNonEmpty>;

#[test]
fn test_non_empty_hashmap_first() {
    let mut map = HashMap::new();
    let _ = map.insert(1, "one");
    let _ = map.insert(2, "two");
    let _ = map.insert(3, "three");
    let xs = NeHashMap::new(map).unwrap();
    let first = xs.first();
    assert!(*first.0 >= 1 && *first.0 <= 3);
}

#[test]
fn test_non_empty_hashmap_minimum() {
    let mut map = HashMap::new();
    let _ = map.insert(10, "ten");
    let _ = map.insert(5, "five");
    let _ = map.insert(20, "twenty");
    let xs = NeHashMap::new(map).unwrap();
    assert_eq!(*xs.minimum().0, 5);
}

#[test]
fn test_non_empty_hashmap_maximum() {
    let mut map = HashMap::new();
    let _ = map.insert(10, "ten");
    let _ = map.insert(5, "five");
    let _ = map.insert(20, "twenty");
    let xs = NeHashMap::new(map).unwrap();
    assert_eq!(*xs.maximum().0, 20);
}

#[test]
fn test_non_empty_hashmap_min_by() {
    let mut map = HashMap::new();
    let _ = map.insert(10, "ten");
    let _ = map.insert(5, "five");
    let _ = map.insert(20, "twenty");
    let xs = NeHashMap::new(map).unwrap();
    assert_eq!(*xs.min_by(|a, b| a.0.cmp(b.0)).0, 5);
}

#[test]
fn test_non_empty_hashmap_max_by() {
    let mut map = HashMap::new();
    let _ = map.insert(10, "ten");
    let _ = map.insert(5, "five");
    let _ = map.insert(20, "twenty");
    let xs = NeHashMap::new(map).unwrap();
    assert_eq!(*xs.max_by(|a, b| a.0.cmp(b.0)).0, 20);
}

#[test]
fn test_non_empty_hashmap_min_by_key() {
    let mut map = HashMap::new();
    let _ = map.insert(10, "ten");
    let _ = map.insert(5, "five");
    let _ = map.insert(20, "twenty");
    let xs = NeHashMap::new(map).unwrap();
    assert_eq!(*xs.min_by_key(|x| x.0).0, 5);
}

#[test]
fn test_non_empty_hashmap_max_by_key() {
    let mut map = HashMap::new();
    let _ = map.insert(10, "ten");
    let _ = map.insert(5, "five");
    let _ = map.insert(20, "twenty");
    let xs = NeHashMap::new(map).unwrap();
    assert_eq!(*xs.max_by_key(|x| x.0).0, 20);
}

#[test]
fn test_non_empty_hashmap_reduce() {
    let mut map = HashMap::new();
    let _ = map.insert(1, "one");
    let _ = map.insert(2, "two");
    let _ = map.insert(3, "three");
    let xs = NeHashMap::new(map).unwrap();
    let sum = xs.reduce(|acc, x| (acc.0 + x.0, acc.1));
    assert_eq!(sum.0, 6);
}

//
// Non-Empty HashSet tests
//

type NeHashSet = Tagged<HashSet<i32>, D, FNonEmpty>;

#[test]
fn test_non_empty_hashset_first() {
    let mut set = HashSet::new();
    let _ = set.insert(10);
    let _ = set.insert(20);
    let _ = set.insert(30);
    let xs = NeHashSet::new(set).unwrap();
    let first = xs.first();
    assert!(*first == 10 || *first == 20 || *first == 30);
}

#[test]
fn test_non_empty_hashset_minimum() {
    let mut set = HashSet::new();
    let _ = set.insert(10);
    let _ = set.insert(5);
    let _ = set.insert(20);
    let xs = NeHashSet::new(set).unwrap();
    assert_eq!(xs.minimum(), &5);
}

#[test]
fn test_non_empty_hashset_maximum() {
    let mut set = HashSet::new();
    let _ = set.insert(10);
    let _ = set.insert(5);
    let _ = set.insert(20);
    let xs = NeHashSet::new(set).unwrap();
    assert_eq!(xs.maximum(), &20);
}

#[test]
fn test_non_empty_hashset_min_by() {
    let mut set = HashSet::new();
    let _ = set.insert(10);
    let _ = set.insert(5);
    let _ = set.insert(20);
    let xs = NeHashSet::new(set).unwrap();
    assert_eq!(xs.min_by(Ord::cmp), &5);
}

#[test]
fn test_non_empty_hashset_max_by() {
    let mut set = HashSet::new();
    let _ = set.insert(10);
    let _ = set.insert(5);
    let _ = set.insert(20);
    let xs = NeHashSet::new(set).unwrap();
    assert_eq!(xs.max_by(Ord::cmp), &20);
}

#[test]
fn test_non_empty_hashset_reduce() {
    let mut set = HashSet::new();
    let _ = set.insert(1);
    let _ = set.insert(2);
    let _ = set.insert(3);
    let _ = set.insert(4);
    let xs = NeHashSet::new(set).unwrap();
    let sum = xs.reduce(|a, b| a + b);
    assert_eq!(sum, 10);
}

//
// Non-Empty BTreeMap tests
//

type NeBTreeMap =
    Tagged<BTreeMap<i32, &'static str>, D, FNonEmpty>;

#[test]
fn test_non_empty_btreenmap_first() {
    let mut map = BTreeMap::new();
    let _ = map.insert(1, "one");
    let _ = map.insert(2, "two");
    let _ = map.insert(3, "three");
    let xs = NeBTreeMap::new(map).unwrap();
    assert_eq!(*xs.first().0, 1);
}

#[test]
fn test_non_empty_btreenmap_last() {
    let mut map = BTreeMap::new();
    let _ = map.insert(1, "one");
    let _ = map.insert(2, "two");
    let _ = map.insert(3, "three");
    let xs = NeBTreeMap::new(map).unwrap();
    assert_eq!(*xs.last().0, 3);
}

#[test]
fn test_non_empty_btreenmap_minimum() {
    let mut map = BTreeMap::new();
    let _ = map.insert(10, "ten");
    let _ = map.insert(5, "five");
    let _ = map.insert(20, "twenty");
    let xs = NeBTreeMap::new(map).unwrap();
    assert_eq!(*xs.minimum().0, 5);
}

#[test]
fn test_non_empty_btreenmap_maximum() {
    let mut map = BTreeMap::new();
    let _ = map.insert(10, "ten");
    let _ = map.insert(5, "five");
    let _ = map.insert(20, "twenty");
    let xs = NeBTreeMap::new(map).unwrap();
    assert_eq!(*xs.maximum().0, 20);
}

#[test]
fn test_non_empty_btreenmap_reduce() {
    let mut map = BTreeMap::new();
    let _ = map.insert(1, "one");
    let _ = map.insert(2, "two");
    let _ = map.insert(3, "three");
    let xs = NeBTreeMap::new(map).unwrap();
    let sum = xs.reduce(|acc, x| (acc.0 + x.0, acc.1));
    assert_eq!(sum.0, 6);
}

//
// Non-Empty BTreeSet tests
//

type NeBTreeSet = Tagged<BTreeSet<i32>, D, FNonEmpty>;

#[test]
fn test_non_empty_btreeset_first() {
    let mut set = BTreeSet::new();
    let _ = set.insert(10);
    let _ = set.insert(20);
    let _ = set.insert(30);
    let xs = NeBTreeSet::new(set).unwrap();
    assert_eq!(xs.first(), &10);
}

#[test]
fn test_non_empty_btreeset_last() {
    let mut set = BTreeSet::new();
    let _ = set.insert(10);
    let _ = set.insert(20);
    let _ = set.insert(30);
    let xs = NeBTreeSet::new(set).unwrap();
    assert_eq!(xs.last(), &30);
}

#[test]
fn test_non_empty_btreeset_minimum() {
    let mut set = BTreeSet::new();
    let _ = set.insert(10);
    let _ = set.insert(5);
    let _ = set.insert(20);
    let xs = NeBTreeSet::new(set).unwrap();
    assert_eq!(xs.minimum(), &5);
}

#[test]
fn test_non_empty_btreeset_maximum() {
    let mut set = BTreeSet::new();
    let _ = set.insert(10);
    let _ = set.insert(5);
    let _ = set.insert(20);
    let xs = NeBTreeSet::new(set).unwrap();
    assert_eq!(xs.maximum(), &20);
}

#[test]
fn test_non_empty_btreeset_reduce() {
    let mut set = BTreeSet::new();
    let _ = set.insert(1);
    let _ = set.insert(2);
    let _ = set.insert(3);
    let _ = set.insert(4);
    let xs = NeBTreeSet::new(set).unwrap();
    let sum = xs.reduce(|a, b| a + b);
    assert_eq!(sum, 10);
}

//
// Non-Empty VecDeque tests
//

type NeVecDeque = Tagged<VecDeque<i32>, D, FNonEmpty>;

#[test]
fn test_non_empty_vecdeque_first() {
    let mut dq = VecDeque::new();
    dq.push_back(10);
    dq.push_back(20);
    dq.push_back(30);
    let xs = NeVecDeque::new(dq).unwrap();
    assert_eq!(xs.first(), &10);
}

#[test]
fn test_non_empty_vecdeque_last() {
    let mut dq = VecDeque::new();
    dq.push_back(10);
    dq.push_back(20);
    dq.push_back(30);
    let xs = NeVecDeque::new(dq).unwrap();
    assert_eq!(xs.last(), &30);
}

#[test]
fn test_non_empty_vecdeque_minimum() {
    let mut dq = VecDeque::new();
    dq.push_back(10);
    dq.push_back(5);
    dq.push_back(20);
    let xs = NeVecDeque::new(dq).unwrap();
    assert_eq!(xs.minimum(), &5);
}

#[test]
fn test_non_empty_vecdeque_maximum() {
    let mut dq = VecDeque::new();
    dq.push_back(10);
    dq.push_back(5);
    dq.push_back(20);
    let xs = NeVecDeque::new(dq).unwrap();
    assert_eq!(xs.maximum(), &20);
}

#[test]
fn test_non_empty_vecdeque_reduce() {
    let mut dq = VecDeque::new();
    dq.push_back(1);
    dq.push_back(2);
    dq.push_back(3);
    dq.push_back(4);
    let xs = NeVecDeque::new(dq).unwrap();
    let sum = xs.reduce(|a, b| a + b);
    assert_eq!(sum, 10);
}

//
// Non-Empty convert tests
//

#[test]
fn test_non_empty_convert_vec_to_hashmap() {
    let pairs = NonEmpty::new(vec![
        (1, "one"),
        (2, "two"),
        (3, "three"),
    ])
    .unwrap();
    let map: NonEmpty<HashMap<i32, &'static str>> =
        pairs.convert(|v| v.into_iter().collect());
    assert_eq!(map.length(), 3);
    assert_eq!(map.get(&1), Some(&"one"));
    assert_eq!(map.get(&2), Some(&"two"));
    assert_eq!(map.get(&3), Some(&"three"));
}

#[test]
fn test_non_empty_convert_vec_to_btreeset() {
    let xs = NonEmpty::new(vec![3, 1, 2]).unwrap();
    let set: NonEmpty<BTreeSet<i32>> =
        xs.convert(|v| v.into_iter().collect());
    assert_eq!(set.length(), 3);
    assert_eq!(set.first(), &1);
    assert_eq!(set.last(), &3);
}

#[test]
fn test_non_empty_convert_hashmap_to_vec() {
    let mut map = HashMap::new();
    let _ = map.insert(1, "one");
    let _ = map.insert(2, "two");
    let ne_map = NonEmpty::new(map).unwrap();
    let vec: NonEmpty<Vec<(i32, &'static str)>> =
        ne_map.convert(|m| m.into_iter().collect());
    assert_eq!(vec.length(), 2);
}

#[test]
fn test_non_empty_convert_preserves_nonempty() {
    let xs = NonEmpty::new(vec![1]).unwrap();
    let set: NonEmpty<BTreeSet<i32>> =
        xs.convert(|v| v.into_iter().collect());
    assert_eq!(set.length(), 1);
    assert!(set.contains(&1));
}
