use functora_tagged::*;
use std::ops::Deref;

//
// Construction & validation
//

#[test]
fn test_nonempty_slice_ref_empty_rejected() {
    let empty: &[i32] = &[];
    assert!(NonEmpty::<&[i32]>::new(empty).is_err());
}

#[test]
fn test_nonempty_str_ref_empty_rejected() {
    let empty: &str = "";
    assert!(NonEmpty::<&str>::new(empty).is_err());
}

#[test]
fn test_nonempty_slice_ref_success() {
    let slice: &[i32] = &[1, 2, 3];
    let xs = NonEmpty::<&[i32]>::new(slice).unwrap();
    assert_eq!(**xs, [1, 2, 3]);
    assert_eq!(xs.length(), 3);
    assert!(!xs.zero_length());
}

#[test]
fn test_nonempty_str_ref_success() {
    let s: &str = "hello";
    let xs = NonEmpty::<&str>::new(s).unwrap();
    assert_eq!(&**xs, "hello");
    assert_eq!(xs.length(), 5);
    assert!(!xs.zero_length());
}

#[test]
fn test_nonempty_slice_ref_single() {
    let slice: &[i32] = &[42];
    let xs = NonEmpty::<&[i32]>::new(slice).unwrap();
    assert_eq!(xs.length(), 1);
    assert_eq!((*xs)[0], 42);
}

#[test]
fn test_nonempty_str_ref_single_char() {
    let s: &str = "x";
    let xs = NonEmpty::<&str>::new(s).unwrap();
    assert_eq!(xs.length(), 1);
    assert_eq!(&**xs, "x");
}

//
// &Vec<A> via deref coercion
//

#[test]
fn test_nonempty_vec_ref_empty_rejected() {
    let vec: Vec<i32> = vec![];
    assert!(NonEmpty::<&[i32]>::new(&vec).is_err());
}

#[test]
fn test_nonempty_vec_ref_success() {
    let vec: Vec<i32> = vec![1, 2, 3];
    let xs = NonEmpty::<&[i32]>::new(&vec).unwrap();
    assert_eq!(xs.length(), 3);
    assert_eq!(**xs, [1, 2, 3]);
}

//
// ref_first
//

#[test]
fn test_nonempty_slice_ref_first() {
    let xs =
        NonEmpty::<&[i32]>::new(&[10, 20, 30][..]).unwrap();
    assert_eq!(xs.ref_first(), &10);
}

#[test]
fn test_nonempty_slice_ref_first_single() {
    let xs = NonEmpty::<&[i32]>::new(&[42][..]).unwrap();
    assert_eq!(xs.ref_first(), &42);
}

//
// ref_last
//

#[test]
fn test_nonempty_slice_ref_last() {
    let xs =
        NonEmpty::<&[i32]>::new(&[10, 20, 30][..]).unwrap();
    assert_eq!(xs.ref_last(), &30);
}

#[test]
fn test_nonempty_slice_ref_last_single() {
    let xs = NonEmpty::<&[i32]>::new(&[42][..]).unwrap();
    assert_eq!(xs.ref_last(), &42);
}

//
// ref_iter
//

#[test]
fn test_nonempty_slice_ref_iter() {
    let xs =
        NonEmpty::<&[i32]>::new(&[10, 20, 30][..]).unwrap();
    let collected: Vec<&i32> = xs.ref_iter().collect();
    assert_eq!(collected, vec![&10, &20, &30]);
}

#[test]
fn test_nonempty_slice_ref_iter_single() {
    let xs = NonEmpty::<&[i32]>::new(&[77][..]).unwrap();
    let collected: Vec<&i32> = xs.ref_iter().collect();
    assert_eq!(collected, vec![&77]);
}

#[test]
fn test_nonempty_slice_ref_iter_sum() {
    let xs =
        NonEmpty::<&[i32]>::new(&[1, 2, 3, 4][..]).unwrap();
    let sum: i32 = xs.ref_iter().sum();
    assert_eq!(sum, 10);
}

//
// ref_minimum / ref_maximum
//

#[test]
fn test_nonempty_slice_ref_minimum() {
    let xs =
        NonEmpty::<&[i32]>::new(&[30, 10, 20][..]).unwrap();
    assert_eq!(xs.ref_minimum(), &10);
}

#[test]
fn test_nonempty_slice_ref_maximum() {
    let xs =
        NonEmpty::<&[i32]>::new(&[10, 30, 20][..]).unwrap();
    assert_eq!(xs.ref_maximum(), &30);
}

#[test]
fn test_nonempty_slice_ref_min_max_single() {
    let xs = NonEmpty::<&[i32]>::new(&[99][..]).unwrap();
    assert_eq!(xs.ref_minimum(), &99);
    assert_eq!(xs.ref_maximum(), &99);
}

//
// ref_min_by / ref_max_by
//

#[test]
fn test_nonempty_slice_ref_min_by() {
    let xs =
        NonEmpty::<&[i32]>::new(&[30, 10, 20][..]).unwrap();
    assert_eq!(xs.ref_min_by(Ord::cmp), &10);
}

#[test]
fn test_nonempty_slice_ref_max_by() {
    let xs =
        NonEmpty::<&[i32]>::new(&[10, 30, 20][..]).unwrap();
    assert_eq!(xs.ref_max_by(Ord::cmp), &30);
}

#[test]
fn test_nonempty_slice_ref_min_by_single() {
    let xs = NonEmpty::<&[i32]>::new(&[55][..]).unwrap();
    assert_eq!(xs.ref_min_by(|a, b| a.cmp(b)), &55);
}

//
// ref_min_by_key / ref_max_by_key
//

#[test]
fn test_nonempty_slice_ref_min_by_key() {
    let xs =
        NonEmpty::<&[i32]>::new(&[100, 1, 10][..]).unwrap();
    assert_eq!(xs.ref_min_by_key(|x| *x), &1);
}

#[test]
fn test_nonempty_slice_ref_max_by_key() {
    let xs =
        NonEmpty::<&[i32]>::new(&[1, 100, 10][..]).unwrap();
    assert_eq!(xs.ref_max_by_key(|x| *x), &100);
}

#[test]
fn test_nonempty_slice_ref_min_by_key_single() {
    let xs = NonEmpty::<&[i32]>::new(&[42][..]).unwrap();
    assert_eq!(xs.ref_min_by_key(|x| *x), &42);
}

//
// &str operations via Deref (chars, bytes — not IntoIterator)
//

#[test]
fn test_nonempty_str_ref_first_char() {
    let xs = NonEmpty::<&str>::new("xyz").unwrap();
    assert_eq!(xs.chars().next(), Some('x'));
}

#[test]
fn test_nonempty_str_ref_last_char() {
    let xs = NonEmpty::<&str>::new("xyz").unwrap();
    assert_eq!(xs.chars().next_back(), Some('z'));
}

#[test]
fn test_nonempty_str_ref_min_char() {
    let xs = NonEmpty::<&str>::new("cab").unwrap();
    assert_eq!(xs.chars().min(), Some('a'));
}

#[test]
fn test_nonempty_str_ref_max_char() {
    let xs = NonEmpty::<&str>::new("cab").unwrap();
    assert_eq!(xs.chars().max(), Some('c'));
}

#[test]
fn test_nonempty_str_ref_iter_chars() {
    let xs = NonEmpty::<&str>::new("abc").unwrap();
    let collected: String = xs.chars().collect();
    assert_eq!(collected, "abc");
}

#[test]
fn test_nonempty_str_ref_rev_chars() {
    let xs = NonEmpty::<&str>::new("rust").unwrap();
    let result: String = xs.chars().rev().collect();
    assert_eq!(result, "tsur");
}

//
// No allocation / no clone verification
//

#[test]
fn test_nonempty_slice_ref_no_allocation() {
    let slice: &[i32] = &[1, 2, 3];
    let xs = NonEmpty::<&[i32]>::new(slice).unwrap();
    let _: &NonEmpty<&[i32]> = &xs;
    assert_eq!(xs.length(), 3);
}

#[test]
fn test_nonempty_str_ref_no_allocation() {
    let s: &str = "hello";
    let xs = NonEmpty::<&str>::new(s).unwrap();
    let _: &NonEmpty<&str> = &xs;
    assert_eq!(xs.length(), 5);
}

#[test]
fn test_nonempty_slice_ref_copy() {
    let slice: &[i32] = &[1, 2, 3];
    let xs = NonEmpty::<&[i32]>::new(slice).unwrap();
    let ys = xs;
    assert_eq!(xs.length(), 3);
    assert_eq!(ys.length(), 3);
}

#[test]
fn test_nonempty_str_ref_copy() {
    let s: &str = "hello";
    let xs = NonEmpty::<&str>::new(s).unwrap();
    let ys = xs;
    assert_eq!(xs.length(), 5);
    assert_eq!(ys.length(), 5);
}

//
// Deref-based operations
//

#[test]
fn test_nonempty_slice_ref_indexing() {
    let xs =
        NonEmpty::<&[i32]>::new(&[7, 8, 9][..]).unwrap();
    assert_eq!(xs[0], 7);
    assert_eq!(xs[1], 8);
    assert_eq!(xs[2], 9);
}

#[test]
fn test_nonempty_str_ref_indexing() {
    let xs = NonEmpty::<&str>::new("hi!").unwrap();
    assert_eq!(&xs[0..2], "hi");
}

#[test]
fn test_nonempty_slice_ref_preserves_pointer() {
    let array = [1, 2, 3, 4, 5];
    let slice: &[i32] = &array;
    let xs = NonEmpty::<&[i32]>::new(slice).unwrap();
    assert_eq!(xs.rep(), &[1, 2, 3, 4, 5]);
}

#[test]
fn test_nonempty_slice_ref_different_types() {
    let slice_f64: &[f64] = &[1.5, 2.5, 3.5];
    let xs = NonEmpty::<&[f64]>::new(slice_f64).unwrap();
    assert_eq!(xs.length(), 3);

    let slice_str: &[&str] = &["a", "bb", "ccc"];
    let ys = NonEmpty::<&[&str]>::new(slice_str).unwrap();
    assert_eq!(ys.length(), 3);
}

#[test]
fn test_nonempty_slice_ref_large() {
    let large: Vec<i32> = (0..1000).collect();
    let slice: &[i32] = &large;
    let xs = NonEmpty::<&[i32]>::new(slice).unwrap();
    assert_eq!(xs.length(), 1000);
    assert_eq!(&xs[0], &0);
    assert_eq!(&xs[999], &999);
    assert_eq!(xs.ref_first(), &0);
    assert_eq!(xs.ref_last(), &999);
    assert_eq!(xs.ref_minimum(), &0);
    assert_eq!(xs.ref_maximum(), &999);
    let sum: i32 = xs.ref_iter().sum();
    assert_eq!(sum, 499500);
}

#[test]
fn test_nonempty_slice_ref_complex_type() {
    let vec: Vec<String> = vec![
        "alpha".to_string(),
        "beta".to_string(),
        "gamma".to_string(),
    ];
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    assert_eq!(xs.length(), 3);
    assert_eq!(&xs[0], &"alpha".to_string());
    assert_eq!(&xs[2], &"gamma".to_string());
    assert_eq!(xs.ref_first(), &"alpha".to_string());
    assert_eq!(xs.ref_last(), &"gamma".to_string());
}

//
// Display / Debug
//

#[test]
fn test_nonempty_slice_ref_display() {
    let xs =
        NonEmpty::<&[i32]>::new(&[1, 2, 3][..]).unwrap();
    assert!(format!("{xs:?}").contains("[1, 2, 3]"));
}

#[test]
fn test_nonempty_str_ref_display() {
    let xs = NonEmpty::<&str>::new("world").unwrap();
    assert_eq!(format!("{xs}"), "world");
}

//
// Custom struct
//

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

#[test]
fn test_nonempty_vec_ref_custom_struct_empty_rejected() {
    let vec: Vec<Point> = vec![];
    assert!(NonEmpty::<&[Point]>::new(&vec).is_err());
}

#[test]
fn test_nonempty_vec_ref_custom_struct_success() {
    let vec = vec![
        Point::new(1, 2),
        Point::new(3, 4),
        Point::new(5, 6),
    ];
    let xs = NonEmpty::<&[Point]>::new(&vec).unwrap();
    assert_eq!(xs.length(), 3);
    assert_eq!(xs.ref_first(), &Point::new(1, 2));
    assert_eq!(xs.ref_last(), &Point::new(5, 6));
}

#[test]
fn test_nonempty_vec_ref_custom_struct_min_max() {
    let vec = vec![
        Point::new(3, 0),
        Point::new(1, 9),
        Point::new(2, 5),
    ];
    let xs = NonEmpty::<&[Point]>::new(&vec).unwrap();
    assert_eq!(xs.ref_minimum(), &Point::new(1, 9));
    assert_eq!(xs.ref_maximum(), &Point::new(3, 0));
}

#[test]
fn test_nonempty_vec_ref_custom_struct_min_by_key() {
    let vec = vec![
        Point::new(3, 0),
        Point::new(1, 9),
        Point::new(2, 5),
    ];
    let xs = NonEmpty::<&[Point]>::new(&vec).unwrap();
    assert_eq!(
        xs.ref_min_by_key(|p| p.x),
        &Point::new(1, 9)
    );
    assert_eq!(
        xs.ref_max_by_key(|p| p.x),
        &Point::new(3, 0)
    );
    assert_eq!(
        xs.ref_min_by_key(|p| p.y),
        &Point::new(3, 0)
    );
    assert_eq!(
        xs.ref_max_by_key(|p| p.y),
        &Point::new(1, 9)
    );
}

#[test]
fn test_nonempty_vec_ref_custom_struct_iter() {
    let vec = vec![
        Point::new(1, 0),
        Point::new(2, 0),
        Point::new(3, 0),
    ];
    let xs = NonEmpty::<&[Point]>::new(&vec).unwrap();
    let x_sum: i32 = xs.ref_iter().map(|p| p.x).sum();
    assert_eq!(x_sum, 6);
}

#[test]
fn test_nonempty_vec_ref_custom_struct_indexing() {
    let vec = vec![Point::new(10, 20), Point::new(30, 40)];
    let xs = NonEmpty::<&[Point]>::new(&vec).unwrap();
    assert_eq!(xs[0].x, 10);
    assert_eq!(xs[0].y, 20);
    assert_eq!(xs[1].x, 30);
    assert_eq!(xs[1].y, 40);
}

#[test]
fn test_nonempty_vec_ref_custom_struct_single() {
    let vec = vec![Point::new(99, 88)];
    let xs = NonEmpty::<&[Point]>::new(&vec).unwrap();
    assert_eq!(xs.length(), 1);
    assert_eq!(xs.ref_first(), &Point::new(99, 88));
    assert_eq!(xs.ref_last(), &Point::new(99, 88));
    assert_eq!(xs.ref_minimum(), &Point::new(99, 88));
    assert_eq!(xs.ref_maximum(), &Point::new(99, 88));
}

//
// Custom enum
//

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy,
)]
enum Status {
    Active,
    Inactive,
    Pending,
}

#[test]
fn test_nonempty_vec_ref_enum_empty_rejected() {
    let vec: Vec<Status> = vec![];
    assert!(NonEmpty::<&[Status]>::new(&vec).is_err());
}

#[test]
fn test_nonempty_vec_ref_enum_success() {
    let vec = vec![
        Status::Active,
        Status::Pending,
        Status::Inactive,
    ];
    let xs = NonEmpty::<&[Status]>::new(&vec).unwrap();
    assert_eq!(xs.length(), 3);
    assert_eq!(xs.ref_first(), &Status::Active);
    assert_eq!(xs.ref_last(), &Status::Inactive);
}

#[test]
fn test_nonempty_vec_ref_enum_min_max() {
    let vec = vec![
        Status::Pending,
        Status::Active,
        Status::Inactive,
    ];
    let xs = NonEmpty::<&[Status]>::new(&vec).unwrap();
    assert_eq!(xs.ref_minimum(), &Status::Active);
    assert_eq!(xs.ref_maximum(), &Status::Pending);
}

#[test]
fn test_nonempty_vec_ref_enum_iter() {
    let vec = vec![
        Status::Active,
        Status::Active,
        Status::Inactive,
    ];
    let xs = NonEmpty::<&[Status]>::new(&vec).unwrap();
    let active_count = xs
        .ref_iter()
        .filter(|s| **s == Status::Active)
        .count();
    assert_eq!(active_count, 2);
}

#[test]
fn test_nonempty_vec_ref_enum_min_by() {
    let vec = vec![
        Status::Pending,
        Status::Active,
        Status::Inactive,
    ];
    let xs = NonEmpty::<&[Status]>::new(&vec).unwrap();
    assert_eq!(xs.ref_min_by(Ord::cmp), &Status::Active);
    assert_eq!(xs.ref_max_by(Ord::cmp), &Status::Pending);
}

#[test]
fn test_nonempty_vec_ref_enum_single() {
    let vec = vec![Status::Pending];
    let xs = NonEmpty::<&[Status]>::new(&vec).unwrap();
    assert_eq!(xs.length(), 1);
    assert_eq!(xs.ref_first(), &Status::Pending);
    assert_eq!(xs.ref_last(), &Status::Pending);
}

//
// NonEmpty<&Vec<String>>
//

#[test]
fn test_nonempty_vec_ref_string_empty_rejected() {
    let vec: Vec<String> = vec![];
    assert!(NonEmpty::<&[String]>::new(&vec).is_err());
}

#[test]
fn test_nonempty_vec_ref_string_first_last() {
    let vec = vec![
        "alpha".to_string(),
        "beta".to_string(),
        "gamma".to_string(),
    ];
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    assert_eq!(xs.ref_first(), &"alpha".to_string());
    assert_eq!(xs.ref_last(), &"gamma".to_string());
}

#[test]
fn test_nonempty_vec_ref_string_minimum() {
    let vec = vec![
        "cherry".to_string(),
        "apple".to_string(),
        "banana".to_string(),
    ];
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    assert_eq!(xs.ref_minimum(), &"apple".to_string());
}

#[test]
fn test_nonempty_vec_ref_string_maximum() {
    let vec = vec![
        "cherry".to_string(),
        "apple".to_string(),
        "banana".to_string(),
    ];
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    assert_eq!(xs.ref_maximum(), &"cherry".to_string());
}

#[test]
fn test_nonempty_vec_ref_string_min_by_key() {
    let vec = vec![
        "abcd".to_string(),
        "ef".to_string(),
        "ghi".to_string(),
    ];
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    assert_eq!(
        xs.ref_min_by_key(|s| s.len()),
        &"ef".to_string()
    );
    assert_eq!(
        xs.ref_max_by_key(|s| s.len()),
        &"abcd".to_string()
    );
}

#[test]
fn test_nonempty_vec_ref_string_min_by() {
    let vec = vec![
        "zzz".to_string(),
        "aaa".to_string(),
        "mmm".to_string(),
    ];
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    assert_eq!(
        xs.ref_min_by(|a, b| a.cmp(b)),
        &"aaa".to_string()
    );
    assert_eq!(
        xs.ref_max_by(|a, b| a.cmp(b)),
        &"zzz".to_string()
    );
}

#[test]
fn test_nonempty_vec_ref_string_iter() {
    let vec =
        vec!["hello".to_string(), "world".to_string()];
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    let total_len: usize =
        xs.ref_iter().map(|s| s.len()).sum();
    assert_eq!(total_len, 10);
}

#[test]
fn test_nonempty_vec_ref_string_single() {
    let vec = vec!["solo".to_string()];
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    assert_eq!(xs.length(), 1);
    assert_eq!(xs.ref_first(), &"solo".to_string());
    assert_eq!(xs.ref_last(), &"solo".to_string());
    assert_eq!(xs.ref_minimum(), &"solo".to_string());
    assert_eq!(xs.ref_maximum(), &"solo".to_string());
}

#[test]
fn test_nonempty_vec_ref_string_indexing() {
    let vec = vec![
        "zero".to_string(),
        "one".to_string(),
        "two".to_string(),
    ];
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    assert_eq!(xs[0], "zero");
    assert_eq!(xs[1], "one");
    assert_eq!(xs[2], "two");
}

#[test]
fn test_nonempty_vec_ref_string_large() {
    let vec: Vec<String> =
        (0..100).map(|i| format!("item_{i}")).collect();
    let xs = NonEmpty::<&[String]>::new(&vec).unwrap();
    assert_eq!(xs.length(), 100);
    assert_eq!(xs.ref_first(), &"item_0".to_string());
    assert_eq!(xs.ref_last(), &"item_99".to_string());
}

//
// Ensure Deref is satisfied (compile-time check)
//

#[test]
fn test_nonempty_slice_ref_impl_deref() {
    let xs =
        NonEmpty::<&[i32]>::new(&[1, 2, 3][..]).unwrap();
    let _: &[i32] = xs.deref();
}

#[test]
fn test_nonempty_str_ref_impl_deref() {
    let xs = NonEmpty::<&str>::new("hello").unwrap();
    let _: &str = xs.deref();
}
