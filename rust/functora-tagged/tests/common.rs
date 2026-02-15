use functora_tagged::*;

#[derive(Debug)]
struct D;

type NonEmptyVec<T> = Tagged<Vec<T>, D, FNonEmpty>;
type NonEmptyBoxSlice<T> = Tagged<Box<[T]>, D, FNonEmpty>;

#[test]
fn test_vec_i32() {
    let empty: Vec<i32> = vec![];
    let res = NonEmptyVec::new(empty);
    assert!(res.is_err());
    assert_eq!(res.unwrap_err(), NonEmptyError(vec![]));

    let non_empty = vec![1, 2, 3];
    let res = NonEmptyVec::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}

#[test]
fn test_vec_str_slice() {
    let empty: Vec<&str> = vec![];
    let res = NonEmptyVec::new(empty);
    assert!(res.is_err());
    assert_eq!(res.unwrap_err(), NonEmptyError(vec![]));

    let non_empty = vec!["a", "b", "c"];
    let res = NonEmptyVec::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}

#[test]
fn test_vec_string() {
    let empty: Vec<String> = vec![];
    let res = NonEmptyVec::new(empty);
    assert!(res.is_err());
    assert_eq!(res.unwrap_err(), NonEmptyError(vec![]));

    let non_empty = vec![
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
    ];
    let res = NonEmptyVec::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}

#[test]
fn test_box_slice_i32() {
    let empty: Box<[i32]> = vec![].into_boxed_slice();
    let res = NonEmptyBoxSlice::new(empty);
    assert!(res.is_err());
    assert_eq!(
        res.unwrap_err(),
        NonEmptyError(vec![].into_boxed_slice())
    );

    let non_empty: Box<[i32]> =
        vec![10, 20].into_boxed_slice();
    let res = NonEmptyBoxSlice::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}

#[test]
fn test_cow_slice_i32() {
    use std::borrow::Cow;
    type NonEmptyCow<'a, T> =
        Tagged<Cow<'a, [T]>, D, FNonEmpty>;

    let empty: Cow<[i32]> = Cow::Borrowed(&[]);
    let res = NonEmptyCow::new(empty);
    assert!(res.is_err());
    assert_eq!(
        res.unwrap_err(),
        NonEmptyError(Cow::Borrowed(&[] as &[i32]))
    );

    let non_empty: Cow<[i32]> = Cow::Borrowed(&[1, 2, 3]);
    let res = NonEmptyCow::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}

#[test]
fn test_string_collection() {
    type NonEmptyString = Tagged<String, D, FNonEmpty>;

    let empty = String::new();
    let res = NonEmptyString::new(empty);
    assert!(res.is_err());
    assert_eq!(
        res.unwrap_err(),
        NonEmptyError(String::new())
    );

    let non_empty = String::from("hello");
    let res = NonEmptyString::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}

#[test]
fn test_hash_set() {
    use std::collections::HashSet;
    type NonEmptySet<T> = Tagged<HashSet<T>, D, FNonEmpty>;

    let empty: HashSet<i32> = HashSet::new();
    let res = NonEmptySet::new(empty);
    assert!(res.is_err());
    assert_eq!(
        res.unwrap_err(),
        NonEmptyError(HashSet::new())
    );

    let mut non_empty = HashSet::new();
    let _ = non_empty.insert(1);
    let res = NonEmptySet::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}

#[test]
fn test_vec_deque() {
    use std::collections::VecDeque;
    type NonEmptyDeque<T> =
        Tagged<VecDeque<T>, D, FNonEmpty>;

    let empty: VecDeque<i32> = VecDeque::new();
    let res = NonEmptyDeque::new(empty);
    assert!(res.is_err());
    assert_eq!(
        res.unwrap_err(),
        NonEmptyError(VecDeque::new())
    );

    let mut non_empty = VecDeque::new();
    non_empty.push_back(1);
    let res = NonEmptyDeque::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}

#[test]
fn test_linked_list() {
    use std::collections::LinkedList;
    type NonEmptyList<T> =
        Tagged<LinkedList<T>, D, FNonEmpty>;

    let empty: LinkedList<i32> = LinkedList::new();
    let res = NonEmptyList::new(empty);
    assert!(res.is_err());
    assert_eq!(
        res.unwrap_err(),
        NonEmptyError(LinkedList::new())
    );

    let mut non_empty = LinkedList::new();
    non_empty.push_back(1);
    let res = NonEmptyList::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}
