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
    assert_eq!(
        res.unwrap_err(),
        NonEmptyError(PhantomData)
    );

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
    assert_eq!(
        res.unwrap_err(),
        NonEmptyError(PhantomData)
    );

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
    assert_eq!(
        res.unwrap_err(),
        NonEmptyError(PhantomData)
    );

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
        NonEmptyError(PhantomData)
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
        NonEmptyError(PhantomData)
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
        NonEmptyError(PhantomData)
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
        NonEmptyError(PhantomData)
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
        NonEmptyError(PhantomData)
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
        NonEmptyError(PhantomData)
    );

    let mut non_empty = LinkedList::new();
    non_empty.push_back(1);
    let res = NonEmptyList::new(non_empty.clone());
    assert!(res.is_ok());
    assert_eq!(*res.unwrap(), non_empty);
}

#[test]
fn test_haslength_arrays() {
    let empty: [i32; 0] = [];
    assert_eq!(empty.length(), 0);
    assert!(empty.zero_length());

    let arr = [1, 2, 3, 4, 5];
    assert_eq!(arr.length(), 5);
    assert!(!arr.zero_length());
}

#[test]
fn test_haslength_slices() {
    let empty: &[i32] = &[];
    assert_eq!(empty.length(), 0);
    assert!(empty.zero_length());

    let slice: &[i32] = &[1, 2, 3];
    assert_eq!(slice.length(), 3);
    assert!(!slice.zero_length());
}

#[test]
fn test_haslength_str() {
    let empty: &str = "";
    assert_eq!(empty.length(), 0);
    assert!(empty.zero_length());

    let s: &str = "hello";
    assert_eq!(s.length(), 5);
    assert!(!s.zero_length());
}

#[test]
fn test_haslength_string() {
    let empty = String::new();
    assert_eq!(empty.length(), 0);
    assert!(empty.zero_length());

    let s = String::from("hello world");
    assert_eq!(s.length(), 11);
    assert!(!s.zero_length());
}

#[test]
fn test_haslength_vec() {
    let empty: Vec<i32> = vec![];
    assert_eq!(empty.length(), 0);
    assert!(empty.zero_length());

    let v = vec![1, 2, 3, 4];
    assert_eq!(v.length(), 4);
    assert!(!v.zero_length());
}

#[test]
fn test_haslength_box() {
    let boxed_vec: Box<Vec<i32>> = Box::new(vec![1, 2, 3]);
    assert_eq!(boxed_vec.length(), 3);

    let boxed_slice: Box<[i32]> =
        vec![1, 2, 3, 4].into_boxed_slice();
    assert_eq!(boxed_slice.length(), 4);

    let boxed_str: Box<str> =
        String::from("test").into_boxed_str();
    assert_eq!(boxed_str.length(), 4);
}

#[test]
fn test_haslength_cow() {
    use std::borrow::Cow;

    let cow_borrowed: Cow<[i32]> =
        Cow::Borrowed(&[1, 2, 3]);
    assert_eq!(cow_borrowed.length(), 3);

    let cow_owned: Cow<[i32]> =
        Cow::Owned(vec![1, 2, 3, 4]);
    assert_eq!(cow_owned.length(), 4);

    let cow_str_borrowed: Cow<str> = Cow::Borrowed("hello");
    assert_eq!(cow_str_borrowed.length(), 5);

    let cow_str_owned: Cow<str> =
        Cow::Owned(String::from("world"));
    assert_eq!(cow_str_owned.length(), 5);
}

#[test]
fn test_haslength_hashmap() {
    use std::collections::HashMap;

    let empty: HashMap<i32, String> = HashMap::new();
    assert_eq!(empty.length(), 0);
    assert!(empty.zero_length());

    let mut map = HashMap::new();
    let _ = map.insert(1, "one".to_string());
    let _ = map.insert(2, "two".to_string());
    assert_eq!(map.length(), 2);
    assert!(!map.zero_length());
}

#[test]
fn test_haslength_btreeset() {
    use std::collections::BTreeSet;

    let empty: BTreeSet<i32> = BTreeSet::new();
    assert_eq!(empty.length(), 0);
    assert!(empty.zero_length());

    let mut set = BTreeSet::new();
    let _ = set.insert(1);
    let _ = set.insert(2);
    let _ = set.insert(3);
    assert_eq!(set.length(), 3);
    assert!(!set.zero_length());
}

#[test]
fn test_haslength_btreemap() {
    use std::collections::BTreeMap;

    let empty: BTreeMap<i32, String> = BTreeMap::new();
    assert_eq!(empty.length(), 0);
    assert!(empty.zero_length());

    let mut map = BTreeMap::new();
    let _ = map.insert(1, "one".to_string());
    let _ = map.insert(2, "two".to_string());
    assert_eq!(map.length(), 2);
    assert!(!map.zero_length());
}

#[test]
fn test_haslength_binaryheap() {
    use std::collections::BinaryHeap;

    let empty: BinaryHeap<i32> = BinaryHeap::new();
    assert_eq!(empty.length(), 0);
    assert!(empty.zero_length());

    let mut heap = BinaryHeap::new();
    heap.push(1);
    heap.push(2);
    heap.push(3);
    assert_eq!(heap.length(), 3);
    assert!(!heap.zero_length());
}

#[test]
fn test_haslength_path() {
    use std::path::Path;

    let path = Path::new("/usr/local/bin");
    assert_eq!(path.length(), 14);
    assert!(!path.zero_length());
}

#[test]
fn test_haslength_pathbuf() {
    use std::path::PathBuf;

    let path = PathBuf::from("/home/user/file.txt");
    assert_eq!(path.length(), 19);
    assert!(!path.zero_length());
}

#[test]
fn test_haslength_osstr() {
    use std::ffi::OsStr;

    let os_str = OsStr::new("hello");
    assert_eq!(os_str.length(), 5);
    assert!(!os_str.zero_length());
}

#[test]
fn test_haslength_osstring() {
    use std::ffi::OsString;

    let os_string = OsString::from("hello world");
    assert_eq!(os_string.length(), 11);
    assert!(!os_string.zero_length());
}

#[test]
fn test_haslength_cstr() {
    use std::ffi::CStr;

    let c_str =
        CStr::from_bytes_with_nul(b"hello\0").unwrap();
    assert_eq!(c_str.length(), 5);
    assert!(!c_str.zero_length());
}

#[test]
fn test_haslength_cstring() {
    use std::ffi::CString;

    let c_string = CString::new("hello").unwrap();
    assert_eq!(c_string.length(), 5);
    assert!(!c_string.zero_length());
}

#[test]
fn test_haslength_rc() {
    use std::rc::Rc;

    let rc_vec: Rc<Vec<i32>> = Rc::new(vec![1, 2, 3]);
    assert_eq!(rc_vec.length(), 3);

    let rc_str: Rc<str> = Rc::from("hello");
    assert_eq!(rc_str.length(), 5);
}

#[test]
fn test_haslength_arc() {
    use std::sync::Arc;

    let arc_vec: Arc<Vec<i32>> = Arc::new(vec![1, 2, 3, 4]);
    assert_eq!(arc_vec.length(), 4);

    let arc_str: Arc<str> = Arc::from("world");
    assert_eq!(arc_str.length(), 5);
}
