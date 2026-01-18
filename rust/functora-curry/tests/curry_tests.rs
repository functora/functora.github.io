use functora_curry::{Curry, PiMut, PiRef, PiVal};

#[test]
fn test_owned_owned() {
    fn add(a: i32, b: i32) -> i32 {
        a + b
    }
    let curried = add.curry::<PiVal<_>>(5);
    assert_eq!(curried(10), 15);
}

#[test]
fn test_ref_ref() {
    fn add_refs(a: &i32, b: &i32) -> i32 {
        *a + *b
    }
    let a = 5;
    let b = 10;
    let curried = add_refs.curry::<PiRef<_>>(&a);
    assert_eq!(curried(&b), 15);
}

#[test]
fn test_mut_mut() {
    fn add_muts(a: &mut i32, b: &mut i32) {
        *a += *b;
    }
    let mut a = 5;
    let mut b = 10;
    let curried = add_muts.curry::<PiMut<_>>(&mut a);
    curried(&mut b);
    assert_eq!(a, 15);
}

#[test]
fn test_mixed_ref_owned() {
    fn mixed(a: &i32, b: i32) -> i32 {
        *a + b
    }
    let a = 5;
    let curried = mixed.curry::<PiVal<_>>(&a);
    assert_eq!(curried(10), 15);
}

#[test]
fn test_mixed_owned_ref() {
    fn mixed(a: i32, b: &i32) -> i32 {
        a + *b
    }
    let b = 10;
    let curried = mixed.curry::<PiRef<_>>(5);
    assert_eq!(curried(&b), 15);
}

#[test]
fn test_higher_order() {
    fn apply_once<F, T, R>(f: F, arg: T) -> R
    where
        F: FnOnce(T) -> R,
    {
        f(arg)
    }

    fn add(a: i32, b: i32) -> i32 {
        a + b
    }

    let curried = add.curry::<PiVal<_>>(5);
    let result = apply_once(curried, 10);
    assert_eq!(result, 15);
}

#[test]
fn test_closure() {
    let add = |a: i32, b: i32| a + b;
    let curried = add.curry::<PiVal<_>>(5);
    assert_eq!(curried(10), 15);
}

#[test]
fn test_complex_types() {
    #[derive(Debug, PartialEq, Eq)]
    struct Point {
        x: i32,
        y: i32,
    }

    fn move_point(p: Point, offset: i32) -> Point {
        Point {
            x: p.x + offset,
            y: p.y + offset,
        }
    }

    let p = Point { x: 1, y: 2 };
    let curried = move_point.curry::<PiVal<_>>(p);
    assert_eq!(curried(3), Point { x: 4, y: 5 });
}

#[test]
fn test_ref_to_unsized() {
    fn str_len(prefix: &str, s: &str) -> usize {
        prefix.len() + s.len()
    }
    let prefix = "hello";
    let s = "world";
    let curried = str_len.curry::<PiRef<_>>(prefix);
    assert_eq!(curried(s), 10);
}

#[test]
fn test_mut_ref() {
    assert_eq!(bar(foo.curry::<PiMut<_>>(4)), 10);
}

fn foo(a: u8, b: &mut Vec<u8>) -> u8 {
    b.push(a);
    b.iter().sum()
}

fn bar(f: impl FnOnce(&mut Vec<u8>) -> u8) -> u8 {
    let mut v = vec![1, 2, 3];
    f(&mut v)
}
