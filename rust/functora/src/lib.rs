#![doc = include_str!("../README.md")]

pub fn id<T>(x: T) -> T {
    x
}

pub fn void<T>(_: T) {}
pub trait Void {
    fn void(&self);
}
impl<T> Void for T {
    fn void(&self) {}
}

pub fn once<T, U>(x: T) -> impl FnOnce(U) -> T {
    move |_| x
}
pub trait Once<T> {
    fn once<U>(self) -> impl FnOnce(U) -> T;
}
impl<T> Once<T> for T {
    fn once<U>(self) -> impl FnOnce(U) -> T {
        move |_| self
    }
}

pub fn always<T: Copy, U>(x: T) -> impl Fn(U) -> T {
    move |_| x
}
pub trait Always<T> {
    fn always<U>(self) -> impl Fn(U) -> Self;
}
impl<T: Copy> Always<T> for T {
    fn always<U>(self) -> impl Fn(U) -> Self {
        move |_| self
    }
}

#[macro_export]
macro_rules! always {
    ($x:expr) => {{ move |_| $x }};
}

pub trait Tweak {
    fn tweak(&mut self, f: impl FnOnce(&Self) -> Self)
    where
        Self: Sized;
}
impl<T> Tweak for T {
    fn tweak(&mut self, f: impl FnOnce(&T) -> T) {
        *self = f(self);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_function() {
        assert_eq!(id("Hello"), "Hello");
        let x = "Hello";
        assert_eq!(id(x), "Hello");
        assert_eq!(id(x), "Hello");
        let x = "Hello".to_string();
        assert_eq!(id(x), "Hello");
    }

    #[test]
    fn void_function() {
        assert_eq!(void("Hello"), ());
        let x = "Hello";
        assert_eq!(void(x), ());
        assert_eq!(void(x), ());
        let x = "Hello".to_string();
        assert_eq!(void(x), ());
    }
    #[test]
    fn void_method() {
        assert_eq!("Hello".void(), ());
        let x = "Hello";
        assert_eq!(x.void(), ());
        assert_eq!(x.void(), ());
        let x = "Hello".to_string();
        assert_eq!(x.void(), ());
        assert_eq!(x.void(), ());
    }

    #[test]
    fn once_function() {
        let f = once("Hello".to_string());
        assert_eq!(f(3), "Hello");
    }
    #[test]
    fn once_method() {
        let f = "Hello".to_string().once();
        assert_eq!(f(3), "Hello");
    }

    #[test]
    fn always_function() {
        let f = always("Hello");
        assert_eq!(f(3), "Hello");
        assert_eq!(f(4), "Hello");
    }
    #[test]
    fn always_method() {
        let f = "Hello".always();
        assert_eq!(f(3), "Hello");
        assert_eq!(f(4), "Hello");
    }

    #[test]
    fn always_macro_lit() {
        let f = always!("Hello");
        assert_eq!(f(3), "Hello");
        assert_eq!(f(4), "Hello");
    }
    #[test]
    fn always_macro_exp() {
        let f = always!("Hello".split_at(2).0);
        assert_eq!(f(3), "He");
        assert_eq!(f(4), "He");
    }
    #[test]
    fn always_macro_blk() {
        let f = always!({
            let x = "Hello".split_at(2);
            x.0
        });
        assert_eq!(f(3), "He");
        assert_eq!(f(4), "He");
    }

    #[test]
    fn tweak_method() {
        let mut x = "Hello".to_string();
        x.tweak(|s| s.to_uppercase());
        assert_eq!(x, "HELLO");
    }
    #[test]
    fn tweak_nested() {
        let mut x = ((("hello".to_string(), 3), 2), 1);
        x.0.0.0.tweak(|x| x.to_uppercase());
        assert_eq!(x, ((("HELLO".to_string(), 3), 2), 1));
    }
}
