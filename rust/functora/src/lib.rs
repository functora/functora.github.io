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

pub trait Guard {
    fn guard(&self) -> Option<()>;
}
impl Guard for bool {
    fn guard(&self) -> Option<()> {
        self.then_some(())
    }
}
pub fn guard<T: Guard>(x: &T) -> Option<()> {
    x.guard()
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
