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
    fn always<U>(self) -> impl Fn(U) -> T;
}
impl<T: Copy> Always<T> for T {
    fn always<U>(self) -> impl Fn(U) -> T {
        move |_| self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_function() {
        assert_eq!(id("Hello"), "Hello");
    }

    #[test]
    fn void_function() {
        assert_eq!(void("Hello"), ());
    }
    #[test]
    fn void_method() {
        assert_eq!("Hello".void(), ());
    }

    #[test]
    fn once_function() {
        let f = once("Hello");
        assert_eq!(f(3), "Hello");
    }
    #[test]
    fn once_method() {
        let f = "Hello".once();
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
}
