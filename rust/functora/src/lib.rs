#![doc = include_str!("../README.md")]

pub fn id<T>(x: T) -> T {
    x
}

pub fn ok<E>() -> Result<(), E> {
    Ok(())
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

pub trait Guard<E> {
    fn guard(self, e: E) -> Result<(), E>;
}
impl<E> Guard<E> for bool {
    fn guard(self, e: E) -> Result<(), E> {
        if self { ok() } else { Err(e) }
    }
}
impl<T, E> Guard<E> for Option<T>
where
    T: Guard<E>,
{
    fn guard(self, e: E) -> Result<(), E> {
        match self {
            None => Err(e),
            Some(x) => x.guard(e),
        }
    }
}
impl<T, EOuter, EInner> Guard<EOuter> for Result<T, EInner>
where
    T: Guard<EOuter>,
    EOuter: From<EInner>,
{
    fn guard(self, e: EOuter) -> Result<(), EOuter> {
        self?.guard(e)
    }
}
pub fn guard<T, E>(x: T, e: E) -> Result<(), E>
where
    T: Guard<E>,
{
    x.guard(e)
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

    #[test]
    fn guard_function() {
        assert_eq!(guard(true), Some(()));
        assert_eq!(guard(false), None);
    }
    #[test]
    fn guard_method() {
        assert_eq!(true.guard(), Some(()));
        assert_eq!(false.guard(), None);
    }
    #[test]
    fn guard_expression() {
        let f = |x: u32| {
            guard(x > 0)?;
            Some(42)
        };
        assert_eq!(f(1), Some(42));
        assert_eq!(f(0), None);
    }
    #[test]
    fn guard_option() {
        assert_eq!(Some(true).guard(), Some(()));
        assert_eq!(Some(false).guard(), None);
        assert_eq!(None::<bool>.guard(), None);
    }
    #[test]
    fn guard_result() {
        assert_eq!(Ok::<_, ()>(true).guard(), Some(()));
        assert_eq!(Ok::<_, ()>(false).guard(), None);
        assert_eq!(Err::<bool, _>(()).guard(), None);
    }
    #[test]
    fn guard_nested() {
        assert_eq!(Some(Some(true)).guard(), Some(()));
        assert_eq!(Some(Some(false)).guard(), None);
        assert_eq!(Some(None::<bool>).guard(), None);
        assert_eq!(
            Ok::<_, ()>(Some(true)).guard(),
            Some(())
        );
        assert_eq!(Ok::<_, ()>(Some(false)).guard(), None);
        assert_eq!(Ok::<_, ()>(None::<bool>).guard(), None);
        assert_eq!(
            Err::<Option<bool>, _>(()).guard(),
            None
        );
    }
}
