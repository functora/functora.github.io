#![doc = include_str!("../README.md")]

pub fn id<T>(x: T) -> T {
    x
}

pub fn ok<E>() -> Result<(), E> {
    Ok(())
}

pub trait Void {
    fn void(&self);
}

impl<T> Void for T {
    fn void(&self) {}
}

pub fn void<T>(_: T) {}

pub trait Tweak
where
    Self: Sized,
{
    fn tweak(&mut self, f: impl FnOnce(&Self) -> Self);
    fn try_tweak<E>(
        &mut self,
        f: impl FnOnce(&Self) -> Result<Self, E>,
    ) -> Result<(), E>;
}

impl<T> Tweak for T {
    fn tweak(&mut self, f: impl FnOnce(&T) -> T) {
        *self = f(self);
    }
    fn try_tweak<E>(
        &mut self,
        f: impl FnOnce(&T) -> Result<Self, E>,
    ) -> Result<(), E> {
        f(self).and_then(|x| {
            *self = x;
            ok()
        })
    }
}

pub trait Guard<E>
where
    Self: Sized,
{
    fn guard(self, e: E) -> Result<(), E> {
        self.guard_then(|| e)
    }
    fn guard_then(
        self,
        f: impl FnOnce() -> E,
    ) -> Result<(), E>;
}

impl<E> Guard<E> for bool {
    fn guard_then(
        self,
        f: impl FnOnce() -> E,
    ) -> Result<(), E> {
        if self { ok() } else { Err(f()) }
    }
}

impl<T, E> Guard<E> for Option<T>
where
    T: Guard<E>,
{
    fn guard_then(
        self,
        f: impl FnOnce() -> E,
    ) -> Result<(), E> {
        match self {
            None => Err(f()),
            Some(x) => x.guard_then(f),
        }
    }
}

impl<T, EOuter, EInner> Guard<EOuter> for Result<T, EInner>
where
    T: Guard<EOuter>,
    EOuter: From<EInner>,
{
    fn guard_then(
        self,
        f: impl FnOnce() -> EOuter,
    ) -> Result<(), EOuter> {
        self?.guard_then(f)
    }
}

pub fn guard<T, E>(x: T, e: E) -> Result<(), E>
where
    T: Guard<E>,
{
    x.guard(e)
}

pub fn guard_then<T, E>(
    x: T,
    f: impl FnOnce() -> E,
) -> Result<(), E>
where
    T: Guard<E>,
{
    x.guard_then(f)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::Infallible;

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
    fn ok_function() {
        assert_eq!(ok::<Infallible>(), Ok(()));
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
    fn try_tweak_method_ok() {
        let mut x = "41".to_string();
        assert_eq!(
            x.try_tweak(|s| {
                let n: u32 = s.parse()?;
                Ok::<String, std::num::ParseIntError>(
                    (n + 1).to_string(),
                )
            }),
            Ok(())
        );
        assert_eq!(x, "42");
    }
    #[test]
    fn try_tweak_method_err_does_not_mutate() {
        let mut x = "not a number".to_string();
        assert!(
            x.try_tweak(|s| {
                let n: u32 = s.parse()?;
                Ok::<String, std::num::ParseIntError>(
                    (n + 1).to_string(),
                )
            })
            .is_err()
        );
        assert_eq!(x, "not a number");
    }
    #[test]
    fn try_tweak_nested_ok() {
        let mut x = ((("41".to_string(), 3), 2), 1);
        assert_eq!(
            x.0.0.0.try_tweak(|s| {
                let n: u32 = s.parse()?;
                Ok::<String, std::num::ParseIntError>(
                    (n + 1).to_string(),
                )
            }),
            Ok(())
        );
        assert_eq!(x, ((("42".to_string(), 3), 2), 1));
    }

    #[test]
    fn guard_function() {
        assert_eq!(guard(true, ()), Ok(()));
        assert_eq!(guard(false, "error"), Err("error"));
    }
    #[test]
    fn guard_method() {
        assert_eq!(true.guard(()), Ok(()));
        assert_eq!(false.guard("error"), Err("error"));
    }
    #[test]
    fn guard_expression() {
        let f = |x: u32| {
            guard(x > 0, ())?;
            Ok(42u32)
        };
        assert_eq!(f(1), Ok(42));
        assert_eq!(f(0), Err(()));
    }
    #[test]
    fn guard_option() {
        assert_eq!(Some(true).guard(()), Ok(()));
        assert_eq!(
            Some(false).guard("error"),
            Err("error")
        );
        assert_eq!(
            None::<bool>.guard("error"),
            Err("error")
        );
    }
    #[test]
    fn guard_result() {
        assert_eq!(
            Ok::<_, &str>(true).guard("error"),
            Ok(())
        );
        assert_eq!(
            Ok::<_, &str>(false).guard("error"),
            Err("error")
        );
        assert_eq!(
            Err::<bool, _>("orig").guard("error"),
            Err("orig")
        );
    }
    #[test]
    fn guard_nested() {
        assert_eq!(Some(Some(true)).guard(()), Ok(()));
        assert_eq!(
            Some(Some(false)).guard("error"),
            Err("error")
        );
        assert_eq!(
            Some(None::<bool>).guard("error"),
            Err("error")
        );
        assert_eq!(
            Ok::<_, &str>(Some(true)).guard("error"),
            Ok(())
        );
        assert_eq!(
            Ok::<_, &str>(Some(false)).guard("error"),
            Err("error")
        );
        assert_eq!(
            Ok::<_, &str>(None::<bool>).guard("error"),
            Err("error")
        );
        assert_eq!(
            Err::<Option<bool>, _>("orig").guard("error"),
            Err("orig")
        );
    }

    #[test]
    fn guard_real_life_scenarios() {
        struct Request {
            path: String,
            token: Option<String>,
        }

        #[derive(Debug, PartialEq)]
        enum Error {
            AccessDenied,
            RateLimitExceeded,
            InvalidInput,
        }

        let check_access = |user_id: u32, role: &str| {
            guard(user_id != 0, Error::InvalidInput)?;
            guard(role == "admin", Error::AccessDenied)?;
            Ok("Success")
        };

        assert_eq!(check_access(1, "admin"), Ok("Success"));
        assert_eq!(
            check_access(0, "admin"),
            Err(Error::InvalidInput)
        );
        assert_eq!(
            check_access(1, "user"),
            Err(Error::AccessDenied)
        );

        let process_data =
            |data: Option<u32>, limit: u32| {
                let val =
                    data.ok_or(Error::InvalidInput)?;
                guard(
                    val < limit,
                    Error::RateLimitExceeded,
                )?;
                Ok(val * 2)
            };

        assert_eq!(process_data(Some(10), 20), Ok(20));
        assert_eq!(
            process_data(None, 20),
            Err(Error::InvalidInput)
        );
        assert_eq!(
            process_data(Some(30), 20),
            Err(Error::RateLimitExceeded)
        );

        let handle_request = |req: Request| {
            guard(
                req.path.starts_with("/api"),
                Error::InvalidInput,
            )?;
            req.token
                .is_some()
                .guard(Error::AccessDenied)?;
            Ok("Authorized")
        };

        assert_eq!(
            handle_request(Request {
                path: "/api/v1".into(),
                token: Some("abc".into())
            }),
            Ok("Authorized")
        );
        assert_eq!(
            handle_request(Request {
                path: "/home".into(),
                token: Some("abc".into())
            }),
            Err(Error::InvalidInput)
        );
        assert_eq!(
            handle_request(Request {
                path: "/api/v1".into(),
                token: None
            }),
            Err(Error::AccessDenied)
        );
    }
}
