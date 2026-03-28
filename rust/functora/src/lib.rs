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
    fn try_tweak<E>(&mut self, f: impl FnOnce(&Self) -> Result<Self, E>) -> Result<(), E>;
}

impl<T> Tweak for T {
    fn tweak(&mut self, f: impl FnOnce(&T) -> T) {
        *self = f(self);
    }
    fn try_tweak<E>(&mut self, f: impl FnOnce(&T) -> Result<Self, E>) -> Result<(), E> {
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
    fn guard_then(self, f: impl FnOnce() -> E) -> Result<(), E>;
}

impl<E> Guard<E> for bool {
    fn guard_then(self, f: impl FnOnce() -> E) -> Result<(), E> {
        if self { ok() } else { Err(f()) }
    }
}

impl<T, E> Guard<E> for Option<T>
where
    T: Guard<E>,
{
    fn guard_then(self, f: impl FnOnce() -> E) -> Result<(), E> {
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
    fn guard_then(self, f: impl FnOnce() -> EOuter) -> Result<(), EOuter> {
        self?.guard_then(f)
    }
}

pub fn guard<T, E>(x: T, e: E) -> Result<(), E>
where
    T: Guard<E>,
{
    x.guard(e)
}

pub fn guard_then<T, E>(x: T, f: impl FnOnce() -> E) -> Result<(), E>
where
    T: Guard<E>,
{
    x.guard_then(f)
}

#[cfg(feature = "futures")]
mod futures;
#[cfg(feature = "futures")]
pub use crate::futures::*;
