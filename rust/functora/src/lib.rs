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
pub mod futures {
    use futures::future::{Map, Ready};
    use futures::stream::TryFold;
    use futures::{FutureExt, StreamExt, TryStream, TryStreamExt, future};
    use std::marker::PhantomData;
    use std::ops::ControlFlow;

    pub struct ControlStream<S, Cont, Halt>(S, PhantomData<(Cont, Halt)>);

    impl<S, Cont, Halt> ControlStream<S, Cont, Halt> {
        #[allow(clippy::type_complexity)]
        pub fn new(
            stream: S,
        ) -> ControlStream<futures::stream::Map<S, fn(Cont) -> Result<Cont, Halt>>, Cont, Halt>
        where
            S: futures::Stream<Item = Cont>,
        {
            ControlStream(stream.map(Ok), PhantomData)
        }

        #[allow(clippy::type_complexity)]
        pub fn try_fold<Acc, F>(
            self,
            init: Acc,
            mut f: F,
        ) -> Map<
            TryFold<
                S,
                Ready<Result<Acc, Halt>>,
                Acc,
                impl FnMut(Acc, Cont) -> Ready<Result<Acc, Halt>>,
            >,
            impl FnMut(Result<Acc, Halt>) -> ControlFlow<Halt, Acc>,
        >
        where
            S: TryStream<Ok = Cont, Error = Halt> + TryStreamExt,
            F: FnMut(Acc, Cont) -> ControlFlow<Halt, Acc>,
            Self: Sized,
        {
            let g = move |acc, item| match f(acc, item) {
                ControlFlow::Continue(cont) => future::ready(Ok(cont)),
                ControlFlow::Break(halt) => future::ready(Err(halt)),
            };
            let h = |res: Result<Acc, Halt>| match res {
                Ok(cont) => ControlFlow::Continue(cont),
                Err(halt) => ControlFlow::Break(halt),
            };
            self.0.try_fold(init, g).map(h)
        }
    }
}
