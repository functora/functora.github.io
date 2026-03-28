use futures::future::{Map, Ready};
use futures::stream::TryFold;
use futures::{FutureExt, StreamExt, TryStream, TryStreamExt, future};
pub use std::ops::ControlFlow;

pub struct ControlStream<S>(S);

impl<S> ControlStream<S> {
    #[allow(clippy::type_complexity)]
    pub fn new<Cont, Halt>(
        stream: S,
    ) -> ControlStream<futures::stream::Map<S, fn(Cont) -> Result<Cont, Halt>>>
    where
        S: futures::Stream<Item = Cont>,
    {
        ControlStream(stream.map(Ok))
    }

    #[allow(clippy::type_complexity)]
    pub fn try_fold<Cont, Halt, Acc, F>(
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
        S: TryStream<Ok = Cont, Error = Halt>,
        F: FnMut(Acc, Cont) -> ControlFlow<Halt, Acc>,
    {
        let g = move |acc, item| match f(acc, item) {
            ControlFlow::Continue(cont) => future::ready(Ok(cont)),
            ControlFlow::Break(halt) => future::ready(Err(halt)),
        };
        let h = |res| match res {
            Ok(cont) => ControlFlow::Continue(cont),
            Err(halt) => ControlFlow::Break(halt),
        };
        self.0.try_fold(init, g).map(h)
    }
}
