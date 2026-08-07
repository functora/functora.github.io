use futures::{Future, FutureExt, Stream, StreamExt, TryStreamExt};
use std::ops::ControlFlow;

pub struct ControlStream<S>(S);

impl<S> ControlStream<S> {
    pub const fn new(stream: S) -> ControlStream<S> {
        ControlStream(stream)
    }

    pub fn try_fold<Cont, Halt, Acc, F, Fut>(
        self,
        init: Acc,
        mut f: F,
    ) -> impl Future<Output = ControlFlow<Halt, Acc>>
    where
        S: Stream<Item = Cont>,
        F: FnMut(Acc, Cont) -> Fut,
        Fut: Future<Output = ControlFlow<Halt, Acc>>,
    {
        let g = move |acc, item| {
            f(acc, item).map(|cf| match cf {
                ControlFlow::Continue(cont) => Ok(cont),
                ControlFlow::Break(halt) => Err(halt),
            })
        };
        self.0.map(Ok).try_fold(init, g).map(|res| match res {
            Ok(cont) => ControlFlow::Continue(cont),
            Err(halt) => ControlFlow::Break(halt),
        })
    }
}
