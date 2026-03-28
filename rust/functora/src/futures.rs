use futures::{Future, FutureExt, StreamExt, TryStream, TryStreamExt};
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

    pub fn try_fold<Cont, Halt, Acc, F, Fut>(
        self,
        init: Acc,
        mut f: F,
    ) -> impl Future<Output = ControlFlow<Halt, Acc>>
    where
        S: TryStream<Ok = Cont, Error = Halt>,
        F: FnMut(Acc, Cont) -> Fut,
        Fut: Future<Output = ControlFlow<Halt, Acc>>,
    {
        let g = move |acc, item| {
            f(acc, item).map(|cf| match cf {
                ControlFlow::Continue(cont) => Ok(cont),
                ControlFlow::Break(halt) => Err(halt),
            })
        };
        let h = |res| match res {
            Ok(cont) => ControlFlow::Continue(cont),
            Err(halt) => ControlFlow::Break(halt),
        };
        self.0.try_fold(init, g).map(h)
    }
}
