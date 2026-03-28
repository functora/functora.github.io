#![cfg(feature = "futures")]

use functora::futures::ControlStream;
use futures::stream::{self, StreamExt};
use std::convert::Infallible;
use std::ops::ControlFlow;

#[tokio::test]
async fn control_stream_new_and_count() {
    let stream = stream::iter(vec![Ok::<i32, &str>(1), Ok(2), Ok(3)]);
    let cs = ControlStream::new(stream);
    let count = cs.0.count().await;
    assert_eq!(count, 3);
}

#[tokio::test]
async fn try_fold_continue_all() {
    let stream = stream::iter(vec![Ok::<i32, &str>(1), Ok(2), Ok(3)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |acc, item| ControlFlow::Continue(acc + item));
    assert_eq!(result.await, ControlFlow::Continue(6));
}

#[tokio::test]
async fn try_fold_break_early() {
    let stream = stream::iter(vec![Ok::<i32, &str>(1), Ok(2), Ok(3), Ok(4)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |acc, item| {
        if acc + item >= 5 {
            ControlFlow::Break("halt")
        } else {
            ControlFlow::Continue(acc + item)
        }
    });
    assert_eq!(result.await, ControlFlow::Break("halt"));
}

#[tokio::test]
async fn try_fold_break_on_first() {
    let stream = stream::iter(vec![Ok::<i32, &str>(1), Ok(2), Ok(3)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |_acc, _item| ControlFlow::Break("immediate"));
    assert_eq!(result.await, ControlFlow::Break("immediate"));
}

#[tokio::test]
async fn try_fold_empty_stream() {
    let stream = stream::iter(Vec::<Result<i32, &str>>::new());
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(42, |acc, item| ControlFlow::Continue(acc + item));
    assert_eq!(result.await, ControlFlow::Continue(42));
}

#[tokio::test]
async fn try_fold_stream_error() {
    let stream = stream::iter(vec![Ok::<i32, &str>(1), Err("stream_error"), Ok(3)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |acc, item| ControlFlow::Continue(acc + item));
    assert_eq!(result.await, ControlFlow::Break("stream_error"));
}

#[tokio::test]
async fn try_fold_break_with_error() {
    let stream = stream::iter(vec![Ok::<i32, &str>(1), Ok(2), Ok(3)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |acc, item| {
        if item == 2 {
            ControlFlow::Break("halt_at_2")
        } else {
            ControlFlow::Continue(acc + item)
        }
    });
    assert_eq!(result.await, ControlFlow::Break("halt_at_2"));
}

#[tokio::test]
async fn try_fold_accumulate_until_break() {
    let stream = stream::iter(vec![Ok::<i32, i32>(1), Ok(2), Ok(3), Ok(4), Ok(5)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |acc, item| {
        if item == 4 {
            ControlFlow::Break(acc)
        } else {
            ControlFlow::Continue(acc + item)
        }
    });
    assert_eq!(result.await, ControlFlow::Break(6));
}

#[tokio::test]
async fn try_fold_with_string_halt() {
    let stream = stream::iter(vec![Ok::<&str, &str>("a"), Ok("b"), Ok("c")]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(String::new(), |mut acc, item| {
        if item == "b" {
            ControlFlow::Break("stopped")
        } else {
            acc.push_str(item);
            ControlFlow::Continue(acc)
        }
    });
    assert_eq!(result.await, ControlFlow::Break("stopped"));
}

#[tokio::test]
async fn try_fold_complex_accumulator() {
    let stream = stream::iter(vec![Ok::<i32, Vec<i32>>(1), Ok(2), Ok(3), Ok(4)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(Vec::new(), |mut acc, item| {
        if item % 2 == 0 {
            acc.push(item);
        }
        if acc.len() >= 2 {
            ControlFlow::Break(acc)
        } else {
            ControlFlow::Continue(acc)
        }
    });
    assert_eq!(result.await, ControlFlow::Break(vec![2, 4]));
}

#[tokio::test]
async fn try_fold_infallible_stream() {
    let stream = stream::iter(vec![1, 2, 3, 4, 5]);
    let mapped = stream.map(Ok::<_, Infallible>);
    let cs = ControlStream::new(mapped);
    let result = cs.try_fold(0, |acc, item| ControlFlow::Continue(acc + item));
    assert_eq!(result.await, ControlFlow::Continue(15));
}

#[tokio::test]
async fn try_fold_single_element_continue() {
    let stream = stream::iter(vec![Ok::<i32, &str>(42)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |acc, item| ControlFlow::Continue(acc + item));
    assert_eq!(result.await, ControlFlow::Continue(42));
}

#[tokio::test]
async fn try_fold_single_element_break() {
    let stream = stream::iter(vec![Ok::<i32, &str>(42)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |_acc, _item| ControlFlow::Break("halt"));
    assert_eq!(result.await, ControlFlow::Break("halt"));
}

#[tokio::test]
async fn try_fold_zero_initial_accumulator() {
    let stream = stream::iter(vec![Ok::<i32, &str>(1), Ok(2), Ok(3)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |acc, item| ControlFlow::Continue(acc + item));
    assert_eq!(result.await, ControlFlow::Continue(6));
}

#[tokio::test]
async fn try_fold_non_zero_initial_accumulator() {
    let stream = stream::iter(vec![Ok::<i32, &str>(1), Ok(2), Ok(3)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(10, |acc, item| ControlFlow::Continue(acc + item));
    assert_eq!(result.await, ControlFlow::Continue(16));
}

#[tokio::test]
async fn try_fold_with_result_returning_ok() {
    let stream = stream::iter(vec![Ok::<i32, &str>(1), Ok(2), Ok(3)]);
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(0, |acc, item| ControlFlow::Continue(acc + item));
    assert_eq!(result.await, ControlFlow::Continue(6));
}

#[tokio::test]
async fn try_fold_preserves_stream_order() {
    let input = vec![1, 2, 3, 4, 5];
    let stream = stream::iter(input.iter().copied().map(Ok::<_, &str>));
    let cs = ControlStream::new(stream);
    let result = cs.try_fold(Vec::new(), |mut acc, item| {
        acc.push(item);
        ControlFlow::Continue(acc)
    });
    assert_eq!(result.await, ControlFlow::Continue(vec![1, 2, 3, 4, 5]));
}
