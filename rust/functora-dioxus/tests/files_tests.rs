use dioxus::core::ScopeId;
use dioxus::prelude::{Signal, VirtualDom, rsx};
use dioxus_document::{Eval, EvalError, Evaluator};
use functora_dioxus::abort::EvalAbort;
use functora_dioxus::files::{BlobMemo, blob_url_script, pick_files_with, pick_script, video_thumbnail_script};
use functora_dioxus::progress::{Job, Stage};
use generational_box::{AnyStorage, Owner, UnsyncStorage};
use serde_json::json;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use std::task::{Context, Poll};
#[test]
fn pick_script_single_and_multiple() {
    let single = pick_script(false);
    assert!(single.contains("input.multiple = false"));
    assert!(!single.contains("input.multiple = true"));
    let multiple = pick_script(true);
    assert!(multiple.contains("input.multiple = true"));
    assert!(multiple.contains("2 * 1024 * 1024"));
    assert!(multiple.contains("dioxus.send"));
}

#[test]
fn video_thumbnail_script_extracts_first_frame() {
    let script = video_thumbnail_script();
    assert!(script.contains("dioxus.recv"));
    assert!(script.contains("dioxus.send"));
    assert!(script.contains("URL.createObjectURL"));
    assert!(script.contains("canvas.getContext('2d').drawImage"));
    assert!(script.contains("canvas.toDataURL('image/jpeg', 0.7)"));
}

#[test]
fn blob_url_script_assembles_blob_and_returns_object_url() {
    let script = blob_url_script();
    assert!(script.contains("dioxus.recv"));
    assert!(script.contains("new Blob"));
    assert!(script.contains("URL.createObjectURL"));
    assert!(script.contains("dioxus.send({ t: 'ack' })"));
    assert!(script.contains("dioxus.send({ ok: true, url })"));
    assert!(script.contains("dioxus.send({ ok: false, error: msg })"));
}

#[test]
fn blob_url_script_aborts_on_abort_message() {
    let script = blob_url_script();
    assert!(script.contains("m.t === 'abort'"));
}

#[test]
fn video_thumbnail_script_aborts_and_revokes_url_on_failure() {
    let script = video_thumbnail_script();
    assert!(script.contains("m.t === 'abort'"));
    assert!(script.contains("if (url) URL.revokeObjectURL(url);"));
}

#[test]
fn data_url_mime_strips_data_url_parameters() {
    assert_eq!(
        functora_dioxus::web::data_url_mime("data:video/mp4;base64"),
        Some("video/mp4")
    );
    assert_eq!(functora_dioxus::web::data_url_mime("data:image/png"), Some("image/png"));
    assert_eq!(
        functora_dioxus::web::data_url_mime("data:image/png;charset=utf-8"),
        Some("image/png")
    );
    assert_eq!(functora_dioxus::web::data_url_mime("data:"), None);
    assert_eq!(functora_dioxus::web::data_url_mime("video/mp4"), None);
}

#[derive(Default)]
struct MockEvaluator(Rc<RefCell<Vec<serde_json::Value>>>);

impl Evaluator for MockEvaluator {
    fn send(&self, data: serde_json::Value) -> Result<(), EvalError> {
        self.0.borrow_mut().push(data);
        Ok(())
    }

    fn poll_recv(&mut self, _: &mut Context<'_>) -> Poll<Result<serde_json::Value, EvalError>> {
        Poll::Ready(Err(EvalError::Communication(String::from("closed"))))
    }

    fn poll_join(&mut self, _: &mut Context<'_>) -> Poll<Result<serde_json::Value, EvalError>> {
        Poll::Ready(Err(EvalError::Communication(String::from("closed"))))
    }
}

fn mock_owner(log: Rc<RefCell<Vec<serde_json::Value>>>) -> (Eval, Owner<UnsyncStorage>) {
    let owner = UnsyncStorage::owner();
    let eval = Eval::new(owner.insert(Box::new(MockEvaluator(log)) as Box<dyn Evaluator>));
    (eval, owner)
}

#[test]
fn eval_abort_guard_sends_abort_when_dropped_armed() {
    let log = Rc::new(RefCell::new(Vec::new()));
    let (eval, _owner) = mock_owner(log.clone());
    let guard = EvalAbort::new(eval, serde_json::json!({ "t": "abort" }));
    drop(guard);
    assert_eq!(*log.borrow(), vec![serde_json::json!({ "t": "abort" })]);
}

#[test]
fn eval_abort_guard_sends_nothing_when_disarmed() {
    let log = Rc::new(RefCell::new(Vec::new()));
    let (eval, _owner) = mock_owner(log.clone());
    let guard = EvalAbort::new(eval, serde_json::json!({ "t": "abort" }));
    guard.disarm();
    assert!(log.borrow().is_empty());
}

#[test]
fn blob_memo_forgets_revoked_urls() {
    let mut memo = BlobMemo::default();
    memo.insert("clip.mp4", 1, "blob:https://functora/a".into());
    memo.insert("pic.png", 2, "blob:https://functora/b".into());
    assert_eq!(memo.forget("blob:https://functora/a"), 1);
    assert!(memo.get("clip.mp4", 1).is_none());
    assert_eq!(memo.get("pic.png", 2), Some("blob:https://functora/b"));
    assert_eq!(memo.forget("blob:https://functora/missing"), 0);
}

struct ScriptedEvaluator {
    responses: RefCell<VecDeque<Poll<Result<serde_json::Value, EvalError>>>>,
}

impl Evaluator for ScriptedEvaluator {
    fn send(&self, _: serde_json::Value) -> Result<(), EvalError> {
        Ok(())
    }
    fn poll_recv(&mut self, _: &mut Context<'_>) -> Poll<Result<serde_json::Value, EvalError>> {
        self.responses
            .borrow_mut()
            .pop_front()
            .unwrap_or_else(|| Poll::Ready(Err(EvalError::Communication("script exhausted".into()))))
    }
    fn poll_join(&mut self, _: &mut Context<'_>) -> Poll<Result<serde_json::Value, EvalError>> {
        Poll::Ready(Err(EvalError::Communication("closed".into())))
    }
}

fn scripted_eval(responses: Vec<Poll<Result<serde_json::Value, EvalError>>>) -> (Eval, Owner<UnsyncStorage>) {
    let owner = UnsyncStorage::owner();
    let eval = Eval::new(owner.insert(Box::new(ScriptedEvaluator {
        responses: RefCell::new(responses.into()),
    }) as Box<dyn Evaluator>));
    (eval, owner)
}

fn progress_in(dom: &mut VirtualDom) -> Signal<Option<Job<Stage>>> {
    dom.rebuild_in_place();
    dom.in_runtime(|| Signal::<Option<Job<Stage>>>::new_in_scope(None, ScopeId(0)))
}

#[tokio::test]
async fn pick_files_with_retries_on_eval_finished_and_succeeds() {
    let mut dom = VirtualDom::new(|| rsx! { "x" });
    let progress = progress_in(&mut dom);
    let (eval_dies, _owner_dies) = scripted_eval(vec![Poll::Ready(Err(EvalError::Finished))]);
    let (eval_ok, _owner_ok) = scripted_eval(vec![
        Poll::Ready(Ok(json!({ "t": "begin", "name": "a.txt", "size": 2 }))),
        Poll::Ready(Ok(json!({ "t": "chunk", "data": "aGk=" }))),
        Poll::Ready(Ok(json!({ "t": "done" }))),
    ]);
    let mut evals = vec![eval_dies, eval_ok];
    let result = pick_files_with(progress, Stage::Attach, move || evals.remove(0)).await;
    let files = match result {
        Ok(f) => f,
        Err(e) => panic!("expected Ok(files), got Err({e:?})"),
    };
    assert_eq!(files.len(), 1);
    assert_eq!(files[0].0, "a.txt");
    assert_eq!(files[0].1, b"hi");
}

#[tokio::test]
async fn pick_files_with_returns_eval_finished_when_retry_also_dies() {
    let mut dom = VirtualDom::new(|| rsx! { "x" });
    let progress = progress_in(&mut dom);
    let (eval1, _o1) = scripted_eval(vec![Poll::Ready(Err(EvalError::Finished))]);
    let (eval2, _o2) = scripted_eval(vec![Poll::Ready(Err(EvalError::Finished))]);
    let mut evals = vec![eval1, eval2];
    let result = pick_files_with(progress, Stage::Attach, move || evals.remove(0)).await;
    assert!(matches!(result, Err(functora_dioxus::Error::EvalFinished)));
}

#[tokio::test]
async fn pick_files_with_does_not_retry_on_success() {
    let mut dom = VirtualDom::new(|| rsx! { "x" });
    let progress = progress_in(&mut dom);
    let (eval_ok, _owner_ok) = scripted_eval(vec![
        Poll::Ready(Ok(json!({ "t": "begin", "name": "x", "size": 1 }))),
        Poll::Ready(Ok(json!({ "t": "chunk", "data": "AQ==" }))),
        Poll::Ready(Ok(json!({ "t": "done" }))),
    ]);
    let mut evals = vec![eval_ok];
    let result = pick_files_with(progress, Stage::Attach, move || evals.remove(0)).await;
    assert!(result.is_ok());
    let files = match result {
        Ok(f) => f,
        Err(e) => panic!("expected Ok(files), got Err({e:?})"),
    };
    assert_eq!(files.len(), 1);
}

#[tokio::test]
async fn pick_files_with_does_not_retry_on_non_finished_error() {
    let mut dom = VirtualDom::new(|| rsx! { "x" });
    let progress = progress_in(&mut dom);
    let (eval_err, _o) = scripted_eval(vec![Poll::Ready(Err(EvalError::Communication("boom".into())))]);
    let mut evals = vec![eval_err];
    let result = pick_files_with(progress, Stage::Attach, move || evals.remove(0)).await;
    assert!(matches!(result, Err(functora_dioxus::Error::JS(_))));
}
