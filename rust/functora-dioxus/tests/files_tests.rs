use dioxus::core::ScopeId;
use dioxus::prelude::{Signal, VirtualDom, rsx};
use dioxus_document::{Eval, EvalError, Evaluator};
use functora_dioxus::abort::EvalAbort;
use functora_dioxus::files::{
    Attachment, BlobMemo, Preview, blob_url_script, format_size, is_text, mime_for, pick_files_with, pick_script,
    preview, preview_blob_url, preview_cached, preview_initial, video_thumbnail_script,
};
use functora_dioxus::progress::{Job, Stage};
use generational_box::{AnyStorage, Owner, UnsyncStorage};
use serde_json::json;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use std::task::{Context, Poll};

#[test]
fn attachment_defaults() {
    let att = Attachment::default();
    assert!(att.name.is_empty());
    assert!(att.data.is_empty());
}

#[test]
fn format_size_bytes() {
    assert_eq!(format_size(0), "0 B");
    assert_eq!(format_size(1023), "1023 B");
}

#[test]
fn format_size_kilobytes() {
    assert_eq!(format_size(1024), "1.0 KB");
    assert_eq!(format_size(1536), "1.5 KB");
}

#[test]
fn format_size_megabytes() {
    assert_eq!(format_size(1024 * 1024), "1.0 MB");
    assert_eq!(format_size(1536 * 1024), "1.5 MB");
}

#[test]
fn mime_for_known_extensions() {
    assert_eq!(mime_for("photo.JPG"), Some("image/jpeg"));
    assert_eq!(mime_for("doc.png"), Some("image/png"));
    assert_eq!(mime_for("note.txt"), Some("text/plain"));
    assert_eq!(mime_for("book.pdf"), Some("application/pdf"));
}

#[test]
fn mime_for_unknown_extension_is_none() {
    assert_eq!(mime_for("archive.bin"), None);
    assert_eq!(mime_for("noextension"), None);
}

#[test]
fn is_text_recognizes_text_and_structured() {
    assert!(is_text("text/plain"));
    assert!(is_text("text/markdown"));
    assert!(is_text("application/json"));
    assert!(!is_text("image/png"));
    assert!(!is_text("application/pdf"));
}

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
fn preview_image_video_audio_pdf() {
    assert!(matches!(preview("a.png", b"png"), Preview::Image(_)));
    assert!(matches!(preview("a.mp4", b"mp4"), Preview::Video(_)));
    assert!(matches!(preview("a.mp3", b"mp3"), Preview::Audio(_)));
    assert!(matches!(preview("a.pdf", b"pdf"), Preview::Pdf(_)));
}

#[test]
fn preview_text_and_markdown() {
    assert!(matches!(preview("a.txt", b"hello"), Preview::Text(t) if t == "hello"));
    assert!(matches!(preview("a.md", b"# hi"), Preview::Markdown(t) if t == "# hi"));
}

#[test]
fn preview_unknown_and_binary() {
    assert!(matches!(preview("a.bin", b"\x00\x01"), Preview::Download));
    assert!(matches!(preview("a.txt", b"\xff\xfe"), Preview::Download));
    assert!(matches!(preview("a.json", b"{\"a\":1}"), Preview::Text(_)));
}

#[test]
fn preview_data_uri_is_base64() {
    assert!(matches!(preview("a.png", b"png"), Preview::Image(url) if url.starts_with("data:image/png;base64,")));
}

#[test]
fn preview_cached_matches_preview() {
    let data = b"video-content";
    assert_eq!(preview("a.mp4", data), preview_cached("a.mp4", data));
}

#[test]
fn preview_cached_memoizes_same_attachment() {
    let data = b"video-content";
    assert_eq!(preview_cached("a.mp4", data), preview_cached("a.mp4", data));
    assert_eq!(preview_cached("a.mp4", data), preview_cached("a.mp4", b"video-content"));
}

#[test]
fn preview_cached_distinguishes_attachments() {
    let first = preview_cached("a.mp4", b"first-video");
    let second = preview_cached("a.mp4", b"second-video");
    assert_ne!(first, second);
    assert_eq!(preview_cached("b.mp4", b"first-video"), first);
    assert_ne!(preview_cached("a.png", b"first-video"), first);
}

#[test]
fn preview_initial_sync_for_text_and_none_for_streaming() {
    assert!(preview_initial("a.txt", b"hello").is_some());
    assert!(preview_initial("a.md", b"# hi").is_some());
    assert!(preview_initial("a.bin", b"\x00\x01").is_some());
    assert!(preview_initial("a.png", b"png").is_none());
    assert!(preview_initial("a.mp4", b"mp4").is_none());
    assert!(preview_initial("a.mp3", b"mp3").is_none());
    assert!(preview_initial("a.pdf", b"pdf").is_none());
}

#[test]
fn preview_blob_url_identifies_blob_uris_only() {
    assert!(preview_blob_url(&Preview::Image("data:image/png;base64,AQID".into())).is_none());
    assert!(preview_blob_url(&Preview::Video("data:video/mp4;base64,AQID".into())).is_none());
    assert_eq!(
        preview_blob_url(&Preview::Image("blob:https://functora/abc".into())),
        Some("blob:https://functora/abc")
    );
    assert_eq!(
        preview_blob_url(&Preview::Pdf("blob:https://functora/def".into())),
        Some("blob:https://functora/def")
    );
    assert!(preview_blob_url(&Preview::Text("hello".into())).is_none());
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
