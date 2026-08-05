use std::cell::RefCell;
use std::rc::Rc;

use cryptonote::archive::create_archive_package_async;
use cryptonote::crypto::CipherType;
use cryptonote::storage::PersistentState;
use cryptonote::{
    Attachment, External, ExternalArchive, InfallibleInto, Language, PersistentSignal, PersistentStateStoreExt, Route,
    Store, TemporaryState, TemporaryStateStoreExt, Theme,
};

mod common;
use dioxus::history::History;
use dioxus::history::MemoryHistory;
use dioxus::prelude::*;

thread_local! {
    static PST: RefCell<Option<Store<PersistentState>>> = const { RefCell::new(None) };
}

#[component]
fn Harness(
    path: String,
    note: String,
    attachments: Vec<Attachment>,
    archive: Option<Vec<u8>>,
    attachment: Option<usize>,
) -> Element {
    let tst = Store::new(TemporaryState::default());
    tst.note().set(note);
    tst.attachments().set(attachments);
    tst.attachment().set(attachment);
    match archive {
        Some(bytes) => tst
            .external()
            .set(External::Archive(ExternalArchive::new(bytes).infallible())),
        None => tst.external().set(External::Nothing),
    }
    let _ = use_context_provider(move || tst);
    let pst = PersistentSignal::new(
        Store::new(PersistentState {
            theme: Theme::Light,
            language: Language::Eng,
        }),
        "cryptonote:test",
    );
    let _ = use_context_provider(move || pst);
    provide_context(Rc::new(MemoryHistory::with_initial_path(path)) as Rc<dyn History>);
    rsx! {
        Router::<Route> {}
    }
}

fn mount(
    path: &str,
    note: &str,
    attachments: Vec<Attachment>,
    archive: Option<Vec<u8>>,
    attachment: Option<usize>,
) -> String {
    let mut dom = VirtualDom::new_with_props(
        Harness,
        HarnessProps {
            path: path.to_string(),
            note: note.to_string(),
            attachments,
            archive,
            attachment,
        },
    );
    let edits = dom.rebuild_to_vec();
    format!("{:?}", edits)
}

fn attachment(name: &str) -> Attachment {
    Attachment {
        name: name.into(),
        data: vec![1, 2, 3],
    }
}

#[component]
fn AboutHarness() -> Element {
    let tst = Store::new(TemporaryState::default());
    let _ = use_context_provider(move || tst);
    let pst = PersistentSignal::new(
        Store::new(PersistentState {
            theme: Theme::Light,
            language: Language::Eng,
        }),
        "cryptonote:test",
    );
    PST.with(|c| *c.borrow_mut() = Some(*pst));
    let _ = use_context_provider(move || pst);
    provide_context(Rc::new(MemoryHistory::with_initial_path("/?screen=about")) as Rc<dyn History>);
    rsx! {
        Router::<Route> {}
    }
}

#[test]
fn about_content_follows_language_change() {
    let mut dom = VirtualDom::new_with_props(AboutHarness, ());
    let eng = format!("{:?}", dom.rebuild_to_vec());
    assert!(
        eng.contains("completely serverless"),
        "english about text not rendered: {eng}"
    );
    PST.with(|c| c.borrow().as_ref().unwrap().language().set(Language::Spa));
    let spa = format!("{:?}", dom.render_immediate_to_vec());
    assert!(
        spa.contains("multiplataforma"),
        "spanish about text not rendered: {spa}"
    );
}

#[test]
fn view_shows_attachments_after_decrypt_state() {
    let edits = mount("/?screen=view", "decrypted", vec![attachment("report.pdf")], None, None);
    assert!(edits.contains("report.pdf"), "attachment name not rendered: {edits}");
    assert!(edits.contains("Download all"), "attachment dock not rendered: {edits}");
}

#[test]
fn view_file_names_are_links() {
    let edits = mount("/?screen=view", "decrypted", vec![attachment("photo.jpg")], None, None);
    assert!(
        name_is_clickable(&edits, "photo.jpg"),
        "attachment name is not a link: {edits}"
    );
}

#[test]
fn home_attachment_names_are_links() {
    let edits = mount("/?screen=home", "creating", vec![attachment("photo.jpg")], None, None);
    assert!(
        name_is_clickable(&edits, "photo.jpg"),
        "attachment name is not a link: {edits}"
    );
}

#[test]
fn open_shows_decrypt_form_for_encrypted_archive() {
    let archive = common::with_runtime(|| {
        common::block_on(create_archive_package_async(
            "secret",
            &[attachment("data.bin")],
            "pw",
            Some(CipherType::ChaCha20Poly1305),
            common::progress(),
        ))
    })
    .unwrap();
    let edits = mount("/?screen=open", "", vec![], Some(archive), None);
    assert!(edits.contains("Decrypt"), "decrypt button not rendered: {edits}");
    assert!(!edits.contains("data.bin"), "attachments shown before decrypt: {edits}");
}

fn file(name: &str, data: &[u8]) -> Attachment {
    Attachment {
        name: name.into(),
        data: data.to_vec(),
    }
}

fn name_is_clickable(edits: &str, name: &str) -> bool {
    let text = format!("CreateTextNode {{ value: \"{name}\"");
    let Some(pos) = edits.find(&text) else {
        return false;
    };
    let Some((_, rest)) = edits[pos..].split_once("ReplacePlaceholder { path: ") else {
        return false;
    };
    let Some(path) = rest.split(']').next().map(|p| format!("{p}]")) else {
        return false;
    };
    let Some(parent) = path.strip_suffix(", 0]").map(|p| format!("{p}]")) else {
        return false;
    };
    let head = &edits[..pos];
    let Some(assign) = head.rfind(&format!("AssignId {{ path: {parent}, id: ElementId(")) else {
        return false;
    };
    let Some(id) = head[assign..]
        .split("ElementId(")
        .nth(1)
        .and_then(|s| s.split(')').next())
    else {
        return false;
    };
    head[assign..].contains(&format!("NewEventListener {{ name: \"click\", id: ElementId({id}) }}"))
}

#[test]
fn file_shows_image_viewer() {
    let edits = mount("/?screen=file", "", vec![file("photo.png", &[1, 2, 3])], None, Some(0));
    assert!(
        edits.contains("data:image/png;base64,AQID"),
        "image viewer not rendered: {edits}"
    );
}

#[test]
fn file_shows_video_viewer() {
    let edits = mount("/?screen=file", "", vec![file("clip.mp4", &[1, 2, 3])], None, Some(0));
    assert!(
        edits.contains("data:video/mp4;base64,AQID"),
        "video viewer not rendered: {edits}"
    );
}

#[test]
fn file_shows_audio_viewer() {
    let edits = mount("/?screen=file", "", vec![file("song.mp3", &[1, 2, 3])], None, Some(0));
    assert!(
        edits.contains("data:audio/mpeg;base64,AQID"),
        "audio viewer not rendered: {edits}"
    );
}

#[test]
fn file_shows_pdf_viewer() {
    let edits = mount("/?screen=file", "", vec![file("book.pdf", &[1, 2, 3])], None, Some(0));
    assert!(
        edits.contains("data:application/pdf;base64,AQID"),
        "pdf viewer not rendered: {edits}"
    );
}

#[test]
fn file_shows_text_viewer() {
    let edits = mount(
        "/?screen=file",
        "",
        vec![file("note.txt", b"hello world")],
        None,
        Some(0),
    );
    assert!(edits.contains("hello world"), "text viewer not rendered: {edits}");
}

#[test]
fn file_shows_json_as_text() {
    let edits = mount("/?screen=file", "", vec![file("data.json", b"myjson")], None, Some(0));
    assert!(edits.contains("myjson"), "json viewer not rendered: {edits}");
}

#[test]
fn file_shows_preview_unavailable_for_invalid_text() {
    let edits = mount(
        "/?screen=file",
        "",
        vec![file("note.txt", &[0xff, 0xfe])],
        None,
        Some(0),
    );
    assert!(
        edits.contains("Preview is not available"),
        "unavailable preview message not rendered: {edits}"
    );
}

#[test]
fn file_renders_markdown() {
    let edits = mount("/?screen=file", "", vec![file("readme.md", b"# Title")], None, Some(0));
    assert!(edits.contains("Title"), "markdown viewer not rendered: {edits}");
}

#[test]
fn file_shows_preview_unavailable_for_unknown_type() {
    let edits = mount("/?screen=file", "", vec![attachment("data.bin")], None, Some(0));
    assert!(
        edits.contains("Preview is not available"),
        "unavailable preview message not rendered: {edits}"
    );
}

#[test]
fn file_shows_not_found_without_selection() {
    let edits = mount("/?screen=file", "", vec![attachment("data.bin")], None, None);
    assert!(
        edits.contains("File not found"),
        "not found message not rendered: {edits}"
    );
}

#[test]
fn file_shows_not_found_for_out_of_bounds_index() {
    let edits = mount("/?screen=file", "", vec![attachment("data.bin")], None, Some(5));
    assert!(
        edits.contains("File not found"),
        "not found message not rendered: {edits}"
    );
}
