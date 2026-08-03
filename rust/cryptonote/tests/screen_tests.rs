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
fn Harness(path: String, note: String, attachments: Vec<Attachment>, archive: Option<Vec<u8>>) -> Element {
    let tst = Store::new(TemporaryState::default());
    tst.note().set(note);
    tst.attachments().set(attachments);
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

fn mount(path: &str, note: &str, attachments: Vec<Attachment>, archive: Option<Vec<u8>>) -> String {
    let mut dom = VirtualDom::new_with_props(
        Harness,
        HarnessProps {
            path: path.to_string(),
            note: note.to_string(),
            attachments,
            archive,
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
    let edits = mount("/?screen=view", "decrypted", vec![attachment("report.pdf")], None);
    assert!(edits.contains("report.pdf"), "attachment name not rendered: {edits}");
    assert!(edits.contains("Download all"), "attachment dock not rendered: {edits}");
}

#[test]
fn open_shows_note_and_attachments_after_decrypt_state() {
    let edits = mount("/?screen=open", "decrypted", vec![attachment("photo.jpg")], None);
    assert!(edits.contains("photo.jpg"), "attachment name not rendered: {edits}");
    assert!(!edits.contains("Decrypt"), "decrypt form still rendered: {edits}");
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
    let edits = mount("/?screen=open", "", vec![], Some(archive));
    assert!(edits.contains("Decrypt"), "decrypt button not rendered: {edits}");
    assert!(!edits.contains("data.bin"), "attachments shown before decrypt: {edits}");
}
