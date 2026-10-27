//! Text layout discipline: widgets measure and paint text through
//! `functora_egui::text`, whose only unbounded primitive is confined to that
//! module. Any other `layout_no_wrap` call would paint one long line past the
//! box it was given.

use std::path::{Path, PathBuf};

fn rust_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            rust_sources(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "rs") {
            out.push(path);
        }
    }
}

fn read(path: &Path) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|err| panic!("read {}: {err}", path.display()))
}

fn sources_with(needle: &str) -> Vec<PathBuf> {
    let src = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
    let mut files = Vec::new();
    rust_sources(&src, &mut files);
    assert!(
        !files.is_empty(),
        "no sources found under {}",
        src.display()
    );
    files
        .into_iter()
        .filter(|path| read(path).contains(needle))
        .collect()
}

#[test]
fn layout_no_wrap_is_confined_to_the_text_module() {
    let text_module = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src")
        .join("text.rs");
    let users = sources_with("layout_no_wrap");
    assert!(
        users.contains(&text_module),
        "text.rs must own the unbounded layout"
    );
    let offenders: Vec<_> = users
        .into_iter()
        .filter(|path| *path != text_module)
        .collect();
    assert!(
        offenders.is_empty(),
        "these files lay text out without a width limit: {offenders:?}"
    );
}

#[test]
fn widgets_measure_their_text_through_the_text_module() {
    let users = sources_with("crate::text::layout(");
    assert!(
        users.len() >= 20,
        "expected the shared layout to be the common path, found it in {} file(s)",
        users.len()
    );
}
