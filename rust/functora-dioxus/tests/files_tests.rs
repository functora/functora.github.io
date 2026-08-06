use functora_dioxus::files::{Attachment, format_size, is_text, mime_for, pick_script};

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
