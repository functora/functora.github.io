use functora_dioxus::encoding::download_script;

#[test]
fn download_script_escapes_special_characters() {
    let script = download_script("a\"</script>");
    assert!(!script.contains("</script>"));
    assert!(script.contains("a.download="));
}

#[test]
fn download_script_aborts_on_abort_message() {
    let script = download_script("file.bin");
    assert!(script.contains("m.t==='abort'"));
}
