use std::path::PathBuf;

#[test]
fn desktop_gitignore_mirrors_demo_verbatim() {
    let ours_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("desktop/.gitignore");
    let ours = std::fs::read_to_string(&ours_path).unwrap_or_else(|err| panic!("read desktop gitignore: {err}"));
    let demo_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../functora-egui-demo/desktop/.gitignore");
    let expected =
        std::fs::read_to_string(&demo_path).unwrap_or_else(|err| panic!("read demo desktop gitignore: {err}"));
    assert_eq!(
        ours, expected,
        "cryptonote desktop gitignore must mirror the demo file verbatim"
    );
}
