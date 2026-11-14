use std::path::PathBuf;

fn desktop_gitignore() -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("desktop/.gitignore");
    std::fs::read_to_string(&path).unwrap_or_else(|err| panic!("read desktop gitignore: {err}"))
}

#[test]
fn desktop_gitignore_covers_generated_boilerplate() {
    let ignore = desktop_gitignore();
    for line in ["linux/", "metainfo/", "icons/"] {
        assert!(
            ignore.lines().any(|entry| entry == line),
            "desktop gitignore must cover generated {line}"
        );
    }
}
