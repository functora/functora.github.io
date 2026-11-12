use std::path::PathBuf;

fn crate_gitignore() -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(".gitignore");
    std::fs::read_to_string(&path).unwrap_or_else(|err| panic!("read crate gitignore: {err}"))
}

#[test]
fn android_gitignore_ignores_android_dir() {
    let ignore = crate_gitignore();
    assert!(
        ignore.lines().any(|entry| entry == "android/"),
        "crate gitignore must ignore android/"
    );
}
