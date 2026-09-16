#![allow(clippy::unwrap_used, clippy::expect_used)]
use std::path::PathBuf;

fn android_gitignore() -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("android/.gitignore");
    std::fs::read_to_string(&path).expect("read android gitignore")
}

#[test]
fn android_gitignore_mirrors_demo_verbatim() {
    let ours = android_gitignore();
    let demo = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../functora-egui-demo/android/.gitignore");
    let expected = std::fs::read_to_string(&demo).expect("read demo android gitignore");
    assert_eq!(
        ours, expected,
        "cryptonote android gitignore must mirror the demo file verbatim"
    );
}

#[test]
fn android_gitignore_covers_generated_boilerplate() {
    let ignore = android_gitignore();
    for line in [
        "app/build.gradle",
        "app/src/main/AndroidManifest.xml",
        "app/src/main/java/",
        "app/src/main/res/values/styles.xml",
        "build.gradle",
        "settings.gradle",
        "gradle.properties",
        "app/build/",
        "app/src/main/jniLibs/",
        "app/src/main/res/mipmap-*/ic_launcher.png",
    ] {
        assert!(
            ignore.lines().any(|entry| entry == line),
            "android gitignore must cover generated {line}"
        );
    }
}
