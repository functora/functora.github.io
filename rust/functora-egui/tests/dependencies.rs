use std::collections::BTreeSet;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::mpsc;
use std::time::Duration;

const TARGET: &str = "x86_64-unknown-linux-gnu";
const TREE_TIMEOUT: Duration = Duration::from_mins(2);
const CHECK_TIMEOUT: Duration = Duration::from_mins(5);
const POLL_INTERVAL: Duration = Duration::from_millis(10);
const CRATES: &[&str] = &["functora-core", "functora-egui"];
const HEAVY_CORE: &[&str] = &[
    "aes-gcm",
    "ammonia",
    "argon2",
    "chacha20poly1305",
    "image",
    "mp4",
    "pulldown-cmark",
    "rust_h264",
    "rxing",
    "zip",
];
const PLATFORM: &[&str] = &["arboard", "directories", "pollster", "rfd", "threadpool"];
const CORE_CAPABILITIES: &[(&str, &[&str])] = &[
    ("crypto", &["aes-gcm", "argon2", "chacha20poly1305"]),
    ("markdown", &["ammonia", "pulldown-cmark"]),
    ("package", &["aes-gcm", "argon2", "chacha20poly1305", "zip"]),
    ("qr", &["rxing"]),
    ("thumbnail", &["image", "mp4", "rust_h264"]),
    ("zip", &["zip"]),
];
const WIDGET_CAPABILITIES: &[(&str, &[&str])] = &[
    ("clipboard", &["arboard", "image", "pollster", "threadpool"]),
    ("crypto", &["aes-gcm", "argon2", "chacha20poly1305"]),
    ("files", &["pollster", "rfd"]),
    ("html-markdown", &["ammonia", "pulldown-cmark"]),
    ("package", &["aes-gcm", "argon2", "chacha20poly1305", "zip"]),
    ("platform", &[]),
    ("qr", &["rxing"]),
    ("runtime", &["pollster", "threadpool"]),
    ("storage", &["directories"]),
    ("thumbnail", &["image", "mp4", "rust_h264"]),
    ("zip", &["zip"]),
];

#[derive(Debug)]
enum TestError {
    Io(std::io::Error),
    Json(serde_json::Error),
    NoParent,
    Metadata(String),
    Tree {
        command: String,
        stderr: String,
    },
    Check {
        krate: String,
        feature: Option<String>,
        stderr: String,
    },
    Unification {
        stderr: String,
    },
    Timeout {
        command: String,
    },
    Pipe {
        command: String,
    },
}

impl From<std::io::Error> for TestError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

impl From<serde_json::Error> for TestError {
    fn from(error: serde_json::Error) -> Self {
        Self::Json(error)
    }
}

impl std::fmt::Display for TestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(error) => write!(f, "input/output failure: {error}"),
            Self::Json(error) => write!(f, "metadata parse failure: {error}"),
            Self::NoParent => write!(f, "manifest directory has no parent"),
            Self::Metadata(detail) => write!(f, "metadata failure: {detail}"),
            Self::Tree { command, stderr } => {
                write!(f, "dependency tree failed for {command}: {stderr}")
            }
            Self::Check {
                krate,
                feature,
                stderr,
            } => {
                let name = feature.as_deref().unwrap_or("(none)");
                write!(f, "check failed for {krate} {name}: {stderr}")
            }
            Self::Unification { stderr } => write!(f, "feature unification check failed: {stderr}"),
            Self::Timeout { command } => write!(f, "command timed out: {command}"),
            Self::Pipe { command } => write!(f, "output pipe failed for {command}"),
        }
    }
}

fn cargo_binary() -> String {
    std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_owned())
}

fn rust_root() -> Result<PathBuf, TestError> {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .map(Path::to_path_buf)
        .ok_or(TestError::NoParent)
}

fn names(items: &[&str]) -> BTreeSet<String> {
    items.iter().copied().map(str::to_owned).collect()
}

fn error_lines(error: &TestError) -> Vec<String> {
    vec![format!("{error}")]
}

fn drain(
    mut flow: impl Read + Send + 'static,
    sender: mpsc::Sender<std::io::Result<Vec<u8>>>,
) -> std::io::Result<()> {
    drop(
        std::thread::Builder::new()
            .name("cargo-drain".to_owned())
            .spawn(move || {
                let mut bytes = Vec::new();
                drop(sender.send(flow.read_to_end(&mut bytes).map(|_| bytes)));
            })?,
    );
    Ok(())
}

fn run_with_timeout(mut command: Command, timeout: Duration) -> Result<Output, TestError> {
    let label = format!("{command:?}");
    let mut child = command
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let flows = child
        .stdout
        .take()
        .zip(child.stderr.take())
        .ok_or_else(|| TestError::Pipe {
            command: label.clone(),
        })?;
    let (outflow, errflow) = flows;
    let (out_sender, out_receiver) = mpsc::channel();
    let (err_sender, err_receiver) = mpsc::channel();
    drain(outflow, out_sender)?;
    drain(errflow, err_sender)?;
    let ticks = timeout
        .as_millis()
        .div_ceil(POLL_INTERVAL.as_millis())
        .max(1);
    let polled = (0..ticks).find_map(|_| match child.try_wait() {
        Ok(None) => {
            std::thread::sleep(POLL_INTERVAL);
            None
        }
        Ok(Some(status)) => Some(Ok(status)),
        Err(error) => Some(Err(TestError::from(error))),
    });
    match polled {
        Some(Ok(status)) => {
            let stdout = out_receiver.recv().map_err(|_| TestError::Pipe {
                command: label.clone(),
            })??;
            let stderr = err_receiver
                .recv()
                .map_err(|_| TestError::Pipe { command: label })??;
            Ok(Output {
                status,
                stdout,
                stderr,
            })
        }
        Some(Err(error)) => Err(error),
        None => {
            child.kill()?;
            child.wait().map(|_| ())?;
            Err(TestError::Timeout { command: label })
        }
    }
}

fn run_cargo(args: &[String], timeout: Duration) -> Result<Output, TestError> {
    let mut command = Command::new(cargo_binary());
    _ = command.args(args);
    run_with_timeout(command, timeout)
}

fn cargo_tree(krate: &str, extra: &[String]) -> Result<BTreeSet<String>, TestError> {
    let root = rust_root()?;
    let manifest = root
        .join(krate)
        .join("Cargo.toml")
        .to_string_lossy()
        .into_owned();
    let args = ["tree", "--offline", "--locked", "--manifest-path"]
        .into_iter()
        .map(str::to_owned)
        .chain([manifest])
        .chain(
            [
                "--target",
                TARGET,
                "--edges",
                "normal,build",
                "--prefix",
                "none",
                "--format",
                "{p}",
            ]
            .into_iter()
            .map(str::to_owned),
        )
        .chain(extra.iter().cloned())
        .collect::<Vec<String>>();
    run_cargo(&args, TREE_TIMEOUT).and_then(|output| {
        if output.status.success() {
            Ok(String::from_utf8_lossy(&output.stdout)
                .lines()
                .filter_map(|line| line.split_whitespace().next().map(str::to_owned))
                .collect::<BTreeSet<String>>())
        } else {
            Err(TestError::Tree {
                command: args.join(" "),
                stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
            })
        }
    })
}

fn features_of(krate: &str) -> Result<Vec<String>, TestError> {
    let root = rust_root()?;
    let manifest = root
        .join(krate)
        .join("Cargo.toml")
        .to_string_lossy()
        .into_owned();
    let args = [
        "metadata",
        "--format-version",
        "1",
        "--no-deps",
        "--offline",
        "--locked",
        "--manifest-path",
    ]
    .into_iter()
    .map(str::to_owned)
    .chain([manifest])
    .collect::<Vec<String>>();
    run_cargo(&args, TREE_TIMEOUT).and_then(|output| {
        if output.status.success() {
            let metadata: serde_json::Value = serde_json::from_slice(&output.stdout)?;
            metadata
                .get("packages")
                .and_then(serde_json::Value::as_array)
                .ok_or_else(|| TestError::Metadata("metadata is missing packages".to_owned()))
                .and_then(|packages| {
                    packages
                        .iter()
                        .find(|package| {
                            package
                                .get("name")
                                .and_then(serde_json::Value::as_str)
                                .is_some_and(|name| name == krate)
                        })
                        .ok_or_else(|| {
                            TestError::Metadata(format!("metadata is missing package {krate}"))
                        })
                })
                .and_then(|package| {
                    package
                        .get("features")
                        .and_then(serde_json::Value::as_object)
                        .ok_or_else(|| {
                            TestError::Metadata(format!("metadata is missing features for {krate}"))
                        })
                })
                .map(|features| {
                    features
                        .keys()
                        .filter(|key| key.as_str() != "default")
                        .cloned()
                        .collect::<Vec<String>>()
                })
        } else {
            Err(TestError::Metadata(
                String::from_utf8_lossy(&output.stderr).into_owned(),
            ))
        }
    })
}

fn cargo_check(krate: &str, feature: Option<&str>) -> Result<(), TestError> {
    let root = rust_root()?;
    let manifest = root
        .join(krate)
        .join("Cargo.toml")
        .to_string_lossy()
        .into_owned();
    let args = [
        "check",
        "--offline",
        "--locked",
        "--all-targets",
        "--no-default-features",
        "--manifest-path",
    ]
    .into_iter()
    .map(str::to_owned)
    .chain([manifest])
    .chain(
        feature
            .map(|name| ["--features".to_owned(), name.to_owned()])
            .into_iter()
            .flatten(),
    )
    .collect::<Vec<String>>();
    run_cargo(&args, CHECK_TIMEOUT).and_then(|output| {
        if output.status.success() {
            Ok(())
        } else {
            Err(TestError::Check {
                krate: krate.to_owned(),
                feature: feature.map(str::to_owned),
                stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
            })
        }
    })
}

fn feature_args(feature: &str) -> Vec<String> {
    ["--no-default-features", "--features"]
        .into_iter()
        .map(str::to_owned)
        .chain([feature.to_owned()])
        .collect()
}

#[test]
fn features_compile_without_defaults() {
    let failures = CRATES
        .iter()
        .flat_map(|krate| {
            features_of(krate).map_or_else(
                |error| vec![format!("{krate} metadata: {error}")],
                |features| {
                    std::iter::once(None::<String>)
                        .chain(features.into_iter().map(Some))
                        .filter_map(|feature| {
                            let name = feature.as_deref().unwrap_or("(none)");
                            cargo_check(krate, feature.as_deref())
                                .err()
                                .map(|error| format!("{krate} {name}: {error}"))
                        })
                        .collect::<Vec<String>>()
                },
            )
        })
        .collect::<Vec<String>>();
    assert!(
        failures.is_empty(),
        "feature checks failed:\n{}",
        failures.join("\n")
    );
}

fn unification_check() -> Result<(), TestError> {
    let root = rust_root()?;
    let directory = tempfile::tempdir()?;
    let source = directory.path().join("src");
    std::fs::create_dir_all(&source)?;
    std::fs::write(
        source.join("lib.rs"),
        "pub use functora_egui::camera::start_camera;\n",
    )?;
    let egui = serde_json::to_string(&root.join("functora-egui").to_string_lossy())?;
    let core = serde_json::to_string(&root.join("functora-core").to_string_lossy())?;
    std::fs::write(
        directory.path().join("Cargo.toml"),
        format!(
            "[package]\nname = \"feature-unification-test\"\nversion = \"0.0.0\"\nedition = \"2024\"\n[dependencies]\nfunctora-egui = {{ path = {egui}, default-features = false, features = [\"platform\"] }}\nfunctora-core = {{ path = {core} }}\n"
        ),
    )?;
    std::fs::copy(
        root.join("functora-egui").join("Cargo.lock"),
        directory.path().join("Cargo.lock"),
    )
    .map(|_| ())?;
    let target = std::env::var("CARGO_TARGET_DIR").unwrap_or_else(|_| {
        root.join("functora-egui")
            .join("target")
            .to_string_lossy()
            .into_owned()
    });
    let manifest = directory
        .path()
        .join("Cargo.toml")
        .to_string_lossy()
        .into_owned();
    let args = ["check", "--offline", "--lib", "--manifest-path"]
        .into_iter()
        .map(str::to_owned)
        .chain([manifest, "--target-dir".to_owned(), target])
        .collect::<Vec<String>>();
    run_cargo(&args, CHECK_TIMEOUT).and_then(|output| {
        if output.status.success() {
            Ok(())
        } else {
            Err(TestError::Unification {
                stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
            })
        }
    })
}

#[test]
fn core_features_can_be_enabled_by_other_dependents() {
    let failure = unification_check().err().map(|error| format!("{error}"));
    assert!(
        failure.is_none(),
        "feature unification check failed: {failure:?}"
    );
}

#[test]
fn core_minimal() {
    let leaked = cargo_tree("functora-core", &["--no-default-features".to_owned()]).map_or_else(
        |error| error_lines(&error),
        |found| {
            found
                .intersection(&names(HEAVY_CORE))
                .cloned()
                .collect::<Vec<String>>()
        },
    );
    assert!(
        leaked.is_empty(),
        "functora-core minimal build pulls heavy dependencies: {leaked:?}"
    );
}

#[test]
fn widgets_minimal() {
    let mask: BTreeSet<String> = names(HEAVY_CORE).union(&names(PLATFORM)).cloned().collect();
    let leaked = cargo_tree("functora-egui", &["--no-default-features".to_owned()]).map_or_else(
        |error| error_lines(&error),
        |found| found.intersection(&mask).cloned().collect::<Vec<String>>(),
    );
    assert!(
        leaked.is_empty(),
        "functora-egui minimal build pulls heavy dependencies: {leaked:?}"
    );
}

#[test]
fn core_defaults_preserve_capabilities() {
    let missing = cargo_tree("functora-core", &[]).map_or_else(
        |error| error_lines(&error),
        |found| {
            names(HEAVY_CORE)
                .difference(&found)
                .cloned()
                .collect::<Vec<String>>()
        },
    );
    assert!(
        missing.is_empty(),
        "functora-core default build lost capabilities: {missing:?}"
    );
}

#[test]
fn widgets_defaults_preserve_capabilities() {
    let mask: BTreeSet<String> = names(HEAVY_CORE).union(&names(PLATFORM)).cloned().collect();
    let missing = cargo_tree("functora-egui", &[]).map_or_else(
        |error| error_lines(&error),
        |found| mask.difference(&found).cloned().collect::<Vec<String>>(),
    );
    assert!(
        missing.is_empty(),
        "functora-egui default build lost capabilities: {missing:?}"
    );
}

#[test]
fn core_capabilities_are_independent() {
    let heavy = names(HEAVY_CORE);
    let mismatches = CORE_CAPABILITIES
        .iter()
        .filter_map(|(feature, expected)| {
            cargo_tree("functora-core", &feature_args(feature)).map_or_else(
                |error| Some(format!("{feature}: {error}")),
                |found| {
                    let relevant = found
                        .intersection(&heavy)
                        .cloned()
                        .collect::<BTreeSet<String>>();
                    let wanted = names(expected);
                    (relevant != wanted)
                        .then(|| format!("{feature}: expected {wanted:?}, got {relevant:?}"))
                },
            )
        })
        .collect::<Vec<String>>();
    assert!(
        mismatches.is_empty(),
        "core capabilities diverged:\n{}",
        mismatches.join("\n")
    );
}

#[test]
fn widget_capabilities_are_independent() {
    let mask: BTreeSet<String> = names(HEAVY_CORE).union(&names(PLATFORM)).cloned().collect();
    let mismatches = WIDGET_CAPABILITIES
        .iter()
        .filter_map(|(feature, expected)| {
            cargo_tree("functora-egui", &feature_args(feature)).map_or_else(
                |error| Some(format!("{feature}: {error}")),
                |found| {
                    let relevant = found
                        .intersection(&mask)
                        .cloned()
                        .collect::<BTreeSet<String>>();
                    let wanted = names(expected);
                    (relevant != wanted)
                        .then(|| format!("{feature}: expected {wanted:?}, got {relevant:?}"))
                },
            )
        })
        .collect::<Vec<String>>();
    assert!(
        mismatches.is_empty(),
        "widget capabilities diverged:\n{}",
        mismatches.join("\n")
    );
}
