use dioxus::{logger::tracing, prelude::*};
use serde::de::DeserializeOwned;
use serde_json::Value;
use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
    sync::OnceLock,
};

const FILENAME: &str = "storage.json";
const STORAGE: OnceLock<std::path::PathBuf> =
    OnceLock::new();

pub fn storage_path() -> PathBuf {
    STORAGE.get_or_init(|| {
        // aquí ya llamas a tu path::files_dir()
        let dir = files_dir();
        let file_path = dir.join(FILENAME);

        let mut file = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&file_path)
            .expect("Failed to open storage file");

        let metadata = file.metadata().expect("Failed to get file metadata");
        if metadata.len() == 0 {
            file.write_all(b"{}").expect("Failed to write initial JSON to storage file");
        }

        file_path
    }).clone()
}

pub fn update_key<P: AsRef<Path>, T: serde::Serialize>(
    path: P,
    key: &str,
    new_value: T,
) -> anyhow::Result<()> {
    // 1. read whole file
    let data = fs::read_to_string(&path)?;
    // 2. parse
    let mut json: Value = serde_json::from_str(&data)?;

    // 3. update
    if let Some(obj) = json.as_object_mut() {
        obj.insert(
            key.to_string(),
            serde_json::to_value(new_value)?,
        );
    } else {
        anyhow::bail!("root is not an object");
    }

    // 4. write whole file again
    fs::write(path, serde_json::to_string_pretty(&json)?)?;
    Ok(())
}

pub fn find_or_init_key<
    P: AsRef<Path>,
    T: serde::de::DeserializeOwned + Clone + serde::Serialize,
    F: FnOnce() -> T,
>(
    path: P,
    key: &str,
    init: F,
) -> anyhow::Result<T> {
    // 1. read whole file
    let data = fs::read_to_string(&path)?;
    let json: Value = serde_json::from_str(&data)?;

    // 2. find key
    if let Some(value) = json.get(key) {
        Ok(serde_json::from_value(value.clone())?)
    } else {
        // 3. key not found, initialize
        let new_value = init();
        update_key(&path, key, new_value.clone())?;
        Ok(new_value)
    }
}

pub fn use_storage<
    T: serde::Serialize + DeserializeOwned + Clone + 'static,
>(
    key: &str,
    init: impl FnOnce() -> T,
) -> Signal<T> {
    let path = storage_path();

    let signal = Signal::new(
        find_or_init_key(&path, key, init)
            .expect("Failed to update storage file"),
    );
    let key = key.to_string(); // Clone the key to own it

    {
        use_effect(move || {
            let current_value = &*signal.read();
            if let Err(e) =
                update_key(&path, &key, current_value)
            {
                tracing::error!(
                    "Failed to save key '{}' to storage: {}",
                    key,
                    e
                );
            }
        });
    }

    signal
}

#[cfg(target_os = "android")]
pub fn files_dir() -> std::path::PathBuf {
    use jni::JNIEnv;
    use jni::objects::{JObject, JString};
    use std::sync::mpsc::channel;

    let (tx, rx) = channel();

    dioxus::mobile::wry::prelude::dispatch(
        move |env: &mut JNIEnv,
              activity: &JObject,
              _webview| {
            let files_dir = env
                .call_method(
                    activity,
                    "getFilesDir",
                    "()Ljava/io/File;",
                    &[],
                )
                .unwrap()
                .l()
                .unwrap();

            let abs_path = env
                .call_method(
                    files_dir,
                    "getAbsolutePath",
                    "()Ljava/lang/String;",
                    &[],
                )
                .unwrap()
                .l()
                .unwrap();

            let abs_path: JString = abs_path.into();
            let abs_path: String =
                env.get_string(&abs_path).unwrap().into();

            tx.send(std::path::PathBuf::from(abs_path))
                .unwrap();
        },
    );

    rx.recv().unwrap()
}

#[cfg(target_os = "ios")]
pub fn files_dir() -> std::path::PathBuf {
    use std::path::PathBuf;

    let home =
        std::env::var("HOME").expect("HOME not set on iOS");
    std::path::PathBuf::from(home).join("Documents")
}

#[cfg(not(any(target_os = "android", target_os = "ios")))]
pub fn files_dir() -> std::path::PathBuf {
    std::env::current_dir()
        .expect("Failed to get current directory")
}
