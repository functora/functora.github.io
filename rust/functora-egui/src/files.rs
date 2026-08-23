pub use functora_core::files::{
    Attachment, Preview, format_size, is_text, mime_for, preview, preview_blob_url, preview_cached,
    preview_initial, preview_key,
};

use crate::error::Error;
use crate::progress::{Job, Stage};
use std::collections::HashMap;
#[cfg(target_arch = "wasm32")]
use std::sync::Arc;
use std::sync::{LazyLock, Mutex};

pub type PickResult = Result<Vec<(String, Vec<u8>)>, String>;

#[derive(Debug, Default)]
pub struct BlobMemo {
    entries: HashMap<(String, u64), String>,
}

impl BlobMemo {
    #[must_use]
    pub fn get(&self, name: &str, data_key: u64) -> Option<&str> {
        self.entries
            .get(&(name.to_string(), data_key))
            .map(String::as_str)
    }

    pub fn insert(&mut self, name: &str, data_key: u64, url: String) {
        _ = self.entries.insert((name.to_string(), data_key), url);
    }

    #[must_use]
    pub fn forget(&mut self, url: &str) -> usize {
        let before = self.entries.len();
        self.entries.retain(|_, cached| cached.as_str() != url);
        before - self.entries.len()
    }
}

static BLOB_URL_MEMO: LazyLock<Mutex<BlobMemo>> = LazyLock::new(|| Mutex::new(BlobMemo::default()));

#[must_use]
pub fn mime_for_name(name: &str) -> Option<&'static str> {
    mime_for(name)
}

pub fn preview_blob(name: &str, data: &[u8]) -> Preview {
    let _ = BLOB_URL_MEMO;
    let mime_opt = mime_for(name);
    if let Some(mime) = mime_opt.filter(|m| is_blob_mime(m)) {
        let data_key = preview_key(data);
        let cached = BLOB_URL_MEMO
            .lock()
            .ok()
            .and_then(|guard| guard.get(name, data_key).map(str::to_string));
        if let Some(url) = cached {
            preview_from_url(mime, url)
        } else {
            let preview = preview(name, data);
            if let Preview::Image(url)
            | Preview::Video(url)
            | Preview::Audio(url)
            | Preview::Pdf(url) = &preview
                && let Ok(mut guard) = BLOB_URL_MEMO.lock()
            {
                guard.insert(name, data_key, url.clone());
            }
            preview
        }
    } else {
        mime_opt.map_or(Preview::Download, |_| preview(name, data))
    }
}

fn is_blob_mime(mime: &str) -> bool {
    mime.starts_with("image/")
        || mime.starts_with("video/")
        || mime.starts_with("audio/")
        || mime == "application/pdf"
}

fn preview_from_url(mime: &str, url: String) -> Preview {
    match mime {
        m if m.starts_with("image/") => Preview::Image(url),
        m if m.starts_with("video/") => Preview::Video(url),
        m if m.starts_with("audio/") => Preview::Audio(url),
        _ => Preview::Pdf(url),
    }
}

pub fn revoke_blob_url(url: &str) -> Result<(), Error> {
    if let Ok(mut guard) = BLOB_URL_MEMO.lock() {
        _ = guard.forget(url);
    }
    #[cfg(target_arch = "wasm32")]
    {
        #[cfg(feature = "web")]
        {
            if let Err(e) = web_sys::Url::revoke_object_url(url) {
                tracing::warn!("Failed to revoke object URL: {e:?}");
            }
        }
    }
    Ok(())
}

pub async fn pick_files(multiple: bool) -> Result<Vec<(String, Vec<u8>)>, Error> {
    pick_files_with_progress(multiple, None).await
}

pub async fn pick_files_with_progress(
    multiple: bool,
    progress: Option<&mut Option<Job<Stage>>>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    std::future::ready(()).await;
    #[cfg(target_os = "android")]
    {
        let _ = (multiple, progress);
        Err(Error::JS("File picker not supported on Android".into()))
    }
    #[cfg(target_arch = "wasm32")]
    {
        let files = pick_via_web(multiple).await?;
        if let Some(slot) = progress {
            *slot = None;
        }
        Ok(files)
    }
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        let files = pick_via_rfd(multiple).await?;
        if let Some(slot) = progress {
            *slot = None;
        }
        Ok(files)
    }
}

#[cfg(target_arch = "wasm32")]
pub fn pick_files_sync_web(multiple: bool) -> Arc<Mutex<Option<PickResult>>> {
    let result: Arc<Mutex<Option<PickResult>>> = Arc::new(Mutex::new(None));
    let window = match web_sys::window() {
        Some(w) => w,
        None => {
            if let Ok(mut guard) = result.lock() {
                *guard = Some(Err("No window".to_owned()));
            }
            return result;
        }
    };
    let document = match window.document() {
        Some(d) => d,
        None => {
            if let Ok(mut guard) = result.lock() {
                *guard = Some(Err("No document".into()));
            }
            return result;
        }
    };
    let input: web_sys::HtmlInputElement = match document
        .create_element("input")
        .map_err(|e| format!("{e:?}"))
        .and_then(|el| {
            use wasm_bindgen::JsCast;
            el.dyn_into::<web_sys::HtmlInputElement>()
                .map_err(|_| "Not an input".to_owned())
        }) {
        Ok(i) => i,
        Err(e) => {
            if let Ok(mut guard) = result.lock() {
                *guard = Some(Err(e));
            }
            return result;
        }
    };
    input.set_type("file");
    input.set_multiple(multiple);
    if input.style().set_property("display", "none").is_err() {
        if let Ok(mut guard) = result.lock() {
            *guard = Some(Err("set_property failed".into()));
        }
        return result;
    }
    let result_clone = Arc::clone(&result);
    let input_clone = input.clone();
    let document_clone = document.clone();
    {
        use wasm_bindgen::JsCast;
        use wasm_bindgen::closure::Closure;
        let closure = Closure::once(move |_event: web_sys::Event| {
            let file_list = input_clone.files();
            let result2 = Arc::clone(&result_clone);
            let input2 = input_clone.clone();
            let document2 = document_clone.clone();
            wasm_bindgen_futures::spawn_local(async move {
                let mut out = Vec::new();
                if let Some(list) = file_list {
                    let len = list.length();
                    for i in 0..len {
                        if let Some(file) = list.get(i) {
                            let name = file.name();
                            let promise = file.array_buffer();
                            match wasm_bindgen_futures::JsFuture::from(promise).await {
                                Ok(buffer) => {
                                    let uint8 = js_sys::Uint8Array::new(&buffer);
                                    let mut vec = vec![0u8; uint8.length() as usize];
                                    uint8.copy_to(&mut vec);
                                    out.push((name, vec));
                                }
                                Err(e) => {
                                    if let Ok(mut guard) = result2.lock() {
                                        *guard = Some(Err(format!("{e:?}")));
                                    }
                                    if let Some(body) = document2.body() {
                                        drop(body.remove_child(&input2));
                                    }
                                    return;
                                }
                            }
                        }
                    }
                }
                if let Ok(mut guard) = result2.lock() {
                    *guard = Some(Ok(out));
                }
                if let Some(body) = document2.body() {
                    drop(body.remove_child(&input2));
                }
            });
        });
        input.set_onchange(Some(closure.as_ref().unchecked_ref()));
        closure.forget();
    }
    if document
        .body()
        .map(|body| body.append_child(&input).is_ok())
        .unwrap_or(false)
    {
        input.click();
    } else if let Ok(mut guard) = result.lock() {
        *guard = Some(Err("No body".into()));
    }
    result
}

#[cfg(target_arch = "wasm32")]
async fn pick_via_web(multiple: bool) -> Result<Vec<(String, Vec<u8>)>, Error> {
    use wasm_bindgen::JsCast;
    use wasm_bindgen::closure::Closure;
    use wasm_bindgen_futures::JsFuture;

    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    let document = window
        .document()
        .ok_or_else(|| Error::JS("No document".into()))?;
    let input: web_sys::HtmlInputElement = document
        .create_element("input")
        .map_err(|e| Error::JS(format!("{e:?}")))?
        .dyn_into()
        .map_err(|_| Error::JS("Not an input".into()))?;
    input.set_type("file");
    input.set_multiple(multiple);
    input
        .style()
        .set_property("display", "none")
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    let promise = js_sys::Promise::new(&mut |resolve, _reject| {
        let resolve_clone = resolve.clone();
        let closure = Closure::once(move |_event: web_sys::Event| {
            drop(resolve_clone.call0(&wasm_bindgen::JsValue::NULL));
        });
        input.set_onchange(Some(closure.as_ref().unchecked_ref()));
        closure.forget();
    });
    drop(
        document
            .body()
            .ok_or_else(|| Error::JS("No body".into()))?
            .append_child(&input)
            .map_err(|e| Error::JS(format!("{e:?}")))?,
    );
    input.click();
    drop(
        JsFuture::from(promise)
            .await
            .map_err(|e| Error::JS(format!("{e:?}")))?,
    );
    let Some(file_list) = input.files() else {
        if let Some(body) = document.body() {
            drop(body.remove_child(&input));
        }
        return Ok(Vec::new());
    };
    let len = file_list.length();
    let mut out = Vec::new();
    for i in 0..len {
        let file = file_list
            .get(i)
            .ok_or_else(|| Error::JS("No file".into()))?;
        let name = file.name();
        let buffer = JsFuture::from(file.array_buffer())
            .await
            .map_err(|e| Error::JS(format!("{e:?}")))?;
        let uint8 = js_sys::Uint8Array::new(&buffer);
        let mut vec = vec![0u8; uint8.length() as usize];
        uint8.copy_to(&mut vec);
        out.push((name, vec));
    }
    if let Some(body) = document.body() {
        drop(body.remove_child(&input));
    }
    Ok(out)
}

#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
async fn pick_via_rfd(multiple: bool) -> Result<Vec<(String, Vec<u8>)>, Error> {
    let dialog = rfd::AsyncFileDialog::new();
    if multiple {
        let handles = dialog.pick_files().await.unwrap_or_default();
        let mut out = Vec::new();
        for handle in handles {
            let name = handle.file_name();
            let data = handle.read().await;
            out.push((name, data));
        }
        Ok(out)
    } else {
        let Some(handle) = dialog.pick_file().await else {
            return Ok(Vec::new());
        };
        let name = handle.file_name();
        let data = handle.read().await;
        Ok(vec![(name, data)])
    }
}

#[must_use]
pub fn data_url_mime(prefix: &str) -> Option<&str> {
    prefix
        .strip_prefix("data:")
        .and_then(|rest| rest.split(';').next())
        .filter(|mime| !mime.is_empty())
}

#[must_use]
pub fn video_thumbnail(url: &str) -> Option<String> {
    let src = match functora_core::thumbnail::cached_thumbnail(url) {
        Some(src) => src,
        None => {
            #[cfg(not(target_arch = "wasm32"))]
            {
                extract_native(url)
            }
            #[cfg(target_arch = "wasm32")]
            {
                let _ = url;
                None
            }
        }
    };
    functora_core::thumbnail::cache_thumbnail(url, src.clone());
    src
}

#[cfg(not(target_arch = "wasm32"))]
fn extract_native(url: &str) -> Option<String> {
    let (prefix, payload) = url.split_once(',').unwrap_or(("", ""));
    let _mime = data_url_mime(prefix)?;
    let bytes = base64::Engine::decode(&base64::engine::general_purpose::STANDARD, payload).ok()?;
    let jpeg = functora_core::thumbnail::video_thumbnail(&bytes)?;
    Some(functora_core::thumbnail::jpeg_data_url(jpeg))
}
