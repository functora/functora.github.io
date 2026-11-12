pub use functora_core::files::{
    Attachment, Preview, format_size, is_text, mime_for, preview, preview_blob_url, preview_cached,
    preview_initial, preview_key,
};

use crate::error::Error;
#[cfg(target_arch = "wasm32")]
use crate::error::Missing;
use crate::progress::yield_to_paint;
use crate::progress::{Job, Stage};
#[cfg(target_arch = "wasm32")]
use base64::Engine as _;
use std::collections::{HashMap, VecDeque};
#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
use std::path::PathBuf;
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use std::sync::{LazyLock, Mutex};

#[cfg(not(target_os = "android"))]
const PICK_CHUNK: usize = 4 * 1024 * 1024;

pub type PickResult = Result<Vec<(String, Vec<u8>)>, String>;
pub type CancelToken = Arc<AtomicBool>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FilePickerState {
    Pending,
    Done,
    Cancelled,
    Failed(i32),
}

impl From<i32> for FilePickerState {
    fn from(code: i32) -> Self {
        match code {
            0 => Self::Pending,
            1 => Self::Done,
            -1 => Self::Cancelled,
            other => Self::Failed(other),
        }
    }
}

#[must_use]
pub fn new_cancel_token() -> CancelToken {
    Arc::new(AtomicBool::new(false))
}

pub fn cancel(token: &CancelToken) {
    token.store(true, Ordering::Relaxed);
}

#[must_use]
pub fn is_cancelled(token: &CancelToken) -> bool {
    token.load(Ordering::Relaxed)
}

const MEMO_CAPACITY: usize = 128;

#[derive(Debug, Default)]
pub struct BlobMemo {
    entries: HashMap<(String, u64), String>,
    order: VecDeque<(String, u64)>,
}

impl BlobMemo {
    #[must_use]
    pub fn get(&self, name: &str, data_key: u64) -> Option<&str> {
        self.entries
            .get(&(name.to_string(), data_key))
            .map(String::as_str)
    }

    pub fn insert(&mut self, name: &str, data_key: u64, url: String) -> Vec<String> {
        if url.starts_with("data:") {
            return Vec::new();
        }
        let key = (name.to_string(), data_key);
        let replaced = self.entries.insert(key.clone(), url);
        if let Some(old) = replaced {
            vec![old]
        } else {
            self.order.push_back(key);
            (0..self.entries.len().saturating_sub(MEMO_CAPACITY))
                .filter_map(|_| self.order.pop_front())
                .filter_map(|old_key| self.entries.remove(&old_key))
                .collect()
        }
    }

    #[must_use]
    pub fn forget(&mut self, url: &str) -> usize {
        let before = self.entries.len();
        self.entries.retain(|_, cached| cached.as_str() != url);
        let removed = before - self.entries.len();
        if removed > 0 {
            self.order.retain(|key| self.entries.contains_key(key));
        }
        removed
    }
}

static BLOB_URL_MEMO: LazyLock<Mutex<BlobMemo>> = LazyLock::new(|| Mutex::new(BlobMemo::default()));

#[must_use]
pub fn mime_for_name(name: &str) -> Option<&'static str> {
    mime_for(name)
}

pub fn preview_blob(name: &str, data: &[u8]) -> Preview {
    const BLOB_PREVIEW_LIMIT: usize = 20 * 1024 * 1024;
    if data.len() > BLOB_PREVIEW_LIMIT {
        return Preview::Download;
    }
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
            #[cfg(target_arch = "wasm32")]
            {
                let url = {
                    let array = js_sys::Uint8Array::from(data);
                    let parts = js_sys::Array::new();
                    let _ = parts.push(&array.buffer());
                    let bag = web_sys::BlobPropertyBag::new();
                    bag.set_type(mime);
                    web_sys::Blob::new_with_u8_array_sequence_and_options(&parts, &bag)
                        .ok()
                        .and_then(|blob| web_sys::Url::create_object_url_with_blob(&blob).ok())
                        .unwrap_or_else(|| {
                            format!(
                                "data:{mime};base64,{}",
                                base64::engine::general_purpose::STANDARD.encode(data)
                            )
                        })
                };
                if !url.starts_with("data:")
                    && let Ok(mut guard) = BLOB_URL_MEMO.lock()
                {
                    for old in guard.insert(name, data_key, url.clone()) {
                        if let Err(e) = web_sys::Url::revoke_object_url(&old) {
                            tracing::warn!("Failed to revoke object URL: {e:?}");
                        }
                    }
                }
                preview_from_url(mime, url)
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                let preview = preview(name, data);
                if let Preview::Image(url)
                | Preview::Video(url)
                | Preview::Audio(url)
                | Preview::Pdf(url) = &preview
                    && !url.starts_with("data:")
                    && let Ok(mut guard) = BLOB_URL_MEMO.lock()
                {
                    drop(guard.insert(name, data_key, url.clone()));
                }
                preview
            }
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

/// The backend that shows the desktop file picker.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PickerBackend {
    /// The XDG desktop portal, which shows the desktop's own file chooser.
    Portal,
    /// The platform dialog (GTK 3 on Linux), used when the session has no portal.
    Native,
    /// No backend can run: the session has neither a portal nor a display.
    Unavailable,
}

/// Chooses the backend that shows the desktop file picker.
///
/// The portal is preferred because it shows the desktop's own file chooser;
/// the platform dialog is the fallback for sessions without a portal.
#[must_use]
pub fn picker_backend(portal_available: bool, display_available: bool) -> PickerBackend {
    match (portal_available, display_available) {
        (true, _) => PickerBackend::Portal,
        (false, true) => PickerBackend::Native,
        (false, false) => PickerBackend::Unavailable,
    }
}

/// Reports whether a graphical display is reachable for the platform dialog.
///
/// The GTK 3 dialog blocks forever when GTK cannot open a display, so the
/// desktop picker only uses it when a display is announced.
#[must_use]
pub fn desktop_display_available() -> bool {
    std::env::var_os("WAYLAND_DISPLAY").is_some() || std::env::var_os("DISPLAY").is_some()
}

pub async fn pick_files_with_progress(
    multiple: bool,
    progress: Option<&mut Option<Job<Stage>>>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    pick_files_with_cancel(multiple, progress, None).await
}

pub async fn pick_files_with_cancel(
    multiple: bool,
    progress: Option<&mut Option<Job<Stage>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    std::future::ready(()).await;
    if let Some(token) = cancel
        && token.load(Ordering::Relaxed)
    {
        return Err(Error::Cancelled);
    }
    #[cfg(target_os = "android")]
    {
        let files = pick_via_android(multiple, progress, cancel).await?;
        Ok(files)
    }
    #[cfg(target_arch = "wasm32")]
    {
        let files = pick_via_web(multiple, progress, cancel).await?;
        Ok(files)
    }
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        let files = pick_via_desktop(multiple, progress, cancel).await?;
        Ok(files)
    }
}

pub async fn pick_files_with_shared_progress(
    multiple: bool,
    progress: Option<Arc<Mutex<Option<Job<Stage>>>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    std::future::ready(()).await;
    if let Some(token) = cancel
        && token.load(Ordering::Relaxed)
    {
        return Err(Error::Cancelled);
    }
    #[cfg(target_os = "android")]
    {
        let files = pick_via_android_shared(multiple, progress, cancel).await?;
        Ok(files)
    }
    #[cfg(target_arch = "wasm32")]
    {
        let files = pick_via_web_shared(multiple, progress, cancel).await?;
        Ok(files)
    }
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        let files = pick_via_desktop_shared(multiple, progress, cancel).await?;
        Ok(files)
    }
}

#[cfg(target_os = "android")]
async fn pick_via_android(
    multiple: bool,
    progress: Option<&mut Option<Job<Stage>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    let shared = progress
        .as_ref()
        .map(|p| Arc::new(Mutex::new((*p).clone())));
    let result = pick_via_android_shared(multiple, shared.clone(), cancel).await;
    if let (Some(slot), Some(arc)) = (progress, shared)
        && let Ok(guard) = arc.lock()
    {
        slot.clone_from(&guard);
    }
    result
}

#[cfg(target_os = "android")]
async fn pick_via_android_shared(
    multiple: bool,
    progress: Option<Arc<Mutex<Option<Job<Stage>>>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    if let Some(shared) = progress.as_ref()
        && let Ok(mut guard) = shared.lock()
    {
        *guard = Some(Job {
            stage: Stage::Attach,
            done: 0,
            total: 1,
            name: None,
        });
    }
    yield_to_paint().await;
    let files = android_pick_files(multiple, cancel).await?;
    let total: u64 = files.iter().map(|(_, data)| data.len() as u64).sum();
    if let Some(shared) = progress.as_ref()
        && let Ok(mut guard) = shared.lock()
    {
        *guard = Some(Job {
            stage: Stage::Attach,
            done: total,
            total: total.max(1),
            name: None,
        });
    }
    yield_to_paint().await;
    if let Some(shared) = progress
        && let Ok(mut guard) = shared.lock()
    {
        *guard = None;
    }
    Ok(files)
}

#[cfg(target_os = "android")]
fn clear_file_picker() {
    _ = crate::platform::android::with_app(|env, activity| {
        let _ = env.call_method(activity, "filePickerClear", "()V", &[])?;
        Ok(())
    });
}

#[cfg(target_os = "android")]
async fn android_pick_files(
    multiple: bool,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    use jni::objects::{JByteArray, JObjectArray, JString, JValue};
    use std::time::{Duration, Instant};
    const PICKER_TIMEOUT: Duration = Duration::from_mins(5);
    if let Some(token) = cancel
        && token.load(Ordering::Relaxed)
    {
        return Err(Error::Cancelled);
    }
    crate::platform::android::with_app(|env, activity| {
        let _ = env.call_method(
            activity,
            "filePickerStart",
            "(Z)V",
            &[JValue::Bool(u8::from(multiple))],
        )?;
        Ok(())
    })?;
    let deadline = Instant::now() + PICKER_TIMEOUT;
    loop {
        if Instant::now() >= deadline {
            clear_file_picker();
            return Err(Error::FilePickerTimedOut);
        }
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            clear_file_picker();
            return Err(Error::Cancelled);
        }
        let state = FilePickerState::from(crate::platform::android::with_app(|env, activity| {
            let v = env
                .call_method(activity, "filePickerState", "()I", &[])?
                .i()?;
            Ok(v)
        })?);
        match state {
            FilePickerState::Pending => {
                yield_to_paint().await;
                std::thread::sleep(Duration::from_millis(80));
            }
            FilePickerState::Done => break,
            FilePickerState::Cancelled => {
                clear_file_picker();
                return Err(Error::Cancelled);
            }
            FilePickerState::Failed(code) => {
                clear_file_picker();
                tracing::warn!("File picker reported unexpected state {code}");
                return Err(Error::FilePickerFailed);
            }
        }
    }
    let names: Vec<String> = crate::platform::android::with_app(|env, activity| {
        let obj = env
            .call_method(activity, "filePickerNames", "()[Ljava/lang/String;", &[])?
            .l()?;
        if obj.is_null() {
            return Ok(Vec::new());
        }
        let arr = JObjectArray::from(obj);
        let len = usize::try_from(env.get_array_length(&arr)?).unwrap_or(0);
        let mut out = Vec::with_capacity(len);
        for i in 0..len {
            let jobj = env.get_object_array_element(&arr, i32::try_from(i).unwrap_or(0))?;
            if jobj.is_null() {
                out.push("file".to_owned());
                continue;
            }
            let jstr = JString::from(jobj);
            let s: String = env.get_string(&jstr)?.into();
            out.push(s);
        }
        Ok(out)
    })?;
    let datas: Vec<Vec<u8>> = crate::platform::android::with_app(|env, activity| {
        let obj = env
            .call_method(activity, "filePickerBytes", "()[[B", &[])?
            .l()?;
        if obj.is_null() {
            return Ok(Vec::new());
        }
        let outer = JObjectArray::from(obj);
        let len = usize::try_from(env.get_array_length(&outer)?).unwrap_or(0);
        let mut out = Vec::with_capacity(len);
        for i in 0..len {
            let inner_obj = env.get_object_array_element(&outer, i32::try_from(i).unwrap_or(0))?;
            if inner_obj.is_null() {
                out.push(Vec::new());
                continue;
            }
            let arr = JByteArray::from(inner_obj);
            let bytes = env.convert_byte_array(arr)?;
            out.push(bytes);
        }
        Ok(out)
    })?;
    clear_file_picker();
    let out: Vec<(String, Vec<u8>)> = names.into_iter().zip(datas).collect();
    if out.is_empty() {
        return Err(Error::Cancelled);
    }
    Ok(out)
}

#[cfg(target_arch = "wasm32")]
struct DomRemoval(web_sys::Element);

#[cfg(target_arch = "wasm32")]
impl DomRemoval {
    fn new(input: &web_sys::HtmlInputElement) -> Self {
        Self(input.clone().into())
    }
}

#[cfg(target_arch = "wasm32")]
impl Drop for DomRemoval {
    fn drop(&mut self) {
        if let Some(parent) = self.0.parent_node() {
            drop(parent.remove_child(&self.0));
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub fn pick_files_sync_web_with_cancel(
    multiple: bool,
    cancel: &CancelToken,
) -> Arc<Mutex<Option<PickResult>>> {
    let result: Arc<Mutex<Option<PickResult>>> = Arc::new(Mutex::new(None));
    let Some(window) = web_sys::window() else {
        if let Ok(mut guard) = result.lock() {
            *guard = Some(Err("No window".to_owned()));
        }
        return result;
    };
    let Some(document) = window.document() else {
        if let Ok(mut guard) = result.lock() {
            *guard = Some(Err("No document".into()));
        }
        return result;
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
    let cancel_onchange = Arc::clone(cancel);
    {
        use wasm_bindgen::JsCast;
        use wasm_bindgen::closure::Closure;
        let closure = Closure::once(move |_event: web_sys::Event| {
            let file_list = input_clone.files();
            let result2 = Arc::clone(&result_clone);
            let input2 = input_clone.clone();
            let cancel_inner = Arc::clone(&cancel_onchange);
            wasm_bindgen_futures::spawn_local(async move {
                let _cleanup = DomRemoval::new(&input2);
                if cancel_inner.load(Ordering::Relaxed) {
                    if let Ok(mut guard) = result2.lock() {
                        *guard = Some(Err("Cancelled".to_owned()));
                    }
                    return;
                }
                let outcome = match file_list {
                    Some(list) => collect_files_chunked_web(list, None, Some(&cancel_inner))
                        .await
                        .map_err(|e| e.to_string()),
                    None => Err("Cancelled".to_owned()),
                };
                if let Ok(mut guard) = result2.lock() {
                    *guard = Some(outcome);
                }
            });
        });
        input.set_onchange(Some(closure.as_ref().unchecked_ref()));
        closure.forget();
    }
    {
        use wasm_bindgen::JsCast;
        use wasm_bindgen::closure::Closure;
        let result_cancel = Arc::clone(&result);
        let input_cancel = input.clone();
        let document_cancel = document.clone();
        let closure_cancel = Closure::once(move |_event: web_sys::Event| {
            if let Ok(mut guard) = result_cancel.lock()
                && guard.is_none()
            {
                *guard = Some(Err("Cancelled".to_owned()));
            }
            if let Some(body) = document_cancel.body() {
                drop(body.remove_child(&input_cancel));
            }
        });
        drop(
            input.add_event_listener_with_callback(
                "cancel",
                closure_cancel.as_ref().unchecked_ref(),
            ),
        );
        closure_cancel.forget();
    }
    let appended = document
        .body()
        .is_some_and(|body| body.append_child(&input).is_ok());
    if appended {
        input.click();
    } else if let Ok(mut guard) = result.lock() {
        *guard = Some(Err("No body".into()));
    }
    result
}

#[cfg(target_arch = "wasm32")]
async fn collect_files_chunked_web(
    file_list: web_sys::FileList,
    progress: Option<&mut Option<Job<Stage>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    let shared = progress
        .as_ref()
        .map(|p| Arc::new(Mutex::new((*p).clone())));
    let result = collect_files_chunked_web_shared(file_list, shared.clone(), cancel).await;
    if let (Some(slot), Some(arc)) = (progress, shared)
        && let Ok(guard) = arc.lock()
    {
        slot.clone_from(&guard);
    }
    result
}

#[cfg(target_arch = "wasm32")]
async fn pick_via_web(
    multiple: bool,
    progress: Option<&mut Option<Job<Stage>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    let shared = progress
        .as_ref()
        .map(|p| Arc::new(Mutex::new((*p).clone())));
    let result = pick_via_web_shared(multiple, shared.clone(), cancel).await;
    if let (Some(slot), Some(arc)) = (progress, shared)
        && let Ok(guard) = arc.lock()
    {
        slot.clone_from(&guard);
    }
    result
}

#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
async fn pick_via_desktop(
    multiple: bool,
    progress: Option<&mut Option<Job<Stage>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    let shared = progress
        .as_ref()
        .map(|p| Arc::new(Mutex::new((*p).clone())));
    let result = pick_via_desktop_shared(multiple, shared.clone(), cancel).await;
    if let (Some(slot), Some(arc)) = (progress, shared)
        && let Ok(guard) = arc.lock()
    {
        slot.clone_from(&guard);
    }
    result
}

#[cfg(target_arch = "wasm32")]
async fn collect_files_chunked_web_shared(
    file_list: web_sys::FileList,
    progress: Option<Arc<Mutex<Option<Job<Stage>>>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    use wasm_bindgen::JsCast;
    let len = file_list.length();
    if len == 0 {
        if let Some(shared) = progress.as_ref()
            && let Ok(mut guard) = shared.lock()
        {
            *guard = None;
        }
        return Err(Error::Cancelled);
    }
    let mut total: u64 = 0;
    for i in 0..len {
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            return Err(Error::Cancelled);
        }
        let file = file_list
            .get(i)
            .ok_or_else(|| Error::Missing(Missing::FileEntry))?;
        let blob: &web_sys::Blob = file.unchecked_ref();
        let size = crate::utils::f64_to_u64_clamped(blob.size());
        total = total.saturating_add(size);
    }
    if let Some(shared) = progress.as_ref()
        && let Ok(mut guard) = shared.lock()
    {
        *guard = Some(Job {
            stage: Stage::Attach,
            done: 0,
            total: total.max(1),
            name: None,
        });
    }
    yield_to_paint().await;
    let mut out: Vec<(String, Vec<u8>)> = Vec::with_capacity(len as usize);
    let mut done: u64 = 0;
    for i in 0..len {
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            return Err(Error::Cancelled);
        }
        let file = file_list
            .get(i)
            .ok_or_else(|| Error::Missing(Missing::FileEntry))?;
        let name = file.name();
        let data = read_single_file_chunked_shared(
            &file,
            progress.as_ref().map(Arc::clone),
            total,
            &mut done,
            &name,
            cancel,
        )
        .await?;
        out.push((name, data));
    }
    if let Some(shared) = progress
        && let Ok(mut guard) = shared.lock()
    {
        *guard = None;
    }
    Ok(out)
}

#[cfg(target_arch = "wasm32")]
async fn read_single_file_chunked_shared(
    file: &web_sys::File,
    progress: Option<Arc<Mutex<Option<Job<Stage>>>>>,
    total: u64,
    done: &mut u64,
    name: &str,
    cancel: Option<&CancelToken>,
) -> Result<Vec<u8>, Error> {
    use wasm_bindgen::JsCast;
    let blob: &web_sys::Blob = file.unchecked_ref();
    let size = crate::utils::f64_to_u64_clamped(blob.size());
    yield_to_paint().await;
    if size == 0 {
        return Ok(Vec::new());
    }
    let size_usize = usize::try_from(size).map_err(|source| Error::Convert {
        context: "file size",
        source,
    })?;
    let mut buf = vec![0u8; size_usize];
    let mut offset: u64 = 0;
    let mut write_pos: usize = 0;
    while offset < size {
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            return Err(Error::Cancelled);
        }
        let end = offset
            .saturating_add(u64::from(u32::try_from(PICK_CHUNK).unwrap_or(u32::MAX)))
            .min(size);
        let chunk_blob = blob
            .slice_with_f64_and_f64(
                crate::utils::u64_to_f64_js(offset),
                crate::utils::u64_to_f64_js(end),
            )
            .map_err(Error::from)?;
        let promise = chunk_blob.array_buffer();
        let buffer = wasm_bindgen_futures::JsFuture::from(promise)
            .await
            .map_err(Error::from)?;
        let uint8 = js_sys::Uint8Array::new(&buffer);
        let chunk_len = uint8.length() as usize;
        if chunk_len == 0 {
            offset = end;
            continue;
        }
        uint8.copy_to(&mut buf[write_pos..write_pos + chunk_len]);
        write_pos += chunk_len;
        offset = end;
        *done = done.saturating_add(chunk_len as u64);
        if let Some(shared) = progress.as_ref()
            && let Ok(mut guard) = shared.lock()
        {
            *guard = Some(Job {
                stage: Stage::Attach,
                done: *done,
                total: total.max(1),
                name: Some(name.to_string()),
            });
        }
        yield_to_paint().await;
    }
    buf.truncate(write_pos);
    Ok(buf)
}

#[cfg(target_arch = "wasm32")]
async fn pick_via_web_shared(
    multiple: bool,
    progress: Option<Arc<Mutex<Option<Job<Stage>>>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    use wasm_bindgen::JsCast;
    use wasm_bindgen::closure::Closure;
    use wasm_bindgen_futures::JsFuture;

    let window = web_sys::window().ok_or_else(|| Error::Missing(Missing::Window))?;
    let document = window
        .document()
        .ok_or_else(|| Error::Missing(Missing::Document))?;
    let input: web_sys::HtmlInputElement = document
        .create_element("input")
        .map_err(Error::from)?
        .dyn_into()
        .map_err(|_| Error::Missing(Missing::Input))?;
    input.set_type("file");
    input.set_multiple(multiple);
    input
        .style()
        .set_property("display", "none")
        .map_err(Error::from)?;
    let promise = {
        let input_clone = input.clone();
        js_sys::Promise::new(&mut |resolve, _reject| {
            let resolve_change = resolve.clone();
            let closure_change = Closure::once(move |_event: web_sys::Event| {
                drop(resolve_change.call0(&wasm_bindgen::JsValue::NULL));
            });
            input_clone.set_onchange(Some(closure_change.as_ref().unchecked_ref()));
            closure_change.forget();
            let input_cancel = input.clone();
            let resolve_cancel = resolve.clone();
            let closure_cancel = Closure::once(move |_event: web_sys::Event| {
                drop(resolve_cancel.call0(&wasm_bindgen::JsValue::NULL));
            });
            drop(input_cancel.add_event_listener_with_callback(
                "cancel",
                closure_cancel.as_ref().unchecked_ref(),
            ));
            closure_cancel.forget();
        })
    };
    drop(
        document
            .body()
            .ok_or_else(|| Error::Missing(Missing::Body))?
            .append_child(&input)
            .map_err(Error::from)?,
    );
    let _cleanup = DomRemoval::new(&input);
    input.click();
    drop(JsFuture::from(promise).await.map_err(Error::from)?);
    if let Some(token) = cancel
        && token.load(Ordering::Relaxed)
    {
        if let Some(shared) = progress.as_ref()
            && let Ok(mut guard) = shared.lock()
        {
            *guard = None;
        }
        return Err(Error::Cancelled);
    }
    let Some(file_list) = input.files().filter(|files| files.length() > 0) else {
        if let Some(shared) = progress.as_ref()
            && let Ok(mut guard) = shared.lock()
        {
            *guard = None;
        }
        return Err(Error::Cancelled);
    };
    collect_files_chunked_web_shared(file_list, progress, cancel).await
}

#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
async fn pick_via_desktop_shared(
    multiple: bool,
    progress: Option<Arc<Mutex<Option<Job<Stage>>>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    if let Some(token) = cancel
        && token.load(Ordering::Relaxed)
    {
        return Err(Error::Cancelled);
    }
    let selected = pick_paths(multiple).await?;
    read_selected_files(selected, progress, cancel).await
}

#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
async fn pick_paths(multiple: bool) -> Result<Vec<PathBuf>, Error> {
    let display = platform_display_available();
    #[cfg(target_os = "linux")]
    {
        match picker_backend(portal_available().await, display) {
            PickerBackend::Portal => match pick_via_portal(multiple).await {
                PortalOutcome::Selected(paths) => Ok(paths),
                PortalOutcome::Cancelled => Err(Error::Cancelled),
                PortalOutcome::Unavailable if display => pick_via_native(multiple).await,
                PortalOutcome::Unavailable => Err(Error::FilePickerFailed),
            },
            PickerBackend::Native => pick_via_native(multiple).await,
            PickerBackend::Unavailable => Err(Error::FilePickerFailed),
        }
    }
    #[cfg(not(target_os = "linux"))]
    {
        if !display {
            return Err(Error::FilePickerFailed);
        }
        pick_via_native(multiple).await
    }
}

/// Reports whether the platform dialog can open a window.
///
/// The GTK 3 dialog blocks forever when GTK cannot open a display, so the
/// picker only uses it when a display is announced. The macOS and Windows
/// dialogs open without one.
#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
fn platform_display_available() -> bool {
    #[cfg(any(
        target_os = "linux",
        target_os = "freebsd",
        target_os = "dragonfly",
        target_os = "netbsd",
        target_os = "openbsd"
    ))]
    {
        desktop_display_available()
    }
    #[cfg(not(any(
        target_os = "linux",
        target_os = "freebsd",
        target_os = "dragonfly",
        target_os = "netbsd",
        target_os = "openbsd"
    )))]
    {
        true
    }
}

#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
async fn pick_via_native(multiple: bool) -> Result<Vec<PathBuf>, Error> {
    let dialog = rfd::AsyncFileDialog::new();
    let selected = if multiple {
        dialog.pick_files().await
    } else {
        dialog.pick_file().await.map(|handle| vec![handle])
    };
    let Some(handles) = selected else {
        return Err(Error::Cancelled);
    };
    let paths: Vec<PathBuf> = handles
        .into_iter()
        .map(|handle| handle.path().to_path_buf())
        .collect();
    if paths.is_empty() {
        return Err(Error::Cancelled);
    }
    Ok(paths)
}

#[cfg(all(
    not(any(target_os = "android", target_arch = "wasm32")),
    target_os = "linux"
))]
enum PortalOutcome {
    Selected(Vec<PathBuf>),
    Cancelled,
    Unavailable,
}

#[cfg(all(
    not(any(target_os = "android", target_arch = "wasm32")),
    target_os = "linux"
))]
async fn portal_available() -> bool {
    const PORTAL_NAME: &str = "org.freedesktop.portal.Desktop";
    let Ok(connection) = ashpd::zbus::Connection::session().await else {
        return false;
    };
    let Ok(proxy) = ashpd::zbus::fdo::DBusProxy::new(&connection).await else {
        return false;
    };
    let Ok(name) = ashpd::zbus::names::BusName::try_from(PORTAL_NAME) else {
        return false;
    };
    proxy.name_has_owner(name).await.unwrap_or(false)
}

#[cfg(all(
    not(any(target_os = "android", target_arch = "wasm32")),
    target_os = "linux"
))]
async fn pick_via_portal(multiple: bool) -> PortalOutcome {
    let request = ashpd::desktop::file_chooser::OpenFileRequest::default().multiple(multiple);
    let Ok(sent) = request.send().await else {
        return PortalOutcome::Unavailable;
    };
    match sent.response() {
        Ok(selected) => {
            let paths: Vec<PathBuf> = selected
                .uris()
                .iter()
                .filter_map(|uri| uri.to_file_path().ok())
                .collect();
            if paths.is_empty() {
                PortalOutcome::Cancelled
            } else {
                PortalOutcome::Selected(paths)
            }
        }
        Err(ashpd::Error::Response(ashpd::desktop::ResponseError::Cancelled)) => {
            PortalOutcome::Cancelled
        }
        Err(_) => PortalOutcome::Unavailable,
    }
}

#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
async fn read_selected_files(
    selected: Vec<PathBuf>,
    progress: Option<Arc<Mutex<Option<Job<Stage>>>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error> {
    use std::io::Read;

    let mut total: u64 = 0;
    for path in &selected {
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            return Err(Error::Cancelled);
        }
        let meta = std::fs::metadata(path).map_err(Error::from)?;
        total = total.saturating_add(meta.len());
    }
    if let Some(shared) = progress.as_ref()
        && let Ok(mut guard) = shared.lock()
    {
        *guard = Some(Job {
            stage: Stage::Attach,
            done: 0,
            total: total.max(1),
            name: None,
        });
    }
    yield_to_paint().await;
    let mut out: Vec<(String, Vec<u8>)> = Vec::with_capacity(selected.len());
    let mut done: u64 = 0;
    let mut chunk = vec![0u8; PICK_CHUNK];
    for path in selected {
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            return Err(Error::Cancelled);
        }
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or_default()
            .to_string();
        let file_len =
            std::fs::metadata(&path).map_or(0, |meta| usize::try_from(meta.len()).unwrap_or(0));
        let mut data: Vec<u8> = Vec::with_capacity(file_len);
        let mut file = std::fs::File::open(&path).map_err(Error::from)?;
        loop {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                return Err(Error::Cancelled);
            }
            let read = file.read(&mut chunk).map_err(Error::from)?;
            if read == 0 {
                break;
            }
            data.extend_from_slice(&chunk[..read]);
            done = done.saturating_add(read as u64);
            if let Some(shared) = progress.as_ref()
                && let Ok(mut guard) = shared.lock()
            {
                *guard = Some(Job {
                    stage: Stage::Attach,
                    done,
                    total: total.max(1),
                    name: Some(name.clone()),
                });
            }
            yield_to_paint().await;
        }
        out.push((name, data));
    }
    if let Some(shared) = progress
        && let Ok(mut guard) = shared.lock()
    {
        *guard = None;
    }
    Ok(out)
}

#[must_use]
pub fn data_url_mime(prefix: &str) -> Option<&str> {
    prefix
        .strip_prefix("data:")
        .and_then(|rest| rest.split(';').next())
        .filter(|mime| !mime.is_empty())
}

#[must_use]
#[cfg(feature = "thumbnail")]
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
#[cfg(feature = "thumbnail")]
fn extract_native(url: &str) -> Option<String> {
    let (prefix, payload) = url.split_once(',').unwrap_or(("", ""));
    let _mime = data_url_mime(prefix)?;
    let bytes = base64::Engine::decode(&base64::engine::general_purpose::STANDARD, payload).ok()?;
    let jpeg = functora_core::thumbnail::video_thumbnail(&bytes)?;
    Some(functora_core::thumbnail::jpeg_data_url(jpeg))
}
