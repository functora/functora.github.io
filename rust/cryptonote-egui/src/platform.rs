#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
mod native {
    use std::io::Read;
    use std::sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    };

    pub async fn write_clipboard(text: String) -> Result<(), String> {
        std::future::ready(()).await;
        let mut board = arboard::Clipboard::new().map_err(|e| e.to_string())?;
        board.set_text(text).map_err(|e| e.to_string())
    }

    pub async fn read_clipboard() -> Result<String, String> {
        std::future::ready(()).await;
        let mut board = arboard::Clipboard::new().map_err(|e| e.to_string())?;
        board.get_text().map_err(|e| e.to_string())
    }

    pub async fn social_share(_text: String, _url: String) -> Result<(), String> {
        std::future::ready(()).await;
        Err("Social sharing is not supported on this platform".into())
    }

    pub async fn print_page() -> Result<(), String> {
        std::future::ready(()).await;
        Err("Printing is not supported on this platform".into())
    }

    const PICK_CHUNK: usize = 4 * 1024 * 1024;

    pub type CancelToken = Arc<AtomicBool>;

    pub async fn pick_files_with_cancel(
        multiple: bool,
        cancel: Option<&CancelToken>,
    ) -> Result<Vec<(String, Vec<u8>)>, String> {
        let mut dialog = rfd::AsyncFileDialog::new();
        if multiple {
            dialog = dialog.set_title("Select files to attach");
        } else {
            dialog = dialog.set_title("Select a file");
        }
        let handles = if multiple {
            dialog.pick_files().await.unwrap_or_default()
        } else {
            dialog
                .pick_file()
                .await
                .map(|file| vec![file])
                .unwrap_or_default()
        };
        if handles.is_empty() {
            return Ok(Vec::new());
        }
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            return Err("Cancelled".to_string());
        }
        let mut total: u64 = 0;
        for handle in &handles {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                return Err("Cancelled".to_string());
            }
            let meta = std::fs::metadata(handle.path()).map_err(|e| e.to_string())?;
            let size = meta.len();
            total = total.saturating_add(size);
        }
        let mut files: Vec<(String, Vec<u8>)> = Vec::with_capacity(handles.len());
        let mut chunk = vec![0u8; PICK_CHUNK];
        for handle in handles {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                return Err("Cancelled".to_string());
            }
            let name = handle.file_name();
            let path = handle.path().to_path_buf();
            let file_len =
                std::fs::metadata(&path).map_or(0, |m| usize::try_from(m.len()).unwrap_or(0));
            let mut data: Vec<u8> = Vec::with_capacity(file_len);
            let mut file = std::fs::File::open(&path).map_err(|e| e.to_string())?;
            loop {
                if let Some(token) = cancel
                    && token.load(Ordering::Relaxed)
                {
                    return Err("Cancelled".to_string());
                }
                let n = file.read(&mut chunk).map_err(|e| e.to_string())?;
                if n == 0 {
                    break;
                }
                data.extend_from_slice(&chunk[..n]);
            }
            files.push((name, data));
        }
        Ok(files)
    }

    pub async fn pick_files_with_shared_progress(
        multiple: bool,
        progress: Option<std::sync::Arc<std::sync::Mutex<Option<crate::progress::Job>>>>,
        cancel: Option<&CancelToken>,
    ) -> Result<Vec<(String, Vec<u8>)>, String> {
        let mut dialog = rfd::AsyncFileDialog::new();
        if multiple {
            dialog = dialog.set_title("Select files to attach");
        } else {
            dialog = dialog.set_title("Select a file");
        }
        let handles = if multiple {
            dialog.pick_files().await.unwrap_or_default()
        } else {
            dialog
                .pick_file()
                .await
                .map(|file| vec![file])
                .unwrap_or_default()
        };
        if handles.is_empty() {
            if let Some(shared) = progress
                && let Ok(mut guard) = shared.lock()
            {
                *guard = None;
            }
            return Ok(Vec::new());
        }
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            if let Some(shared) = progress
                && let Ok(mut guard) = shared.lock()
            {
                *guard = None;
            }
            return Err("Cancelled".to_string());
        }
        let mut total: u64 = 0;
        for handle in &handles {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                return Err("Cancelled".to_string());
            }
            let meta = std::fs::metadata(handle.path()).map_err(|e| e.to_string())?;
            let size = meta.len();
            total = total.saturating_add(size);
        }
        if let Some(shared) = progress.as_ref()
            && let Ok(mut guard) = shared.lock()
        {
            *guard = Some(crate::progress::Job {
                stage: crate::progress::Stage::Attach,
                done: 0,
                total: total.max(1),
                name: None,
            });
        }
        let mut files: Vec<(String, Vec<u8>)> = Vec::with_capacity(handles.len());
        let mut done: u64 = 0;
        let mut chunk = vec![0u8; PICK_CHUNK];
        for handle in handles {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                return Err("Cancelled".to_string());
            }
            let name = handle.file_name();
            let path = handle.path().to_path_buf();
            let file_len =
                std::fs::metadata(&path).map_or(0, |m| usize::try_from(m.len()).unwrap_or(0));
            let mut data: Vec<u8> = Vec::with_capacity(file_len);
            let mut file = std::fs::File::open(&path).map_err(|e| e.to_string())?;
            loop {
                if let Some(token) = cancel
                    && token.load(Ordering::Relaxed)
                {
                    return Err("Cancelled".to_string());
                }
                let n = file.read(&mut chunk).map_err(|e| e.to_string())?;
                if n == 0 {
                    break;
                }
                data.extend_from_slice(&chunk[..n]);
                done = done.saturating_add(n as u64);
                if let Some(shared) = progress.as_ref()
                    && let Ok(mut guard) = shared.lock()
                {
                    *guard = Some(crate::progress::Job {
                        stage: crate::progress::Stage::Attach,
                        done,
                        total: total.max(1),
                        name: Some(name.clone()),
                    });
                }
            }
            files.push((name, data));
        }
        if let Some(shared) = progress
            && let Ok(mut guard) = shared.lock()
        {
            *guard = None;
        }
        Ok(files)
    }

    pub async fn save_bytes(filename: &str, bytes: Vec<u8>) -> Result<Option<String>, String> {
        let Some(handle) = rfd::AsyncFileDialog::new()
            .set_file_name(filename)
            .save_file()
            .await
        else {
            return Ok(None);
        };
        std::fs::write(handle.path(), bytes).map_err(|e| e.to_string())?;
        Ok(Some(handle.path().to_string_lossy().to_string()))
    }
}

#[cfg(target_arch = "wasm32")]
mod web {
    use js_sys::Uint8Array;
    use wasm_bindgen::{JsCast, JsValue};
    use web_sys::{
        Blob, Clipboard, Document, File, HtmlAnchorElement, HtmlInputElement, Navigator, Url,
        Window,
    };

    pub async fn write_clipboard(text: String) -> Result<(), String> {
        let Some(clip) = clipboard() else {
            return Err("Clipboard API is not available".into());
        };
        let _ = js_promise(clip.write_text(&text)).await?;
        Ok(())
    }

    pub async fn read_clipboard() -> Result<String, String> {
        let Some(clip) = clipboard() else {
            return Err("Clipboard API is not available".into());
        };
        js_promise(clip.read_text())
            .await
            .map(|value| value.as_string().unwrap_or_default())
    }

    pub async fn social_share(text: String, url: String) -> Result<(), String> {
        let Some(nav) = navigator() else {
            return Err("Navigator is not available".into());
        };
        let data = web_sys::ShareData::new();
        data.set_text(&text);
        data.set_url(&url);
        let _ = js_promise(nav.share_with_data(&data)).await?;
        Ok(())
    }

    pub async fn print_page() -> Result<(), String> {
        std::future::ready(()).await;
        let Some(win) = window() else {
            return Err("Window is not available".into());
        };
        win.print().map_err(|e| js_error(&e))
    }

    const PICK_CHUNK: usize = 4 * 1024 * 1024;

    pub type CancelToken = std::sync::Arc<std::sync::atomic::AtomicBool>;

    pub async fn pick_files_with_cancel(
        multiple: bool,
        cancel: Option<&CancelToken>,
    ) -> Result<Vec<(String, Vec<u8>)>, String> {
        use std::sync::atomic::Ordering;
        use wasm_bindgen::JsCast;
        use wasm_bindgen::closure::Closure;
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            return Err("Cancelled".to_string());
        }
        let Some(doc) = document() else {
            return Err("Document is not available".into());
        };
        let input: HtmlInputElement = doc
            .create_element("input")
            .map_err(|e| js_error(&e))?
            .dyn_into()
            .map_err(|_| "Failed to create file input".to_string())?;
        input.set_type("file");
        input.set_multiple(multiple);
        input
            .style()
            .set_property("display", "none")
            .map_err(|e| js_error(&e))?;
        let promise = {
            let input_clone = input.clone();
            js_sys::Promise::new(&mut |resolve, _reject| {
                let resolve_change = resolve.clone();
                let closure_change = Closure::once(move |_event: web_sys::Event| {
                    drop(resolve_change.call0(&JsValue::NULL));
                });
                input_clone.set_onchange(Some(closure_change.as_ref().unchecked_ref()));
                closure_change.forget();
                let input_cancel = input.clone();
                let resolve_cancel = resolve.clone();
                let closure_cancel = Closure::once(move |_event: web_sys::Event| {
                    drop(resolve_cancel.call0(&JsValue::NULL));
                });
                drop(input_cancel.add_event_listener_with_callback(
                    "cancel",
                    closure_cancel.as_ref().unchecked_ref(),
                ));
                closure_cancel.forget();
            })
        };
        let body = doc.body().ok_or_else(|| "No body".to_string())?;
        drop(body.append_child(&input).map_err(|e| js_error(&e))?);
        input.click();
        drop(js_promise(promise).await?);
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            drop(body.remove_child(&input));
            return Err("Cancelled".to_string());
        }
        let Some(list) = input.files() else {
            drop(body.remove_child(&input));
            return Ok(Vec::new());
        };
        if list.length() == 0 {
            drop(body.remove_child(&input));
            return Ok(Vec::new());
        }
        let mut total: u64 = 0;
        for i in 0..list.length() {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                drop(body.remove_child(&input));
                return Err("Cancelled".to_string());
            }
            let file = list.get(i).ok_or("Failed to read file list")?;
            let blob: &Blob = file.unchecked_ref();
            let size = blob.size() as u64;
            total = total.saturating_add(size);
        }
        let mut out: Vec<(String, Vec<u8>)> = Vec::with_capacity(list.length() as usize);
        for i in 0..list.length() {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                drop(body.remove_child(&input));
                return Err("Cancelled".to_string());
            }
            let file = list.get(i).ok_or("Failed to read file list")?;
            let name = file.name();
            let data = read_file_chunked_with_cancel(&file, cancel).await?;
            out.push((name, data));
        }
        drop(body.remove_child(&input));
        Ok(out)
    }

    pub async fn pick_files_with_shared_progress(
        multiple: bool,
        progress: Option<std::sync::Arc<std::sync::Mutex<Option<crate::progress::Job>>>>,
        cancel: Option<&CancelToken>,
    ) -> Result<Vec<(String, Vec<u8>)>, String> {
        use std::sync::atomic::Ordering;
        use wasm_bindgen::JsCast;
        use wasm_bindgen::closure::Closure;
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            return Err("Cancelled".to_string());
        }
        let Some(doc) = document() else {
            return Err("Document is not available".into());
        };
        let input: HtmlInputElement = doc
            .create_element("input")
            .map_err(|e| js_error(&e))?
            .dyn_into()
            .map_err(|_| "Failed to create file input".to_string())?;
        input.set_type("file");
        input.set_multiple(multiple);
        input
            .style()
            .set_property("display", "none")
            .map_err(|e| js_error(&e))?;
        let promise = {
            let input_clone = input.clone();
            js_sys::Promise::new(&mut |resolve, _reject| {
                let resolve_change = resolve.clone();
                let closure_change = Closure::once(move |_evt: web_sys::Event| {
                    drop(resolve_change.call0(&JsValue::NULL));
                });
                input_clone.set_onchange(Some(closure_change.as_ref().unchecked_ref()));
                closure_change.forget();
                let input_cancel = input.clone();
                let resolve_cancel = resolve.clone();
                let closure_cancel = Closure::once(move |_evt: web_sys::Event| {
                    drop(resolve_cancel.call0(&JsValue::NULL));
                });
                drop(input_cancel.add_event_listener_with_callback(
                    "cancel",
                    closure_cancel.as_ref().unchecked_ref(),
                ));
                closure_cancel.forget();
            })
        };
        let body = doc.body().ok_or_else(|| "No body".to_string())?;
        drop(body.append_child(&input).map_err(|e| js_error(&e))?);
        input.click();
        drop(js_promise(promise).await?);
        if let Some(token) = cancel
            && token.load(Ordering::Relaxed)
        {
            if let Some(shared) = progress.as_ref()
                && let Ok(mut guard) = shared.lock()
            {
                *guard = None;
            }
            drop(body.remove_child(&input));
            return Err("Cancelled".to_string());
        }
        let Some(list) = input.files() else {
            if let Some(shared) = progress.as_ref()
                && let Ok(mut guard) = shared.lock()
            {
                *guard = None;
            }
            drop(body.remove_child(&input));
            return Ok(Vec::new());
        };
        if list.length() == 0 {
            if let Some(shared) = progress.as_ref()
                && let Ok(mut guard) = shared.lock()
            {
                *guard = None;
            }
            drop(body.remove_child(&input));
            return Ok(Vec::new());
        }
        let mut total: u64 = 0;
        for i in 0..list.length() {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                if let Some(shared) = progress.as_ref()
                    && let Ok(mut guard) = shared.lock()
                {
                    *guard = None;
                }
                drop(body.remove_child(&input));
                return Err("Cancelled".to_string());
            }
            let file = list.get(i).ok_or("Failed to read file list")?;
            let blob: &Blob = file.unchecked_ref();
            let size = blob.size() as u64;
            total = total.saturating_add(size);
        }
        if let Some(shared) = progress.as_ref()
            && let Ok(mut guard) = shared.lock()
        {
            *guard = Some(crate::progress::Job {
                stage: crate::progress::Stage::Attach,
                done: 0,
                total: total.max(1),
                name: None,
            });
        }
        let mut out: Vec<(String, Vec<u8>)> = Vec::with_capacity(list.length() as usize);
        let mut done: u64 = 0;
        for i in 0..list.length() {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                if let Some(shared) = progress.as_ref()
                    && let Ok(mut guard) = shared.lock()
                {
                    *guard = None;
                }
                drop(body.remove_child(&input));
                return Err("Cancelled".to_string());
            }
            let file = list.get(i).ok_or("Failed to read file list")?;
            let name = file.name();
            let data = read_file_chunked_with_shared_progress(
                &file,
                progress.as_ref().map(std::sync::Arc::clone),
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
        drop(body.remove_child(&input));
        Ok(out)
    }

    pub async fn save_bytes(filename: &str, bytes: Vec<u8>) -> Result<Option<String>, String> {
        std::future::ready(()).await;
        let Some(doc) = document() else {
            return Err("Document is not available".into());
        };
        let blob =
            Blob::new_with_u8_array_sequence(&JsValue::from(Uint8Array::from(bytes.as_slice())))
                .map_err(|e| js_error(&e))?;
        let url = Url::create_object_url_with_blob(&blob).map_err(|e| js_error(&e))?;
        let anchor: HtmlAnchorElement = doc
            .create_element("a")
            .map_err(|e| js_error(&e))?
            .dyn_into()
            .map_err(|_| "Failed to create download link".to_string())?;
        anchor.set_href(&url);
        anchor.set_download(filename);
        if let Some(body) = doc.body() {
            _ = body.append_child(anchor.as_ref());
            anchor.click();
            anchor.remove();
        }
        Ok(Some(filename.to_string()))
    }

    fn window() -> Option<Window> {
        web_sys::window()
    }

    fn document() -> Option<Document> {
        window()?.document()
    }

    fn navigator() -> Option<Navigator> {
        window().map(|win| win.navigator())
    }

    fn clipboard() -> Option<Clipboard> {
        navigator().map(|nav| nav.clipboard())
    }

    #[allow(dead_code)]
    async fn read_file_chunked(file: &File) -> Result<Vec<u8>, String> {
        read_file_chunked_with_cancel(file, None).await
    }

    async fn read_file_chunked_with_cancel(
        file: &File,
        cancel: Option<&CancelToken>,
    ) -> Result<Vec<u8>, String> {
        use std::sync::atomic::Ordering;
        let blob: &Blob = file.unchecked_ref();
        let size = blob.size() as u64;
        if size == 0 {
            return Ok(Vec::new());
        }
        let size_usize = usize::try_from(size).map_err(|e| format!("{e:?}"))?;
        let mut out = vec![0u8; size_usize];
        let mut offset: u64 = 0;
        let mut write_pos: usize = 0;
        while offset < size {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                return Err("Cancelled".to_string());
            }
            let end = (offset + PICK_CHUNK as u64).min(size);
            let chunk_blob = blob
                .slice_with_f64_and_f64(offset as f64, end as f64)
                .map_err(|e| js_error(&e))?;
            let buffer = js_promise(chunk_blob.array_buffer())
                .await?
                .dyn_into::<js_sys::ArrayBuffer>()
                .map_err(|_| "Unexpected file content".to_string())?;
            let uint8 = Uint8Array::new(&buffer);
            let chunk_len = uint8.length() as usize;
            if chunk_len == 0 {
                offset = end;
                continue;
            }
            uint8.copy_to(&mut out[write_pos..write_pos + chunk_len]);
            write_pos += chunk_len;
            offset = end;
        }
        out.truncate(write_pos);
        Ok(out)
    }

    async fn read_file_chunked_with_shared_progress(
        file: &File,
        progress: Option<std::sync::Arc<std::sync::Mutex<Option<crate::progress::Job>>>>,
        total: u64,
        done: &mut u64,
        name: &str,
        cancel: Option<&CancelToken>,
    ) -> Result<Vec<u8>, String> {
        use std::sync::atomic::Ordering;
        let blob: &Blob = file.unchecked_ref();
        let size = blob.size() as u64;
        if size == 0 {
            return Ok(Vec::new());
        }
        let size_usize = usize::try_from(size).map_err(|e| format!("{e:?}"))?;
        let mut out = vec![0u8; size_usize];
        let mut offset: u64 = 0;
        let mut write_pos: usize = 0;
        while offset < size {
            if let Some(token) = cancel
                && token.load(Ordering::Relaxed)
            {
                return Err("Cancelled".to_string());
            }
            let end = (offset + PICK_CHUNK as u64).min(size);
            let chunk_blob = blob
                .slice_with_f64_and_f64(offset as f64, end as f64)
                .map_err(|e| js_error(&e))?;
            let buffer = js_promise(chunk_blob.array_buffer())
                .await?
                .dyn_into::<js_sys::ArrayBuffer>()
                .map_err(|_| "Unexpected file content".to_string())?;
            let uint8 = Uint8Array::new(&buffer);
            let chunk_len = uint8.length() as usize;
            if chunk_len == 0 {
                offset = end;
                continue;
            }
            uint8.copy_to(&mut out[write_pos..write_pos + chunk_len]);
            write_pos += chunk_len;
            offset = end;
            *done = done.saturating_add(chunk_len as u64);
            if let Some(shared) = progress.as_ref()
                && let Ok(mut guard) = shared.lock()
            {
                *guard = Some(crate::progress::Job {
                    stage: crate::progress::Stage::Attach,
                    done: *done,
                    total: total.max(1),
                    name: Some(name.to_string()),
                });
            }
        }
        out.truncate(write_pos);
        Ok(out)
    }

    #[allow(dead_code)]
    async fn read_file(file: &File) -> Result<Vec<u8>, String> {
        read_file_chunked(file).await
    }

    async fn js_promise(promise: js_sys::Promise) -> Result<JsValue, String> {
        wasm_bindgen_futures::JsFuture::from(promise)
            .await
            .map_err(|e| js_error(&e))
    }

    fn js_error(value: &JsValue) -> String {
        value
            .as_string()
            .unwrap_or_else(|| "Unknown JavaScript error".to_string())
    }
}

#[cfg(target_os = "android")]
mod android {
    pub async fn write_clipboard(_text: String) -> Result<(), String> {
        std::future::ready(()).await;
        Err("Clipboard is not supported on Android yet".into())
    }

    pub async fn read_clipboard() -> Result<String, String> {
        std::future::ready(()).await;
        Err("Clipboard is not supported on Android yet".into())
    }

    pub async fn social_share(_text: String, _url: String) -> Result<(), String> {
        std::future::ready(()).await;
        Err("Social sharing is not supported on Android yet".into())
    }

    pub async fn print_page() -> Result<(), String> {
        std::future::ready(()).await;
        Err("Printing is not supported on Android yet".into())
    }

    pub async fn pick_files(_multiple: bool) -> Result<Vec<(String, Vec<u8>)>, String> {
        std::future::ready(()).await;
        Err("File picking is not supported on Android yet".into())
    }

    pub async fn save_bytes(_filename: &str, _bytes: Vec<u8>) -> Result<Option<String>, String> {
        std::future::ready(()).await;
        Err("Downloading is not supported on Android yet".into())
    }
}

pub async fn write_clipboard(text: String) -> Result<(), String> {
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        native::write_clipboard(text).await
    }
    #[cfg(target_arch = "wasm32")]
    {
        web::write_clipboard(text).await
    }
    #[cfg(target_os = "android")]
    {
        android::write_clipboard(text).await
    }
}

pub async fn read_clipboard() -> Result<String, String> {
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        native::read_clipboard().await
    }
    #[cfg(target_arch = "wasm32")]
    {
        web::read_clipboard().await
    }
    #[cfg(target_os = "android")]
    {
        android::read_clipboard().await
    }
}

pub async fn social_share(text: String, url: String) -> Result<(), String> {
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        native::social_share(text, url).await
    }
    #[cfg(target_arch = "wasm32")]
    {
        web::social_share(text, url).await
    }
    #[cfg(target_os = "android")]
    {
        android::social_share(text, url).await
    }
}

pub async fn print_page() -> Result<(), String> {
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        native::print_page().await
    }
    #[cfg(target_arch = "wasm32")]
    {
        web::print_page().await
    }
    #[cfg(target_os = "android")]
    {
        android::print_page().await
    }
}

pub type CancelToken = std::sync::Arc<std::sync::atomic::AtomicBool>;

#[must_use]
pub fn new_cancel_token() -> CancelToken {
    std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false))
}

pub async fn pick_files(multiple: bool) -> Result<Vec<(String, Vec<u8>)>, String> {
    pick_files_with_cancel(multiple, None).await
}

pub async fn pick_files_with_cancel(
    multiple: bool,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, String> {
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        native::pick_files_with_cancel(multiple, cancel).await
    }
    #[cfg(target_arch = "wasm32")]
    {
        web::pick_files_with_cancel(multiple, cancel).await
    }
    #[cfg(target_os = "android")]
    {
        let _ = cancel;
        android::pick_files(multiple).await
    }
}

pub async fn pick_files_with_shared_progress(
    multiple: bool,
    progress: Option<std::sync::Arc<std::sync::Mutex<Option<crate::progress::Job>>>>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, String> {
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        native::pick_files_with_shared_progress(multiple, progress, cancel).await
    }
    #[cfg(target_arch = "wasm32")]
    {
        web::pick_files_with_shared_progress(multiple, progress, cancel).await
    }
    #[cfg(target_os = "android")]
    {
        let _ = (progress, cancel);
        android::pick_files(multiple).await
    }
}

pub async fn save_bytes(filename: &str, bytes: Vec<u8>) -> Result<Option<String>, String> {
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        native::save_bytes(filename, bytes).await
    }
    #[cfg(target_arch = "wasm32")]
    {
        web::save_bytes(filename, bytes).await
    }
    #[cfg(target_os = "android")]
    {
        android::save_bytes(filename, bytes).await
    }
}

pub fn decode_qr_image(bytes: &[u8]) -> Result<String, String> {
    let image = image::load_from_memory(bytes).map_err(|e| e.to_string())?;
    let luma = image.to_luma8();
    let (w, h) = luma.dimensions();
    functora_core::qr::decode_qr_luma(luma.as_raw(), w, h)
        .ok_or_else(|| "No QR code found in image".to_string())
}
