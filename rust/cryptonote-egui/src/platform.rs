#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
mod native {
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

    pub async fn pick_files(multiple: bool) -> Result<Vec<(String, Vec<u8>)>, String> {
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
        let mut files = Vec::new();
        for handle in handles {
            files.push((handle.file_name(), handle.read().await));
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

    pub async fn pick_files(multiple: bool) -> Result<Vec<(String, Vec<u8>)>, String> {
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
        input.click();
        let Some(files) = input.files() else {
            return Ok(Vec::new());
        };
        let mut out = Vec::new();
        for i in 0..files.length() {
            let file = files.get(i).ok_or("Failed to read file list")?;
            out.push((file.name(), read_file(&file).await?));
        }
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

    async fn read_file(file: &File) -> Result<Vec<u8>, String> {
        let buffer = js_promise(file.array_buffer())
            .await?
            .dyn_into::<js_sys::ArrayBuffer>()
            .map_err(|_| "Unexpected file content".to_string())?;
        Ok(Uint8Array::new(&buffer).to_vec())
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

pub async fn pick_files(multiple: bool) -> Result<Vec<(String, Vec<u8>)>, String> {
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        native::pick_files(multiple).await
    }
    #[cfg(target_arch = "wasm32")]
    {
        web::pick_files(multiple).await
    }
    #[cfg(target_os = "android")]
    {
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
