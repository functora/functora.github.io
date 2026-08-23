use crate::error::Error;

pub async fn clipboard_read() -> Result<String, Error> {
    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    let navigator = window.navigator();
    let clipboard = navigator.clipboard();
    let promise = clipboard.read_text();
    let result = wasm_bindgen_futures::JsFuture::from(promise)
        .await
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    result
        .as_string()
        .ok_or_else(|| Error::JS("Clipboard read: not a string".into()))
}

pub async fn clipboard_write(text: String) -> Result<(), Error> {
    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    let navigator = window.navigator();
    let clipboard = navigator.clipboard();
    let promise = clipboard.write_text(&text);
    drop(
        wasm_bindgen_futures::JsFuture::from(promise)
            .await
            .map_err(|e| Error::JS(format!("{e:?}")))?,
    );
    Ok(())
}

#[derive(Debug, Clone)]
pub struct ShareData {
    pub title: String,
    pub text: String,
    pub url: String,
}

pub async fn share(data: ShareData) -> Result<(), Error> {
    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    let navigator = window.navigator();
    let share_data = web_sys::ShareData::new();
    share_data.set_title(&data.title);
    share_data.set_text(&format!("{}\n{}", data.text, data.url));
    share_data.set_url(&data.url);
    let promise = navigator.share_with_data(&share_data);
    drop(
        wasm_bindgen_futures::JsFuture::from(promise)
            .await
            .map_err(|e| Error::JS(format!("{e:?}")))?,
    );
    Ok(())
}

pub async fn print_page() -> Result<(), Error> {
    std::future::ready(()).await;
    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    window.print().map_err(|e| Error::JS(format!("{e:?}")))?;
    Ok(())
}

pub async fn download(data: Vec<u8>, filename: &str) -> Result<String, Error> {
    use wasm_bindgen::JsCast as _;
    std::future::ready(()).await;
    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    let document = window
        .document()
        .ok_or_else(|| Error::JS("No document".into()))?;
    let mime = mime_for(filename);
    let array = js_sys::Uint8Array::from(data.as_slice());
    let blob_parts = js_sys::Array::new();
    let new_len = blob_parts.push(&array.buffer());
    debug_assert!(new_len > 0);
    let options = web_sys::BlobPropertyBag::new();
    options.set_type(mime);
    let blob = web_sys::Blob::new_with_u8_array_sequence_and_options(&blob_parts, &options)
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    let url = web_sys::Url::create_object_url_with_blob(&blob)
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    let anchor = document
        .create_element("a")
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    let anchor_elem: web_sys::HtmlAnchorElement = anchor
        .dyn_into()
        .map_err(|_| Error::JS("Not an anchor".into()))?;
    anchor_elem.set_href(&url);
    anchor_elem.set_download(filename);
    anchor_elem
        .style()
        .set_property("display", "none")
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    drop(
        document
            .body()
            .ok_or_else(|| Error::JS("No body".into()))?
            .append_child(&anchor_elem)
            .map_err(|e| Error::JS(format!("{e:?}")))?,
    );
    anchor_elem.click();
    let url_clone = url.clone();
    let closure = wasm_bindgen::closure::Closure::once_into_js(move || {
        if let Err(e) = web_sys::Url::revoke_object_url(&url_clone) {
            tracing::warn!("Failed to revoke object URL: {e:?}");
        }
    });
    if let Err(e) = window.set_timeout_with_callback_and_timeout_and_arguments_0(
        closure.as_ref().unchecked_ref(),
        1000,
    ) {
        tracing::warn!("set_timeout failed: {e:?}");
    }
    Ok(filename.to_string())
}

fn mime_for(name: &str) -> &str {
    functora_core::files::mime_for(name).unwrap_or("application/octet-stream")
}

#[must_use]
pub fn storage_get(key: &str) -> Option<String> {
    web_sys::window()
        .and_then(|w| w.local_storage().ok()?)
        .and_then(|s| s.get_item(key).ok()?)
}

pub fn storage_set(key: &str, value: &str) -> Result<(), Error> {
    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    let storage = window
        .local_storage()
        .map_err(|e| Error::JS(format!("{e:?}")))?
        .ok_or_else(|| Error::JS("No localStorage".into()))?;
    storage
        .set_item(key, value)
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    Ok(())
}

#[must_use]
pub fn is_mobile_hint() -> Option<bool> {
    let window = web_sys::window()?;
    let width = window.inner_width().ok()?.as_f64().unwrap_or(0.0);
    Some(width < 800.0 && width > 0.0)
}

pub async fn sleep(millis: u64) {
    gloo_timers::future::TimeoutFuture::new(u32::try_from(millis).unwrap_or(u32::MAX)).await;
}

#[must_use]
pub fn location_href() -> Option<String> {
    web_sys::window()?.location().href().ok()
}

#[must_use]
pub fn location_hash() -> Option<String> {
    web_sys::window()?.location().hash().ok()
}
