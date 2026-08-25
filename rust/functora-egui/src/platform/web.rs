use crate::camera::FrameData;
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

fn camera_error_msg(msg: String) -> Error {
    if msg.contains("Permission") || msg.contains("denied") || msg.contains("NotAllowed") {
        Error::CameraPermissionDenied(msg)
    } else {
        Error::CameraNotAvailable(msg)
    }
}

thread_local! {
    static STREAM: std::cell::RefCell<Option<web_sys::MediaStream>> = const { std::cell::RefCell::new(None) };
    static VIDEO: std::cell::RefCell<Option<web_sys::HtmlVideoElement>> = const { std::cell::RefCell::new(None) };
}

static CAPTURE_SESSION: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
thread_local! {
    static CAPTURE_ARMED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

pub fn begin_capture_session() {
    let session = CAPTURE_SESSION.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
    let _ = session;
    CAPTURE_ARMED.with(|c| c.set(false));
}

pub fn stop_capture_worker() {
    CAPTURE_ARMED.with(|c| c.set(false));
}

pub async fn check_camera() -> Result<(), Error> {
    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    let navigator = window.navigator();
    let _ = navigator
        .media_devices()
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    Ok(())
}

pub async fn start_camera() -> Result<(), Error> {
    use wasm_bindgen::JsCast as _;
    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    let navigator = window.navigator();
    let media = navigator
        .media_devices()
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    let constraints = web_sys::MediaStreamConstraints::new();
    let video_constraints = js_sys::Object::new();
    let _ = js_sys::Reflect::set(
        &video_constraints,
        &wasm_bindgen::JsValue::from_str("facingMode"),
        &wasm_bindgen::JsValue::from_str("environment"),
    )
    .map_err(|e| Error::JS(format!("{e:?}")))?;
    constraints.set_video(&video_constraints);
    let promise = media
        .get_user_media_with_constraints(&constraints)
        .map_err(|e| camera_error_msg(format!("{e:?}")))?;
    let stream_js = wasm_bindgen_futures::JsFuture::from(promise)
        .await
        .map_err(|e| camera_error_msg(format!("{e:?}")))?;
    let stream: web_sys::MediaStream = stream_js.unchecked_into();
    let document = window
        .document()
        .ok_or_else(|| Error::JS("No document".into()))?;
    let video: web_sys::HtmlVideoElement = document
        .get_element_by_id("qr-video")
        .and_then(|el| el.dyn_into::<web_sys::HtmlVideoElement>().ok())
        .or_else(|| {
            document.create_element("video").ok().and_then(|el| {
                let _ = el.set_attribute("id", "qr-video");
                let _ = el.set_attribute("autoplay", "true");
                let _ = el.set_attribute("playsinline", "true");
                let _ = el.set_attribute("muted", "true");
                let _ = el.set_attribute("style", "display:none");
                let _ = document.body()?.append_child(&el).ok()?;
                el.dyn_into::<web_sys::HtmlVideoElement>().ok()
            })
        })
        .ok_or_else(|| Error::JS("No video element".into()))?;
    video.set_src_object(Some(&stream));
    let play_promise = video
        .play()
        .map_err(|e| camera_error_msg(format!("{e:?}")))?;
    let _ = wasm_bindgen_futures::JsFuture::from(play_promise)
        .await
        .map_err(|e| camera_error_msg(format!("{e:?}")))?;
    STREAM.with(|s| *s.borrow_mut() = Some(stream));
    VIDEO.with(|v| *v.borrow_mut() = Some(video));
    begin_capture_session();
    Ok(())
}

pub async fn capture_frame() -> Result<FrameData, Error> {
    use wasm_bindgen::JsCast as _;
    let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
    let document = window
        .document()
        .ok_or_else(|| Error::JS("No document".into()))?;
    let video: web_sys::HtmlVideoElement = VIDEO
        .with(|v| v.borrow().clone())
        .or_else(|| {
            document
                .get_element_by_id("qr-video")
                .and_then(|el| el.dyn_into::<web_sys::HtmlVideoElement>().ok())
        })
        .ok_or_else(|| Error::JS("No video element".into()))?;
    let mut waited: u64 = 0;
    while video.video_width() == 0 || video.video_height() == 0 {
        if waited > 5000 {
            return Err(Error::CameraStalled);
        }
        gloo_timers::future::TimeoutFuture::new(100).await;
        waited += 100;
    }
    let w0 = video.video_width();
    let h0 = video.video_height();
    if w0 == 0 || h0 == 0 {
        return Err(Error::CameraStalled);
    }
    let max: u32 = 360;
    let k = f32::min(1.0, max as f32 / u32::max(w0, h0) as f32);
    let w = (w0 as f32 * k).round() as u32;
    let h = (h0 as f32 * k).round() as u32;
    let w = w.max(1);
    let h = h.max(1);
    let canvas: web_sys::HtmlCanvasElement = document
        .get_element_by_id("qr-canvas")
        .and_then(|el| el.dyn_into::<web_sys::HtmlCanvasElement>().ok())
        .or_else(|| {
            document.create_element("canvas").ok().and_then(|el| {
                let _ = el.set_attribute("id", "qr-canvas");
                let _ = el.set_attribute("style", "display:none");
                let _ = document.body()?.append_child(&el).ok()?;
                el.dyn_into::<web_sys::HtmlCanvasElement>().ok()
            })
        })
        .ok_or_else(|| Error::JS("No canvas".into()))?;
    canvas.set_width(w);
    canvas.set_height(h);
    let ctx: web_sys::CanvasRenderingContext2d = canvas
        .get_context("2d")
        .map_err(|e| Error::JS(format!("{e:?}")))?
        .ok_or_else(|| Error::JS("No 2d context".into()))?
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .map_err(|_| Error::JS("Not 2d context".into()))?;
    ctx.draw_image_with_html_video_element_and_dw_and_dh(
        &video,
        0.0,
        0.0,
        f64::from(w),
        f64::from(h),
    )
    .map_err(|e| Error::JS(format!("{e:?}")))?;
    let image_data = ctx
        .get_image_data(0.0, 0.0, f64::from(w), f64::from(h))
        .map_err(|e| Error::JS(format!("{e:?}")))?;
    let data = image_data.data();
    let rgba = data.to_vec();
    let luma: Vec<u8> = rgba
        .chunks_exact(4)
        .map(|px| {
            let r = u32::from(px[0]);
            let g = u32::from(px[1]);
            let b = u32::from(px[2]);
            ((r * 299 + g * 587 + b * 114 + 500) / 1000) as u8
        })
        .collect();
    CAPTURE_ARMED.with(|c| c.set(true));
    Ok(FrameData {
        data: luma,
        width: w,
        height: h,
        preview_rgba: Some(rgba),
    })
}

pub async fn stop_camera() -> Result<(), Error> {
    use wasm_bindgen::JsCast as _;
    let stream_opt = STREAM.with(|s| s.borrow_mut().take());
    if let Some(stream) = stream_opt {
        for track in stream.get_tracks() {
            if let Ok(t) = track.dyn_into::<web_sys::MediaStreamTrack>() {
                t.stop();
            }
        }
    }
    let video_opt = VIDEO.with(|v| v.borrow_mut().take());
    if let Some(video) = video_opt {
        video.set_src_object(None);
    }
    if let Some(window) = web_sys::window()
        && let Some(document) = window.document()
    {
        if let Some(el) = document.get_element_by_id("qr-video")
            && let Some(body) = document.body()
        {
            let _ = body.remove_child(&el);
        }
        if let Some(el) = document.get_element_by_id("qr-canvas")
            && let Some(body) = document.body()
        {
            let _ = body.remove_child(&el);
        }
    }
    stop_capture_worker();
    Ok(())
}

#[must_use]
pub fn location_href() -> Option<String> {
    web_sys::window()?.location().href().ok()
}

#[must_use]
pub fn location_hash() -> Option<String> {
    web_sys::window()?.location().hash().ok()
}
