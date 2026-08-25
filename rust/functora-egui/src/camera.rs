use crate::error::Error;

#[derive(Debug, Clone)]
pub struct FrameData {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
    pub preview_rgba: Option<Vec<u8>>,
}

#[cfg(any(all(target_arch = "wasm32", feature = "web"), target_os = "android"))]
fn camera_error(msg: String) -> Error {
    if msg.contains("Permission") || msg.contains("denied") || msg.contains("NotAllowed") {
        Error::CameraPermissionDenied(msg)
    } else {
        Error::CameraNotAvailable(msg)
    }
}

pub async fn check_camera() -> Result<(), Error> {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        return crate::platform::web::check_camera().await;
    }
    #[cfg(target_os = "android")]
    {
        return crate::platform::android::check_camera().await;
    }
    #[cfg(not(any(all(target_arch = "wasm32", feature = "web"), target_os = "android")))]
    {
        return crate::platform::desktop::check_camera().await;
    }
    #[allow(unreachable_code)]
    Err(Error::CameraNotAvailable("Camera not available".into()))
}

pub async fn start_camera() -> Result<(), Error> {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        return crate::platform::web::start_camera()
            .await
            .map_err(|e| match &e {
                Error::JS(msg) => camera_error(msg.clone()),
                _ => e,
            });
    }
    #[cfg(target_os = "android")]
    {
        return crate::platform::android::start_camera()
            .await
            .map_err(|e| match &e {
                Error::JS(msg) => camera_error(msg.clone()),
                _ => e,
            });
    }
    #[cfg(not(any(all(target_arch = "wasm32", feature = "web"), target_os = "android")))]
    {
        return crate::platform::desktop::start_camera().await;
    }
    #[allow(unreachable_code)]
    Err(Error::CameraNotAvailable("Camera not available".into()))
}

pub async fn capture_frame() -> Result<FrameData, Error> {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        return crate::platform::web::capture_frame().await;
    }
    #[cfg(target_os = "android")]
    {
        return crate::platform::android::capture_frame().await;
    }
    #[cfg(not(any(all(target_arch = "wasm32", feature = "web"), target_os = "android")))]
    {
        return crate::platform::desktop::capture_frame().await;
    }
    #[allow(unreachable_code)]
    Err(Error::CameraNotAvailable("Not implemented".into()))
}

pub fn begin_capture_session() {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        crate::platform::web::begin_capture_session();
    }
    #[cfg(target_os = "android")]
    {
        crate::platform::android::begin_capture_session();
    }
    #[cfg(not(any(all(target_arch = "wasm32", feature = "web"), target_os = "android")))]
    {
        crate::platform::desktop::begin_capture_session();
    }
}

pub fn stop_capture_worker() {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        crate::platform::web::stop_capture_worker();
    }
    #[cfg(target_os = "android")]
    {
        crate::platform::android::stop_capture_worker();
    }
    #[cfg(not(any(all(target_arch = "wasm32", feature = "web"), target_os = "android")))]
    {
        crate::platform::desktop::stop_capture_worker();
    }
}

pub async fn stop_camera() -> Result<(), Error> {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        return crate::platform::web::stop_camera().await;
    }
    #[cfg(target_os = "android")]
    {
        return crate::platform::android::stop_camera().await;
    }
    #[cfg(not(any(all(target_arch = "wasm32", feature = "web"), target_os = "android")))]
    {
        return crate::platform::desktop::stop_camera().await;
    }
    #[allow(unreachable_code)]
    Ok(())
}

pub async fn sleep(millis: u64) -> Result<(), Error> {
    #[cfg(target_arch = "wasm32")]
    {
        #[cfg(feature = "web")]
        {
            crate::platform::web::sleep(millis).await;
            return Ok(());
        }
    }
    #[cfg(target_os = "android")]
    {
        crate::platform::android::sleep(millis).await;
        return Ok(());
    }
    #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
    {
        crate::platform::desktop::sleep(millis).await;
        return Ok(());
    }
    #[allow(unreachable_code)]
    {
        let _ = millis;
        Ok(())
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub enum PwaInstallOutcome {
    Accepted,
    Rejected,
    NotAvailable,
}

pub async fn trigger_pwa_install() -> Result<PwaInstallOutcome, Error> {
    std::future::ready(()).await;
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
        let val = js_sys::Reflect::get(&window, &js_sys::JsString::from("__functoraPwaDeferred"))
            .map_err(|e| Error::JS(format!("{e:?}")))?;
        if val.is_undefined() || val.is_null() {
            return Ok(PwaInstallOutcome::NotAvailable);
        }
        let deferred = js_sys::Object::from(val);
        let prompt = js_sys::Reflect::get(&deferred, &js_sys::JsString::from("prompt"))
            .map_err(|e| Error::JS(format!("{e:?}")))?;
        let func = js_sys::Function::from(prompt);
        let _ = func
            .call0(&deferred)
            .map_err(|e| Error::JS(format!("{e:?}")))?;
        let user_choice = js_sys::Reflect::get(&deferred, &js_sys::JsString::from("userChoice"))
            .map_err(|e| Error::JS(format!("{e:?}")))?;
        let promise = js_sys::Promise::from(user_choice);
        let result = wasm_bindgen_futures::JsFuture::from(promise)
            .await
            .map_err(|e| Error::JS(format!("{e:?}")))?;
        let outcome = js_sys::Reflect::get(&result, &js_sys::JsString::from("outcome"))
            .map_err(|e| Error::JS(format!("{e:?}")))?
            .as_string()
            .unwrap_or_default();
        if !js_sys::Reflect::set(
            &window,
            &js_sys::JsString::from("__functoraPwaDeferred"),
            &wasm_bindgen::JsValue::NULL,
        )
        .unwrap_or(false)
        {
            tracing::warn!("Failed to clear __functoraPwaDeferred");
        }
        return Ok(match outcome.as_str() {
            "accepted" => PwaInstallOutcome::Accepted,
            "rejected" => PwaInstallOutcome::Rejected,
            _ => PwaInstallOutcome::NotAvailable,
        });
    }
    #[cfg(not(all(target_arch = "wasm32", feature = "web")))]
    {
        return Ok(PwaInstallOutcome::NotAvailable);
    }
    #[allow(unreachable_code)]
    Ok(PwaInstallOutcome::NotAvailable)
}

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub enum InstallHint {
    Ios,
    Mac,
    Unavailable,
}

pub async fn install_hint() -> Result<InstallHint, Error> {
    std::future::ready(()).await;
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        let window = web_sys::window().ok_or_else(|| Error::JS("No window".into()))?;
        let ua = window
            .navigator()
            .user_agent()
            .map_err(|e| Error::JS(format!("{e:?}")))?;
        if ua.contains("iPad") || ua.contains("iPhone") || ua.contains("iPod") {
            return Ok(InstallHint::Ios);
        }
        if ua.contains("Macintosh")
            && window.navigator().user_agent().is_ok()
            && ua.contains("Safari/")
            && !ua.contains("Chrome")
            && !ua.contains("CriOS")
            && !ua.contains("Edg/")
        {
            return Ok(InstallHint::Mac);
        }
        return Ok(InstallHint::Unavailable);
    }
    #[cfg(not(all(target_arch = "wasm32", feature = "web")))]
    {
        return Ok(InstallHint::Unavailable);
    }
    #[allow(unreachable_code)]
    Ok(InstallHint::Unavailable)
}
