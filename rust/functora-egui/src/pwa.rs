use crate::error::Error;

fn js_escape(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('\'', "\\'")
        .replace('\n', "\\n")
}

#[must_use]
pub fn pwa_init_js(sw_url: &str, cache_name: &str) -> String {
    let sw_url_js = js_escape(sw_url);
    let cache_name_js = js_escape(cache_name);
    format!(
        r"if('serviceWorker' in navigator){{window.addEventListener('load',()=>{{navigator.serviceWorker.register('{sw_url_js}').catch(e=>console.error(e))}})}}window.addEventListener('beforeinstallprompt',e=>{{e.preventDefault();window.__functoraPwaDeferred=e}});window.__functoraCacheName='{cache_name_js}';"
    )
}

#[must_use]
pub fn pwa_sw_js(cache_name: &str, assets: &[&str]) -> String {
    let cache_name_js = js_escape(cache_name);
    let assets_js = assets
        .iter()
        .map(|a| format!("'{}'", js_escape(a)))
        .collect::<Vec<_>>()
        .join(",");
    format!(
        r"const CACHE='{cache_name_js}';const ASSETS=[{assets_js}];self.addEventListener('install',e=>{{e.waitUntil(caches.open(CACHE).then(c=>c.addAll(ASSETS)))}});self.addEventListener('activate',e=>{{e.waitUntil(caches.keys().then(keys=>Promise.all(keys.filter(k=>k!==CACHE).map(k=>caches.delete(k)))) )}});self.addEventListener('fetch',e=>{{e.respondWith(caches.match(e.request).then(r=>r||fetch(e.request)))}});"
    )
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
