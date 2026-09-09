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
