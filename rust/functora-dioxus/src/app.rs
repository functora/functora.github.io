#![allow(clippy::shadow_reuse)]
use crate::AppAttrs;
use crate::storage::use_storage;
use dioxus::prelude::*;
pub use functora_tagged::InfallibleInto;
use serde::Serialize;
use serde::de::DeserializeOwned;

#[derive(Clone, PartialEq)]
pub struct AppAssets {
    pub icon_ico: Asset,
    pub icon_16_png: Asset,
    pub icon_32_png: Asset,
    pub apple_touch_icon_png: Asset,
    pub icon_192_png: Asset,
    pub icon_512_png: Asset,
    pub sw_js: Asset,
    pub css: Vec<Asset>,
}

impl Default for AppAssets {
    fn default() -> Self {
        Self {
            icon_ico: asset!("/assets/favicon.ico"),
            icon_16_png: asset!("/assets/favicon-16x16.png"),
            icon_32_png: asset!("/assets/favicon-32x32.png"),
            apple_touch_icon_png: asset!("/assets/apple-touch-icon.png"),
            icon_192_png: asset!("/assets/android-chrome-192x192.png"),
            icon_512_png: asset!("/assets/android-chrome-512x512.png"),
            sw_js: asset!("/assets/sw.js"),
            css: vec![asset!(
                "/assets/functora.min.css",
                AssetOptions::css().with_minify(false).into_asset_options()
            )],
        }
    }
}

pub struct ManifestIcon {
    pub src: String,
    pub sizes: &'static str,
    pub r#type: &'static str,
    pub purpose: &'static str,
}

#[must_use]
pub fn manifest_json(
    app: &str,
    vsn: &str,
    description: &str,
    start_url: &str,
    scope: &str,
    icons: &[ManifestIcon],
) -> String {
    let name = crate::white_label::capitalize_first(app);
    let icons_json = icons
        .iter()
        .map(|icon| {
            format!(
                "{{\"src\":{},\"sizes\":\"{}\",\"type\":\"{}\",\"purpose\":\"{}\"}}",
                json_str(&icon.src),
                icon.sizes,
                icon.r#type,
                icon.purpose
            )
        })
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "{{\"name\":{},\"short_name\":{},\"description\":{},\"start_url\":{},\"scope\":{},\"display\":\"standalone\",\"theme_color\":\"#679\",\"background_color\":\"#ffffff\",\"cache_name\":{},\"icons\":[{icons_json}]}}",
        json_str(&name),
        json_str(&name),
        json_str(description),
        json_str(start_url),
        json_str(scope),
        json_str(&format!("{app}-v{vsn}")),
    )
}

fn json_str(s: &str) -> String {
    serde_json::to_string(s).unwrap_or_default()
}

#[must_use]
pub fn pwa_init_js(sw_url: &str, cache_name: &str) -> String {
    format!(
        "if('serviceWorker' in navigator){{navigator.serviceWorker.register('{sw_url}?cache={cache_name}').catch(e=>console.error('SW registration failed:',e));}}window.__functoraPwaDeferred=null;window.addEventListener('beforeinstallprompt',(e)=>{{window.__functoraPwaDeferred=e;}});window.addEventListener('appinstalled',()=>{{window.__functoraPwaDeferred=null;}});"
    )
}

#[allow(non_snake_case)]
#[component]
fn AppMeta(attrs: AppAttrs, assets: AppAssets) -> Element {
    let AppAssets {
        icon_ico,
        icon_16_png,
        icon_32_png,
        apple_touch_icon_png,
        css,
        #[cfg(target_arch = "wasm32")]
        sw_js,
        ..
    } = assets;

    #[cfg(target_arch = "wasm32")]
    let pwa_script = rsx! {
        document::Script {
            "{pwa_init_js(&sw_js.to_string(), &attrs.cache_name())}"
        }
    };
    #[cfg(not(target_arch = "wasm32"))]
    let pwa_script = rsx! {};

    #[cfg(target_arch = "wasm32")]
    let manifest_link = attrs
        .manifest_uri(&assets.icon_192_png.to_string(), &assets.icon_512_png.to_string())
        .map(|href| {
            rsx! {
                document::Link { rel: "manifest", href: href }
            }
        });
    #[cfg(not(target_arch = "wasm32"))]
    let manifest_link = rsx! {};

    rsx! {
        document::Link { rel: "icon", r#type: "image/x-icon", href: icon_ico }
        document::Link {
            rel: "icon",
            r#type: "image/png",
            sizes: "16x16",
            href: icon_16_png,
        }
        document::Link {
            rel: "icon",
            r#type: "image/png",
            sizes: "32x32",
            href: icon_32_png,
        }
        document::Link {
            rel: "apple-touch-icon",
            sizes: "180x180",
            href: apple_touch_icon_png,
        }
        {manifest_link}
        document::Title { "{attrs.app_name()}" }
        for url in &css {
            document::Link { rel: "stylesheet", href: *url }
        }
        {pwa_script}
    }
}

#[allow(non_snake_case)]
pub fn App<T, P, R>(attrs: AppAttrs, assets: AppAssets) -> Element
where
    T: Default + 'static,
    P: Serialize + DeserializeOwned + Clone + Send + Sync + PartialEq + Default + 'static,
    R: Routable + Default + PartialEq + 'static,
{
    let tst = use_store(T::default);
    let pst = use_storage(attrs.app, P::default);
    let _ = use_context_provider(|| tst);
    let _ = use_context_provider(|| pst);

    rsx! {
        AppMeta { attrs, assets }
        Router::<R> {}
    }
}
