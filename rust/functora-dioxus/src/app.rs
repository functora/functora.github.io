use crate::storage::use_storage;
use dioxus::prelude::*;
pub use functora_tagged::InfallibleInto;
use functora_tagged::{FCrude, Tagged};
use serde::Serialize;
use serde::de::DeserializeOwned;

pub enum DAppName {}
pub type AppName = Tagged<&'static str, DAppName, FCrude>;

pub enum DAppId {}
pub type AppId = Tagged<&'static str, DAppId, FCrude>;

pub struct AppAssets {
    pub icon_ico: Asset,
    pub icon_16_png: Asset,
    pub icon_32_png: Asset,
    pub apple_touch_icon_png: Asset,
    pub icon_192_png: Asset,
    pub icon_512_png: Asset,
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
            css: vec![asset!(
                "/assets/functora.min.css",
                AssetOptions::css().with_minify(false).into_asset_options()
            )],
        }
    }
}

fn manifest(name: &str, icon_192_png: &Asset, icon_512_png: &Asset) -> String {
    format!(
        r#"{{"name":"{name}","short_name":"{name}","icons":[{{"src":"{icon_192_png}","sizes":"192x192","type":"image/png"}},{{"src":"{icon_512_png}","sizes":"512x512","type":"image/png"}}],"display":"standalone"}}"#
    )
}

#[allow(non_snake_case)]
pub fn App<T, P, R>(id: AppId, name: AppName, assets: AppAssets) -> Element
where
    T: Default + 'static,
    P: Serialize + DeserializeOwned + Clone + Send + Sync + PartialEq + Default + 'static,
    R: Routable + Default + PartialEq + 'static,
{
    let tst = use_store(T::default);
    let pst = use_storage(*id, P::default);
    let _ = use_context_provider(|| tst);
    let _ = use_context_provider(|| pst);

    let AppAssets {
        icon_ico,
        icon_16_png,
        icon_32_png,
        apple_touch_icon_png,
        icon_192_png,
        icon_512_png,
        css,
    } = assets;

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
        document::Link {
            rel: "manifest",
            href: "data:application/manifest+json,{manifest(*name, &icon_192_png, &icon_512_png)}",
        }
        document::Title { "{name}" }
        for css in &css {
            document::Link { rel: "stylesheet", href: *css }
        }
        Router::<R> {}
    }
}
