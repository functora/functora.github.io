use crate::storage::use_storage;
use dioxus::prelude::*;
pub use functora_tagged::InfallibleInto;
use functora_tagged::{FCrude, Tagged};
use serde::Serialize;
use serde::de::DeserializeOwned;

pub enum DAppName {}
pub type AppName = Tagged<&'static str, DAppName, FCrude>;

pub enum DAppStorage {}
pub type AppStorage = Tagged<&'static str, DAppStorage, FCrude>;

pub struct AppAssets {
    pub favicon_ico: Asset,
    pub favicon_16: Asset,
    pub favicon_32: Asset,
    pub apple_touch_icon: Asset,
    pub manifest: Asset,
    pub css: Vec<Asset>,
}

impl Default for AppAssets {
    fn default() -> Self {
        Self {
            favicon_ico: asset!("/assets/favicon.ico"),
            favicon_16: asset!("/assets/favicon-16x16.png"),
            favicon_32: asset!("/assets/favicon-32x32.png"),
            apple_touch_icon: asset!("/assets/apple-touch-icon.png"),
            manifest: asset!("/assets/site.webmanifest"),
            css: vec![asset!(
                "/assets/functora.min.css",
                AssetOptions::css().with_minify(false).into_asset_options()
            )],
        }
    }
}

#[allow(non_snake_case)]
pub fn App<T, P, R>(name: AppName, storage: AppStorage, assets: AppAssets) -> Element
where
    T: Default + 'static,
    P: Serialize + DeserializeOwned + Clone + Send + Sync + PartialEq + Default + 'static,
    R: Routable + Default + PartialEq + 'static,
{
    let tst = use_store(T::default);
    let pst = use_storage(*storage, P::default);
    let _ = use_context_provider(|| tst);
    let _ = use_context_provider(|| pst);

    let AppAssets {
        favicon_ico,
        favicon_16,
        favicon_32,
        apple_touch_icon,
        manifest,
        css,
    } = assets;

    rsx! {
        document::Link { rel: "icon", r#type: "image/x-icon", href: favicon_ico }
        document::Link {
            rel: "icon",
            r#type: "image/png",
            sizes: "16x16",
            href: favicon_16,
        }
        document::Link {
            rel: "icon",
            r#type: "image/png",
            sizes: "32x32",
            href: favicon_32,
        }
        document::Link {
            rel: "apple-touch-icon",
            sizes: "180x180",
            href: apple_touch_icon,
        }
        document::Link { rel: "manifest", href: manifest }
        document::Title { "{name}" }
        for css in &css {
            document::Link { rel: "stylesheet", href: *css }
        }
        Router::<R> {}
    }
}
