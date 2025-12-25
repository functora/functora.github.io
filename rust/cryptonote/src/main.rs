mod components;
mod crypto;
mod encoding;
mod error;
mod hooks;
mod i18n;
mod js;
mod prelude;
mod storage;

pub use components::*;
pub use crypto::*;
pub use encoding::*;
pub use error::*;
pub use hooks::*;
pub use i18n::*;
pub use js::*;
pub use prelude::*;
pub use storage::*;

const FAVICON_ICO: Asset =
    asset!("/assets/favicon/favicon.ico");
const FAVICON_16: Asset =
    asset!("/assets/favicon/favicon-16x16.png");
const FAVICON_32: Asset =
    asset!("/assets/favicon/favicon-32x32.png");
const APPLE_TOUCH_ICON: Asset =
    asset!("/assets/favicon/apple-touch-icon.png");
const WEB_MANIFEST: Asset =
    asset!("/assets/favicon/site.webmanifest");

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    let nav: Signal<u32> = use_signal(|| 0);
    let ctx = use_signal(AppCtx::default);
    #[cfg(target_arch = "wasm32")]
    let cfg = {
        use dioxus_sdk::storage::{
            LocalStorage, use_synced_storage,
        };
        use_synced_storage::<LocalStorage, AppCfg>(
            APP_STORAGE_KEY.to_string(),
            AppCfg::default,
        )
    };
    #[cfg(not(target_arch = "wasm32"))]
    let cfg = {
        use storage::mobile::use_storage;
        use_storage(APP_STORAGE_KEY, AppCfg::default)
            .unwrap_or_else(|e| {
                tracing::error!(
                    "Storage init error: {}",
                    e
                );
                use_signal(AppCfg::default)
            })
    };

    use_context_provider(|| nav);
    use_context_provider(|| ctx);
    use_context_provider(|| cfg);

    rsx! {
        document::Link { rel: "icon", r#type: "image/x-icon", href: FAVICON_ICO }
        document::Link {
            rel: "icon",
            r#type: "image/png",
            sizes: "16x16",
            href: FAVICON_16,
        }
        document::Link {
            rel: "icon",
            r#type: "image/png",
            sizes: "32x32",
            href: FAVICON_32,
        }
        document::Link {
            rel: "apple-touch-icon",
            sizes: "180x180",
            href: APPLE_TOUCH_ICON,
        }
        document::Link { rel: "manifest", href: WEB_MANIFEST }
        document::Link { rel: "stylesheet", href: asset!("/assets/bare.min.css") }
        document::Link { rel: "stylesheet", href: asset!("/assets/app.css") }
        Router::<Route> {}
    }
}
