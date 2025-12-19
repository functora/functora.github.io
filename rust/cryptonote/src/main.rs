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
use dioxus_sdk::storage::use_persistent;
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
    let app_settings =
        use_persistent("settings", AppSettings::default);
    let app_context = use_signal(AppContext::default);
    let has_navigated = use_signal(|| false);

    use_context_provider(|| app_settings);
    use_context_provider(|| app_context);
    use_context_provider(|| has_navigated);

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
