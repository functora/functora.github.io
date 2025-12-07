use components::{
    Home, Layout, License, Privacy, Share, View,
};
use dioxus::prelude::*;
mod components;
mod crypto;
mod encoding;
mod error;
mod i18n;
pub mod prelude;

#[derive(Debug, Clone, Routable, PartialEq)]
#[rustfmt::skip]
enum Route {
    #[layout(Layout)]
        #[route("/")]
        Home {},
        #[route("/view?:note")]
        View {note: Option<String>},
        #[route("/share")]
        Share {},
        #[route("/license")]
        License {},
        #[route("/privacy")]
        Privacy {},
}

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

#[derive(Clone, Debug, Default)]
pub struct AppContext {
    pub content: Option<String>,
    pub password: String,
    pub cipher: Option<crypto::CipherType>,
    pub share_url: Option<String>,
    pub qr_code: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub struct NavigationState {
    pub has_navigated: bool,
}

#[component]
fn App() -> Element {
    let language =
        use_signal(|| i18n::detect_browser_language());

    let app_context = use_signal(AppContext::default);

    let nav_state =
        use_signal(|| NavigationState::default());

    use_context_provider(|| language);
    use_context_provider(|| app_context);
    use_context_provider(|| nav_state);

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
