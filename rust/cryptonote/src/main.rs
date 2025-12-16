mod components;
mod crypto;
mod encoding;
mod error;
mod i18n;
mod prelude;

pub use components::*;
pub use crypto::*;
pub use encoding::*;
pub use error::*;
pub use i18n::*;
pub use prelude::*;

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
    let language =
        use_signal(i18n::detect_browser_language);

    let app_context = use_signal(AppContext::default);

    let nav_state = use_signal(NavigationState::default);

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
