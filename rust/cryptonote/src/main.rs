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
        #[route("/view")]
        View {},
        #[route("/share")]
        Share {},
        #[route("/license")]
        License {},
        #[route("/privacy")]
        Privacy {},
}

const FAVICON: Asset = asset!("/assets/favicon.ico");

fn main() {
    dioxus::launch(App);
}

#[derive(Clone, Debug)]
pub struct AppContext {
    pub content: Option<String>,
    pub password: String,
    pub cipher: Option<crypto::CipherType>,
    pub share_url: Option<String>,
    pub qr_code: Option<String>,
}

#[component]
fn App() -> Element {
    let language =
        use_signal(|| i18n::detect_browser_language());

    let app_context = use_signal(|| AppContext {
        content: None,
        password: String::new(),
        cipher: None,
        share_url: None,
        qr_code: None,
    });

    use_context_provider(|| language);
    use_context_provider(|| app_context);

    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: asset!("/assets/bare.min.css") }
        document::Link { rel: "stylesheet", href: asset!("/assets/app.css") }
        Router::<Route> {}
    }
}
