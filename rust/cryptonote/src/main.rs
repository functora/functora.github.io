use cryptonote::*;

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
const NO_CSS_MINIFY: AssetOptions = AssetOptions::css()
    .with_minify(false)
    .into_asset_options();

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    let tst = use_store(TemporaryState::default);
    let pst = use_storage(
        APP_STORAGE_KEY,
        PersistentState::default,
    );

    use_context_provider(|| tst);
    use_context_provider(|| pst);

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
        document::Link {
            rel: "stylesheet",
            href: asset!("/assets/bare.min.css", NO_CSS_MINIFY),
        }
        document::Link { rel: "stylesheet", href: asset!("/assets/app.css") }
        Router::<Route> {}
    }
}
