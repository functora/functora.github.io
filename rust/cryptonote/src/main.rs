use cryptonote::*;

const FAVICON_ICO: Asset = asset!("/assets/favicon/favicon.ico");
const FAVICON_16: Asset = asset!("/assets/favicon/favicon-16x16.png");
const FAVICON_32: Asset = asset!("/assets/favicon/favicon-32x32.png");
const APPLE_TOUCH_ICON: Asset = asset!("/assets/favicon/apple-touch-icon.png");
const WEB_MANIFEST: Asset = asset!("/assets/favicon/site.webmanifest");
const NO_CSS_MINIFY: AssetOptions = AssetOptions::css().with_minify(false).into_asset_options();

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    functora_dioxus::app::App::<TemporaryState, PersistentState, Route>(
        AppName::new("Cryptonote").infallible(),
        AppStorage::new(APP_STORAGE_KEY).infallible(),
        AppAssets {
            favicon_ico: FAVICON_ICO,
            favicon_16: FAVICON_16,
            favicon_32: FAVICON_32,
            apple_touch_icon: APPLE_TOUCH_ICON,
            manifest: WEB_MANIFEST,
            css: vec![
                asset!("/assets/functora.min.css", NO_CSS_MINIFY),
                asset!("/assets/app.css"),
            ],
        },
    )
}
