use cryptonote::*;

const FAVICON_ICO: Asset = asset!("/assets/favicon/favicon.ico");
const FAVICON_16: Asset = asset!("/assets/favicon/favicon-16x16.png");
const FAVICON_32: Asset = asset!("/assets/favicon/favicon-32x32.png");
const APPLE_TOUCH_ICON: Asset = asset!("/assets/favicon/apple-touch-icon.png");
const ICON_192: Asset = asset!("/assets/favicon/android-chrome-192x192.png");
const ICON_512: Asset = asset!("/assets/favicon/android-chrome-512x512.png");
const NO_CSS_MINIFY: AssetOptions = AssetOptions::css().with_minify(false).into_asset_options();

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    functora_dioxus::app::App::<TemporaryState, PersistentState, Route>(
        AppId::new(APP_ID).infallible(),
        AppName::new(APP_NAME).infallible(),
        AppAssets {
            icon_ico: FAVICON_ICO,
            icon_16_png: FAVICON_16,
            icon_32_png: FAVICON_32,
            apple_touch_icon_png: APPLE_TOUCH_ICON,
            icon_192_png: ICON_192,
            icon_512_png: ICON_512,
            css: vec![
                asset!("/assets/functora.min.css", NO_CSS_MINIFY),
                asset!("/assets/app.css"),
            ],
        },
    )
}
