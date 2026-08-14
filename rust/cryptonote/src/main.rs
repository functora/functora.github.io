use cryptonote::*;

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    functora_dioxus::App::<TemporaryState, PersistentState, Route>(
        APP_ATTRS,
        AppAssets {
            icon_ico: asset!("/assets/favicon/favicon.ico"),
            icon_16_png: asset!("/assets/favicon/favicon-16x16.png"),
            icon_32_png: asset!("/assets/favicon/favicon-32x32.png"),
            apple_touch_icon_png: asset!("/assets/favicon/apple-touch-icon.png"),
            icon_192_png: asset!("/assets/favicon/android-chrome-192x192.png"),
            icon_512_png: asset!("/assets/favicon/android-chrome-512x512.png"),
            sw_js: AppAssets::default().sw_js,
            css: AppAssets::default().css,
        },
    )
}
