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

const MOBILE_NAV: &str = "
@media (max-width:50rem) {
  nav:not([class]){position:fixed;padding:0;top:0;left:0;right:0}
  nav:not([class]) label{height:4.4rem}
  nav:not([class]) label>ul{margin:0;list-style:none;position:fixed;top:4.4rem;right:0;bottom:0;padding:0;transform:translate3d(100%,0,0);border-top:1px solid var(--primary);text-align:left;background:var(--surface);box-shadow:0 1px 4px 0 rgba(0,0,0,.2);overflow-x:visible;overflow-y:auto;z-index:12}
  nav:not([class]) label>ul li{display:block;border-top:1px solid var(--grey-light);font-size:1.6rem}
  nav:not([class]) label>ul a{padding:1rem 6rem 1rem 2rem;border:none}
  nav:not([class]) label input:checked~ul{transform:none;pointer-events:none}
  nav:not([class]) label input:checked~ul a{pointer-events:auto}
  body{padding-top:4.4rem}
}
";

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
            use_synced_storage, LocalStorage,
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
        document::Style { {MOBILE_NAV} }
        Router::<Route> {}
    }
}
