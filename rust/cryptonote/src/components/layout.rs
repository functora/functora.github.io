use crate::messages::*;
use crate::*;
use functora_dioxus::{WhiteLabelConfig, WhiteLabelLayout};

#[component]
pub fn Layout() -> Element {
    let tst = use_context::<Store<TemporaryState>>();
    rsx! {
        WhiteLabelLayout {
            config: WhiteLabelConfig {
                brand: "🔐 Cryptonote".to_string(),
                copyright_owner: "Functora".to_string(),
                copyright_owner_href: Some(FUNCTORA_URL.to_string()),
                year: 2025,
                version: Some(APP_VERSION.to_string()),
                home: Screen::Home.to_route(None),
                about: Some(Screen::About.to_route(None)),
                about_icon: Some(FaAndroid),
                donate: Some(Screen::Donate.to_route(None)),
                license: Some(Screen::License.to_route(None)),
                privacy: Some(Screen::Privacy.to_route(None)),
                share: Some(Screen::About.to_route(Some(SHARE_APP_ID.into()))),
                on_brand_click: Some(EventHandler::new(move |_| reset_temporary_state(tst))),
                bottom_extra: rsx! {
                    ProgressBar {}
                },
            },
            children: rsx! {
                DeepLinkHandler {}
                Outlet::<Route> {}
            },
        }
    }
}

#[component]
fn DeepLinkHandler() -> Element {
    let tst = use_context::<Store<TemporaryState>>();
    let message = use_message();
    let mut nav = use_context::<Signal<Nav<Route>>>();

    use_hook(|| {
        crate::deep_link::set_schedule_update(dioxus::core::schedule_update());
    });

    dioxus::core::use_after_render(move || {
        if let Some(source) = crate::deep_link::take_archive() {
            let mut message_out = message;
            let _ = spawn(async move {
                if let Err(e) = crate::hooks::open_archive_async(source, tst, nav).await {
                    message_out.set(Some(Msg::Error(e.into())));
                }
            });
        } else if let Some(route) = crate::deep_link::take_url().and_then(|url| crate::deep_link::url_to_route(&url)) {
            nav.write().push_route(&route);
        }
    });

    rsx! {}
}
