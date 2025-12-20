use crate::*;

#[component]
pub fn Dock(
    children: Element,
    message: Signal<Option<UiMessage>>,
) -> Element {
    let app_settings = use_context::<Signal<AppCfg>>();
    let t = get_translations(app_settings.read().language);
    let nav = use_app_nav();

    rsx! {
        p { "txt": "r",
            Message { message }
            if nav.has_navigated() {
                Button {
                    icon: FaArrowLeft,
                    onclick: move |_| {
                        nav.go_back();
                    },
                    "{t.back_button}"
                }
            }
            {children}
        }
    }
}
