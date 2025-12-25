use crate::*;

#[component]
pub fn About() -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let t = get_translations(cfg.read().language);

    rsx! {
        Breadcrumb { title: t.about_title.to_string() }
        section {
            pre { white_space: "pre-wrap", "{t.about_text}" }
            Dock {}
        }
    }
}
