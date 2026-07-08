use crate::messages::Msg;
use crate::*;
use functora_dioxus::i18n::I18N;
use functora_dioxus::widgets::QrScanner as BaseQrScanner;

#[component]
pub fn QrScanner(
    on_scan: EventHandler<String>,
    #[props(default)] message: Option<Signal<Option<Msg>>>,
    lang: Language,
) -> Element {
    let on_error = move |err: functora_dioxus::Error| {
        if let Some(ref mut msg) = message {
            msg.set(Some(Msg::Error(
                AppError::Fd(err).render(lang),
            )));
        }
    };

    rsx! {
        BaseQrScanner { on_scan, on_error: Callback::new(on_error), lang }
    }
}
