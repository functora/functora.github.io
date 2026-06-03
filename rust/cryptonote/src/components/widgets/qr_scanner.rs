use crate::*;
use functora_dioxus::widgets::QrScanner as BaseQrScanner;

#[component]
pub fn QrScanner(
    on_scan: EventHandler<String>,
    #[props(default)] message: Option<
        Signal<Option<UiMessage>>,
    >,
) -> Element {
    let mut message = message;

    let on_error = move |err: functora_dioxus::Error| {
        if let Some(ref mut msg) = message {
            msg.set(Some(UiMessage::Error(err.into())));
        }
    };

    rsx! {
        BaseQrScanner {
            on_scan,
            on_error: Callback::new(on_error),
        }
    }
}
