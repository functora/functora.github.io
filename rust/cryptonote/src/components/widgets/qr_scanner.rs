use crate::*;
use functora_dioxus::widgets::{
    QrMessage, QrScanner as BaseQrScanner,
};

#[component]
pub fn QrScanner(
    on_scan: EventHandler<String>,
    #[props(default)] message: Option<
        Signal<Option<UiMessage>>,
    >,
) -> Element {
    let mut message = message;

    let on_error = move |err: QrMessage| {
        if let Some(ref mut msg) = message {
            let app_err = match err {
                QrMessage::CameraNotAvailable(_) => {
                    AppError::CameraNotAvailable
                }
                QrMessage::CameraPermissionDenied(_) => {
                    AppError::CameraPermissionDenied
                }
            };
            msg.set(Some(UiMessage::Error(app_err)));
        }
    };

    rsx! {
        BaseQrScanner {
            on_scan,
            on_error: Callback::new(on_error),
        }
    }
}
