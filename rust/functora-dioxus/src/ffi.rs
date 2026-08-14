use crate::error::Error;
use crate::i18n::I18N;
use dioxus::prelude::*;
use either::Either;
use serde::{Deserialize, Serialize};

#[derive(Copy, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, derive_more::Display)]
pub enum Theme {
    #[display("Light")]
    Light,
    #[display("Dark")]
    Dark,
}

impl Theme {
    #[must_use]
    pub fn next(self) -> Self {
        match self {
            Theme::Light => Theme::Dark,
            Theme::Dark => Theme::Light,
        }
    }

    #[must_use]
    pub fn to_js_value(&self) -> String {
        self.to_string().to_lowercase()
    }
}

pub async fn set_theme(theme: &Theme) -> Result<(), Error> {
    eval(
        theme.to_js_value(),
        r#"function(arg){
        const el = window.document.documentElement;
        if (arg === "dark") {
            el.removeAttribute("functora-theme-light");
            el.setAttribute("functora-theme-dark", "");
        } else {
            el.removeAttribute("functora-theme-dark");
            el.setAttribute("functora-theme-light", "");
        }
        return null;
        }"#,
    )
    .await
}
#[cfg(target_os = "android")]
pub use crate::android::{clipboard_write, print_page, read_clipboard, social_share};

#[cfg(not(target_os = "android"))]
pub use crate::web::{clipboard_write, print_page, read_clipboard, social_share};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FrameData {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
}

pub async fn check_camera() -> Result<(), Error> {
    eval(
        (),
        r#"function(arg){
        if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
        throw new Error("Camera API not available");
        }
        return null;
        }"#,
    )
    .await
}

pub async fn start_camera() -> Result<(), Error> {
    eval(
        (),
        r#"function(arg){
        const stream = await navigator.mediaDevices.getUserMedia({
        video: {
        facingMode: "environment"
        }
        });
        const video = document.getElementById("qr-video");
        if (video) {
        video.srcObject = stream;
        }
        return null;
        }"#,
    )
    .await
}

pub async fn capture_frame() -> Result<FrameData, Error> {
    eval(
        (),
        r#"function(arg){
        const video = document.getElementById("qr-video");
        const canvas = document.getElementById("qr-canvas");
        if (!video || !canvas) {
        throw new Error("Video or canvas not found");
        }
        const ctx = canvas.getContext("2d");
        const w = video.videoWidth;
        const h = video.videoHeight;
        canvas.width = w;
        canvas.height = h;
        ctx.drawImage(video, 0, 0);
        const imageData = ctx.getImageData(0, 0, w, h);
        return {
        data: Array.from(imageData.data),
        width: w,
        height: h
        };
        }"#,
    )
    .await
}

pub async fn stop_camera() -> Result<(), Error> {
    eval(
        (),
        r#"function(arg){
        const video = document.getElementById("qr-video");
        if (video && video.srcObject) {
        const tracks = video.srcObject.getTracks();
        tracks.forEach(track => track.stop());
        video.srcObject = null;
        }
        return null;
        }"#,
    )
    .await
}

#[derive(Serialize)]
pub struct ShareData {
    pub title: String,
    pub text: String,
    pub url: String,
}

pub async fn sleep(millis: u64) -> Result<(), Error> {
    eval(
        millis,
        r"function(arg){
        return new Promise(resolve => setTimeout(resolve, arg));
        }",
    )
    .await
}

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub enum PwaInstallOutcome {
    Accepted,
    Rejected,
    NotAvailable,
}

const PWA_INSTALL_TRIGGER_JS: &str = r#"function(arg) {
    if (!window.__functoraPwaDeferred) {
        return "NotAvailable";
    }
    const d = window.__functoraPwaDeferred;
    d.prompt();
    const result = await d.userChoice;
    window.__functoraPwaDeferred = null;
    return result.outcome === 'accepted' ? 'Accepted' : 'Rejected';
}
"#;

pub async fn trigger_pwa_install() -> Result<PwaInstallOutcome, Error> {
    let result: String = eval((), PWA_INSTALL_TRIGGER_JS).await?;
    Ok(match result.as_str() {
        "Accepted" => PwaInstallOutcome::Accepted,
        "Rejected" => PwaInstallOutcome::Rejected,
        _ => PwaInstallOutcome::NotAvailable,
    })
}

pub fn write_clipboard<S: I18N + 'static>(
    val: String,
    mut message: impl Writable<Target = Option<S>> + 'static,
    success: S,
    map_error: impl FnOnce(crate::Error) -> S + 'static,
) {
    let _ = spawn(async move {
        match clipboard_write(val).await {
            Ok(()) => message.set(Some(success)),
            Err(e) => message.set(Some(map_error(e))),
        }
    });
}

pub(crate) async fn eval<A: Serialize + 'static, B: serde::de::DeserializeOwned + 'static>(
    arg: A,
    fun: &'static str,
) -> Result<B, Error> {
    let code = &format!(
        r#"
        let arg = await dioxus.recv();
        try {{
        let res = await (async {fun})(arg);
        dioxus.send({{"Right": res}});
        }} catch (e) {{
        dioxus.send({{"Left": String(e)}});
        }}
        "#
    );

    let mut eval = dioxus::document::eval(code);

    eval.send(arg)?;
    match eval.recv::<Either<String, B>>().await? {
        Either::Right(rhs) => Ok(rhs),
        Either::Left(lhs) => Err(Error::from(dioxus::document::EvalError::InvalidJs(lhs))),
    }
}
