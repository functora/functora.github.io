use crate::error::Error;
use crate::i18n::I18N;
use dioxus::prelude::*;
use either::Either;
use serde::{Deserialize, Serialize};

#[derive(Copy, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Theme {
    Light,
    Dark,
}

impl std::fmt::Display for Theme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Theme::Light => "Light",
            Theme::Dark => "Dark",
        })
    }
}

impl Theme {
    #[must_use]
    pub fn next(self) -> Self {
        match self {
            Theme::Light => Theme::Dark,
            Theme::Dark => Theme::Light,
        }
    }

    pub fn to_js_value(&self) -> String {
        self.to_string().to_lowercase()
    }
}

pub async fn set_theme(theme: &Theme) -> Result<(), Error> {
    eval(
        theme.to_js_value(),
        r#"function(arg){
        await window
        .document
        .documentElement
        .setAttribute("data-theme", arg);
        return null;
        }"#,
    )
    .await
}

#[cfg(not(target_os = "android"))]
pub async fn read_clipboard() -> Result<String, Error> {
    eval(
        (),
        r"function(arg){
        return await window.navigator.clipboard.readText();
        }",
    )
    .await
}

#[cfg(target_os = "android")]
fn jni_dispatch<T: Send + 'static>(
    f: impl FnOnce(&mut jni::JNIEnv, &jni::objects::JObject) -> Result<T, jni::errors::Error> + Send + 'static,
) -> Result<T, Error> {
    use std::sync::mpsc::channel;
    let (tx, rx) = channel();
    dioxus::mobile::wry::prelude::dispatch(move |env: &mut jni::JNIEnv, activity: &jni::objects::JObject, _| {
        _ = tx.send(f(env, activity).map_err(Error::from));
    });
    rx.recv()?
}

#[cfg(target_os = "android")]
pub async fn read_clipboard() -> Result<String, Error> {
    jni_dispatch(|env, activity| {
        use jni::objects::JString;
        let svc_name: JString = env.new_string("clipboard")?;
        let clipboard_svc = env
            .call_method(
                activity,
                "getSystemService",
                "(Ljava/lang/String;)Ljava/lang/Object;",
                &[(&svc_name).into()],
            )?
            .l()?;
        let clipboard = env.new_global_ref(clipboard_svc)?;
        let clip = env
            .call_method(
                clipboard.as_obj(),
                "getPrimaryClip",
                "()Landroid/content/ClipData;",
                &[],
            )?
            .l()?;
        let item = env
            .call_method(
                &clip,
                "getItemAt",
                "(I)Landroid/content/ClipData$Item;",
                &[jni::objects::JValue::Int(0)],
            )?
            .l()?;
        let text_obj = env
            .call_method(&item, "getText", "()Ljava/lang/CharSequence;", &[])?
            .l()?;
        let s = JString::from(text_obj);
        Ok(env.get_string(&s).map(String::from)?)
    })
}

#[cfg(not(target_os = "android"))]
pub async fn clipboard_write(msg: String) -> Result<(), Error> {
    eval(
        msg,
        r"function(arg){
        await window.navigator.clipboard.writeText(arg);
        return null;
        }",
    )
    .await
}

#[cfg(target_os = "android")]
pub async fn clipboard_write(msg: String) -> Result<(), Error> {
    jni_dispatch(move |env, activity| {
        use jni::objects::JString;
        let label: JString = env.new_string("Cryptonote")?;
        let text: JString = env.new_string(&msg)?;
        let svc_name: JString = env.new_string("clipboard")?;
        let clipboard_svc = env
            .call_method(
                activity,
                "getSystemService",
                "(Ljava/lang/String;)Ljava/lang/Object;",
                &[(&svc_name).into()],
            )?
            .l()?;
        let clipboard = env.new_global_ref(clipboard_svc)?;
        let clip_data = env
            .call_static_method(
                "android/content/ClipData",
                "newPlainText",
                "(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Landroid/content/ClipData;",
                &[(&label).into(), (&text).into()],
            )?
            .l()?;
        env.call_method(
            clipboard.as_obj(),
            "setPrimaryClip",
            "(Landroid/content/ClipData;)V",
            &[(&clip_data).into()],
        )?;
        Ok(())
    })
}

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

#[allow(clippy::needless_raw_string_hashes)]
pub async fn sleep(millis: u64) -> Result<(), Error> {
    eval(
        millis,
        r#"function(arg){
        return new Promise(resolve => setTimeout(resolve, arg));
        }"#,
    )
    .await
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

async fn eval<A: Serialize + 'static, B: serde::de::DeserializeOwned + 'static>(
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

    eval.send(arg).map_err(Error::from)?;
    match eval.recv::<Either<String, B>>().await {
        Ok(Either::Right(rhs)) => Ok(rhs),
        Ok(Either::Left(lhs)) => Err(Error::from(dioxus::document::EvalError::InvalidJs(lhs))),
        Err(e) => Err(Error::from(e)),
    }
}
