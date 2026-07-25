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
        let _ = env.call_method(
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

#[derive(Serialize)]
pub struct ShareData {
    pub title: String,
    pub text: String,
    pub url: String,
}

#[cfg(not(target_os = "android"))]
pub async fn web_share(data: ShareData) -> Result<(), Error> {
    eval(
        data,
        r"function(arg){
        return navigator.share({
            title: arg.title,
            text: arg.text,
            url: arg.url
        });
        }",
    )
    .await
}

#[cfg(target_os = "android")]
pub async fn web_share(data: ShareData) -> Result<(), Error> {
    jni_dispatch(move |env, activity| {
        let intent_class = env.find_class("android/content/Intent")?;
        let action_send = env
            .get_static_field(&intent_class, "ACTION_SEND", "Ljava/lang/String;")?
            .l()?;
        let intent = env.new_object(&intent_class, "(Ljava/lang/String;)V", &[(&action_send).into()])?;
        let intent = env.new_global_ref(intent)?;
        let text_type = env.new_string("text/plain")?;
        let _ = env.call_method(
            intent.as_obj(),
            "setType",
            "(Ljava/lang/String;)Landroid/content/Intent;",
            &[(&text_type).into()],
        )?;
        let extra_text = env
            .get_static_field(&intent_class, "EXTRA_TEXT", "Ljava/lang/String;")?
            .l()?;
        let share_text = env.new_string(format!("{}\n{}", data.text, data.url))?;
        let _ = env.call_method(
            intent.as_obj(),
            "putExtra",
            "(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;",
            &[(&extra_text).into(), (&share_text).into()],
        )?;
        let extra_subject = env
            .get_static_field(&intent_class, "EXTRA_SUBJECT", "Ljava/lang/String;")?
            .l()?;
        let title = env.new_string(&data.title)?;
        let _ = env.call_method(
            intent.as_obj(),
            "putExtra",
            "(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;",
            &[(&extra_subject).into(), (&title).into()],
        )?;
        let chooser_title = env.new_string("Share via")?;
        let chooser = env.call_static_method(
            "android/content/Intent",
            "createChooser",
            "(Landroid/content/Intent;Ljava/lang/CharSequence;)Landroid/content/Intent;",
            &[(&intent.as_obj()).into(), (&chooser_title).into()],
        )?;
        let chooser = chooser.l()?;
        let flags = env
            .get_static_field(&intent_class, "FLAG_ACTIVITY_NEW_TASK", "I")?
            .i()?;
        let _ = env.call_method(
            intent.as_obj(),
            "setFlags",
            "(I)Landroid/content/Intent;",
            &[jni::objects::JValue::Int(flags)],
        )?;
        let _ = env.call_method(
            activity,
            "startActivity",
            "(Landroid/content/Intent;)V",
            &[(&chooser).into()],
        )?;
        Ok(())
    })
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
