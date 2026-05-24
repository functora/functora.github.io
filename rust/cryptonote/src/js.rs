use crate::*;
use crate::{AppError, UiMessage};
#[cfg(target_os = "android")]
use std::sync::mpsc::channel;

pub fn write_clipboard(
    val: String,
    mut message: Signal<Option<UiMessage>>,
) {
    spawn(async move {
        match write_clipboard_impl(val).await {
            Ok(()) => message.set(Some(UiMessage::Copied)),
            Err(e) => {
                message.set(Some(UiMessage::Error(e)))
            }
        }
    });
}

#[cfg(not(target_os = "android"))]
async fn write_clipboard_impl(
    msg: String,
) -> Result<(), AppError> {
    js_fun(
        msg,
        r#"function(arg){
        await window
        .navigator
        .clipboard
        .writeText(arg);
        return null;
        }"#,
    )
    .await
    .map_err(AppError::JsWriteClipboard)
}

#[cfg(target_os = "android")]
async fn write_clipboard_impl(
    msg: String,
) -> Result<(), AppError> {
    let (tx, rx) = channel();
    dioxus::mobile::wry::prelude::dispatch(
        move |env: &mut jni::JNIEnv,
              activity: &jni::objects::JObject,
              _| {
            use jni::objects::JString;
            let res =
                (|| -> Result<(), jni::errors::Error> {
                    let label: JString =
                        env.new_string("Cryptonote")?;
                    let text: JString =
                        env.new_string(&msg)?;
                    let svc_name: JString =
                        env.new_string("clipboard")?;
                    let clipboard_svc = env.call_method(activity, "getSystemService", "(Ljava/lang/String;)Ljava/lang/Object;", &[(&svc_name).into()])?.l()?;
                    let clipboard =
                        env.new_global_ref(clipboard_svc)?;
                    let clip_data = env.call_static_method("android/content/ClipData", "newPlainText", "(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Landroid/content/ClipData;", &[(&label).into(), (&text).into()])?.l()?;
                    env.call_method(
                        clipboard.as_obj(),
                        "setPrimaryClip",
                        "(Landroid/content/ClipData;)V",
                        &[(&clip_data).into()],
                    )?;
                    Ok(())
                })();
            tx.send(res.map_err(|e| {
                AppError::JsWriteClipboard(
                    EvalError::InvalidJs(e.to_string()),
                )
            }))
            .unwrap_or_else(|e| {
                tracing::error!(
                    "Clipboard channel error: {}",
                    e
                )
            });
        },
    );
    rx.recv().map_err(|e| {
        AppError::JsWriteClipboard(EvalError::InvalidJs(
            e.to_string(),
        ))
    })?
}

#[cfg(target_os = "android")]
pub async fn js_read_clipboard() -> Result<String, EvalError>
{
    let (tx, rx) = channel();
    dioxus::mobile::wry::prelude::dispatch(
        move |env: &mut jni::JNIEnv,
              activity: &jni::objects::JObject,
              _| {
            use jni::objects::JString;
            let res = (|| -> Result<String, jni::errors::Error> {
                let svc_name: JString = env.new_string("clipboard")?;
                let clipboard_svc = env.call_method(activity, "getSystemService", "(Ljava/lang/String;)Ljava/lang/Object;", &[(&svc_name).into()])?.l()?;
                let clipboard = env.new_global_ref(clipboard_svc)?;
                let clip = env.call_method(clipboard.as_obj(), "getPrimaryClip", "()Landroid/content/ClipData;", &[])?.l()?;
                let item = env.call_method(&clip, "getItemAt", "(I)Landroid/content/ClipData$Item;", &[jni::objects::JValue::Int(0)])?.l()?;
                let text_obj = env.call_method(&item, "getText", "()Ljava/lang/CharSequence;", &[])?.l()?;
                let s = JString::from(text_obj);
                Ok(env.get_string(&s).map(String::from)?)
            })();
            tx.send(res.map_err(|e| {
                EvalError::InvalidJs(e.to_string())
            }))
            .unwrap_or_else(|e| {
                tracing::error!(
                    "Clipboard channel error: {}",
                    e
                )
            });
        },
    );
    rx.recv()
        .map_err(|e| EvalError::InvalidJs(e.to_string()))?
}

#[cfg(not(target_os = "android"))]
pub async fn js_read_clipboard() -> Result<String, EvalError>
{
    js_fun(
        (),
        r#"function(arg){
        return await window.navigator.clipboard.readText();
        }"#,
    )
    .await
}

#[derive(
    Copy,
    Debug,
    Clone,
    PartialEq,
    Sequence,
    Display,
    Serialize,
    Deserialize,
)]
pub enum Theme {
    Light,
    Dark,
}

pub async fn js_set_theme(
    theme: &Theme,
) -> Result<(), EvalError> {
    js_fun(
        theme.to_string().to_lowercase(),
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

pub async fn js_check_camera() -> Result<(), EvalError> {
    js_fun(
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

pub async fn js_start_camera() -> Result<(), EvalError> {
    js_fun(
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

#[derive(Serialize, Deserialize, Clone)]
pub struct FrameData {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
}

pub async fn js_capture_frame(
) -> Result<FrameData, EvalError> {
    js_fun(
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

pub async fn js_stop_camera() -> Result<(), EvalError> {
    js_fun(
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

pub async fn js_sleep(
    millis: u64,
) -> Result<(), EvalError> {
    js_fun(
        millis,
        r#"function(arg){
        return new Promise(resolve => setTimeout(resolve, arg));
        }"#,
    )
    .await
}

async fn js_fun<
    A: Serialize + 'static,
    B: DeserializeOwned + 'static,
>(
    arg: A,
    fun: &'static str,
) -> Result<B, EvalError> {
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

    let mut eval = document::eval(code);

    match eval.send(arg) {
        Ok(()) => eval
            .recv::<Either<String, B>>()
            .await
            .and_then(|res| match res {
                Either::Right(rhs) => Ok(rhs),
                Either::Left(lhs) => {
                    Err(EvalError::InvalidJs(lhs))
                }
            }),
        Err(e) => Err(e),
    }
}
