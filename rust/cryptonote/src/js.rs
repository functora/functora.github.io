use crate::prelude::*;
use either::*;

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

pub async fn js_write_clipboard(
    msg: String,
) -> Result<(), EvalError> {
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
        facingMode: "environment",
        width: { ideal: 1280 },
        height: { ideal: 720 }
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

pub async fn js_capture_frame() -> Result<Vec<u8>, EvalError>
{
    js_vec_fun(
        (),
        r#"function(arg){
        const video = document.getElementById("qr-video");
        const canvas = document.getElementById("qr-canvas");
        if (!video || !canvas) {
        throw new Error("Video or canvas not found");
        }
        const ctx = canvas.getContext("2d");
        canvas.width = video.videoWidth || 1280;
        canvas.height = video.videoHeight || 720;
        ctx.drawImage(video, 0, 0);
        const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
        return new Uint8Array(imageData.data.buffer);
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

pub async fn js_video_dimensions(
) -> Result<(u32, u32), EvalError> {
    js_fun(
        (),
        r#"function(arg){
        const video = document.getElementById("qr-video");
        return video
        ? [video.videoWidth || 1280, video.videoHeight || 720]
        : [1280, 720];
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

async fn js_vec_fun<A: Serialize + 'static>(
    arg: A,
    fun: &'static str,
) -> Result<Vec<u8>, EvalError> {
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
            .recv::<Either<String, Vec<u8>>>()
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
