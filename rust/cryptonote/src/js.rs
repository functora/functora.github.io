use crate::prelude::*;
use either::*;

#[derive(
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

pub async fn js_data_theme(
    theme: Theme,
    after: impl FnOnce(Result<(), EvalError>),
) {
    js_fun(
        theme.to_string().to_lowercase(),
        after,
        r#"function(arg){
               window
                 .document
                 .documentElement
                 .setAttribute("data-theme", arg);
             }"#,
    )
    .await
}

pub async fn js_clipboard_write(
    msg: String,
    after: impl FnOnce(Result<(), EvalError>),
) {
    js_fun(
        msg,
        after,
        r#"function(arg){
               await window
                       .navigator
                       .clipboard
                       .writeText(arg);
             }"#,
    )
    .await
}

async fn js_fun<
    A: Serialize + 'static,
    B: DeserializeOwned + 'static,
    C,
>(
    arg: A,
    out: impl FnOnce(Result<B, EvalError>) -> C,
    fun: &'static str,
) {
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

    let res = match eval.send(arg) {
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
    };

    out(res);
}
