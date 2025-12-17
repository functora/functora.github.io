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
               window
                 .document
                 .documentElement
                 .setAttribute("data-theme", arg);
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
