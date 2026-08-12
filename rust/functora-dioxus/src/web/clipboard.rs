use crate::error::Error;
use crate::ffi::eval;

pub async fn read_clipboard() -> Result<String, Error> {
    eval(
        (),
        r"function(arg){
        return await window.navigator.clipboard.readText();
        }",
    )
    .await
}

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
