use crate::error::Error;
use crate::ffi::eval;

pub async fn print_page() -> Result<(), Error> {
    eval(
        (),
        r"function(arg){
        window.print();
        return null;
        }",
    )
    .await
}
