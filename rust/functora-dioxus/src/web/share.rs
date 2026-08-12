use crate::error::Error;
use crate::ffi::{ShareData, eval};

pub async fn social_share(data: ShareData) -> Result<(), Error> {
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
