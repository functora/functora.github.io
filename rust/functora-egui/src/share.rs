use crate::error::Error;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ShareData {
    pub title: String,
    pub text: String,
    pub url: String,
}

pub async fn share(data: ShareData) -> Result<(), Error> {
    let payload = crate::platform::backend::ShareData {
        title: data.title,
        text: data.text,
        url: data.url,
    };
    crate::platform::backend::share(payload).await
}
