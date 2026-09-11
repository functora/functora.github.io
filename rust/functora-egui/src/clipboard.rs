use crate::error::Error;

pub async fn read() -> Result<String, Error> {
    crate::platform::backend::clipboard_read().await
}

pub async fn write(text: String) -> Result<(), Error> {
    crate::platform::backend::clipboard_write(text).await
}
