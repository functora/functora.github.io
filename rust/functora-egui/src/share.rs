use crate::error::Error;
pub use crate::platform::ShareData;

pub async fn share(data: ShareData) -> Result<(), Error> {
    crate::platform::backend::share(data).await
}
