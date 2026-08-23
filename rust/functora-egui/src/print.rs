use crate::error::Error;

pub async fn print_page() -> Result<(), Error> {
    #[cfg(target_os = "android")]
    {
        return crate::platform::android::print_page().await;
    }
    #[cfg(all(target_arch = "wasm32", not(target_os = "android")))]
    {
        #[cfg(feature = "web")]
        {
            return crate::platform::web::print_page().await;
        }
        #[cfg(not(feature = "web"))]
        {
            return Err(Error::JS(
                "Print not available (web feature disabled)".into(),
            ));
        }
    }
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        return crate::platform::desktop::print_page().await;
    }
    #[allow(unreachable_code)]
    Err(Error::JS("Print not available".into()))
}
