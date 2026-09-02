use crate::error::Error;

pub async fn read() -> Result<String, Error> {
    #[cfg(target_os = "android")]
    {
        return crate::platform::android::clipboard_read().await;
    }
    #[cfg(all(target_arch = "wasm32", not(target_os = "android")))]
    {
        #[cfg(feature = "web")]
        {
            return crate::platform::web::clipboard_read().await;
        }
        #[cfg(not(feature = "web"))]
        {
            return Err(Error::JS(
                "Clipboard not available (web feature disabled)".into(),
            ));
        }
    }
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        return crate::platform::desktop::clipboard_read().await;
    }
    #[allow(unreachable_code)]
    Err(Error::JS("Clipboard not available".into()))
}

pub async fn write(text: String) -> Result<(), Error> {
    #[cfg(target_os = "android")]
    {
        return crate::platform::android::clipboard_write(text).await;
    }
    #[cfg(all(target_arch = "wasm32", not(target_os = "android")))]
    {
        #[cfg(feature = "web")]
        {
            return crate::platform::web::clipboard_write(text).await;
        }
        #[cfg(not(feature = "web"))]
        {
            let _ = text;
            return Err(Error::JS(
                "Clipboard not available (web feature disabled)".into(),
            ));
        }
    }
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        return crate::platform::desktop::clipboard_write(text).await;
    }
    #[allow(unreachable_code)]
    {
        let _ = text;
        Err(Error::JS("Clipboard not available".into()))
    }
}
