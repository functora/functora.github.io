use crate::error::Error;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ShareData {
    pub title: String,
    pub text: String,
    pub url: String,
}

pub async fn share(data: ShareData) -> Result<(), Error> {
    #[cfg(target_os = "android")]
    {
        let payload = crate::platform::android::ShareData {
            title: data.title,
            text: data.text,
            url: data.url,
        };
        return crate::platform::android::share(payload).await;
    }
    #[cfg(all(target_arch = "wasm32", not(target_os = "android")))]
    {
        #[cfg(feature = "web")]
        {
            let payload = crate::platform::web::ShareData {
                title: data.title,
                text: data.text,
                url: data.url,
            };
            return crate::platform::web::share(payload).await;
        }
        #[cfg(not(feature = "web"))]
        {
            let _ = data;
            return Err(Error::JS(
                "Share not available (web feature disabled)".into(),
            ));
        }
    }
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        let payload = crate::platform::desktop::ShareData {
            title: data.title,
            text: data.text,
            url: data.url,
        };
        return crate::platform::desktop::share(payload).await;
    }
    #[allow(unreachable_code)]
    {
        let _ = data;
        Err(Error::JS("Share not available".into()))
    }
}
