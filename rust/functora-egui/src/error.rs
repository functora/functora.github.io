pub use functora_core::error::{Error, IoError, JsonError, WorkerStopped};

#[cfg(target_os = "android")]
pub use functora_core::error::JniError;

#[cfg(feature = "zip")]
pub use functora_core::error::ZipErr;
