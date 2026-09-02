pub use functora_core::error::{Error, IoError, JsonError, WorkerStopped, ZipErr};

#[cfg(target_os = "android")]
pub use functora_core::error::JniError;
