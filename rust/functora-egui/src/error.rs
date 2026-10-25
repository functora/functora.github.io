pub use functora_core::error::{
    Capability, ClipboardError, Error, IoError, JsonError, Missing, WorkerStopped,
};

#[cfg(target_arch = "wasm32")]
pub use functora_core::error::JsError;

#[cfg(target_os = "android")]
pub use functora_core::error::JniError;

#[cfg(feature = "zip")]
pub use functora_core::error::ZipErr;
