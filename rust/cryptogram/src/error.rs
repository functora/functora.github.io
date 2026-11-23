use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("invalid invitation: {0}")]
    InvalidInvitation(String),
    #[error("invalid relay input: {0}")]
    InvalidRelayInput(String),
    #[error("invalid frame: {0}")]
    InvalidFrame(String),
    #[error("base64 decoding failed: {0}")]
    Base64(#[from] base64::DecodeError),
    #[error("JSON processing failed: {0}")]
    Json(#[from] serde_json::Error),
    #[error("outer encryption failed: {0}")]
    OuterCrypto(chacha20poly1305::aead::Error),
    #[error("key derivation failed: {0}")]
    KeyDerivation(hkdf::InvalidLength),
    #[error("secure randomness failed: {0}")]
    Random(getrandom::Error),
    #[error("MLS operation failed: {0}")]
    Mls(String),
    #[error("local vault operation failed: {0}")]
    Vault(String),
    #[error("local storage operation failed: {0}")]
    Storage(functora_egui::error::Error),
    #[error("relay operation failed: {0}")]
    Relay(String),
    #[error("unsupported operation on this platform: {0}")]
    Unsupported(&'static str),
    #[cfg(target_arch = "wasm32")]
    #[error("WASM serialization failed: {0}")]
    Wasm(#[from] serde_wasm_bindgen::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<functora_egui::error::Error> for Error {
    fn from(error: functora_egui::error::Error) -> Self {
        Self::Storage(error)
    }
}
