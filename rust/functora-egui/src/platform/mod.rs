#[cfg(target_os = "android")]
pub mod android;

pub mod android_back;

#[cfg(target_arch = "wasm32")]
pub mod web;

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
pub mod desktop;
