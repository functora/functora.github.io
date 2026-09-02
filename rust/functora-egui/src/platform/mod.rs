#[cfg(target_os = "android")]
pub mod android;

#[cfg(target_arch = "wasm32")]
pub mod web;

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
pub mod desktop;
