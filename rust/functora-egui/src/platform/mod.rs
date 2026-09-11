#[cfg(target_os = "android")]
pub mod android;

pub mod android_back;

#[cfg(target_arch = "wasm32")]
pub mod web;

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
pub mod desktop;

#[cfg(all(
    target_arch = "wasm32",
    not(target_os = "android"),
    not(feature = "web")
))]
mod stub;

/// The active platform backend. Callers (`clipboard`, `share`, `download`,
/// `camera`) go through this alias instead of repeating the target/feature
/// cfg matrix per operation.
#[cfg(target_os = "android")]
pub(crate) use android as backend;

#[cfg(all(target_arch = "wasm32", feature = "web"))]
pub(crate) use web as backend;

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
pub(crate) use desktop as backend;

#[cfg(all(
    target_arch = "wasm32",
    not(target_os = "android"),
    not(feature = "web")
))]
pub(crate) use stub as backend;
