use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ShareData {
    pub title: String,
    pub text: String,
    pub url: String,
}

/// Closes the app window gracefully.
///
/// The integration turns this into an ordinary window close: the app saves
/// its state, runs its exit hook and leaves the event loop. No process is
/// killed, so nothing is lost on the way out.
pub fn request_quit(ctx: &egui::Context) {
    ctx.send_viewport_cmd(egui::ViewportCommand::Close);
}

#[cfg(target_os = "android")]
#[cfg(feature = "platform")]
pub mod android;

pub mod android_back;

#[cfg(target_arch = "wasm32")]
#[cfg(feature = "platform")]
pub mod web;

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
#[cfg(feature = "platform")]
pub mod desktop;

#[cfg(all(
    target_arch = "wasm32",
    not(target_os = "android"),
    not(feature = "web")
))]
#[cfg(feature = "platform")]
pub(crate) mod stub;

/// The active platform backend. Callers (`clipboard`, `share`, `download`,
/// `camera`) go through this alias instead of repeating the target/feature
/// cfg matrix per operation.
#[cfg(target_os = "android")]
#[cfg(feature = "platform")]
pub(crate) use android as backend;

#[cfg(all(target_arch = "wasm32", feature = "web"))]
#[cfg(feature = "platform")]
pub(crate) use web as backend;

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
#[cfg(feature = "platform")]
pub(crate) use desktop as backend;

#[cfg(all(
    target_arch = "wasm32",
    not(target_os = "android"),
    not(feature = "web")
))]
#[cfg(feature = "platform")]
pub(crate) use stub as backend;
