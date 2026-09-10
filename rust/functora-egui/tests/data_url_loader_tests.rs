#![cfg(feature = "markdown")]
#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Data URL bytes must load synchronously (wasm has no threads).
//! Upstream `egui_commonmark_backend::DataUrlLoader` spawns a thread and
//! panics on wasm (`operation not supported`). Our loader must return
//! `Ready` on the first call.

use egui::load::{BytesPoll, LoadError};
use functora_egui::{CommonMarkCache, CommonMarkViewer};

const DATA_URL: &str = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAFklEQVR4nGO4o6ZGEmIY1TCqYfhqAAATqigQ9JeO5gAAAABJRU5ErkJggg==";

fn run_viewer_once(ctx: &egui::Context, cache: &mut CommonMarkCache) {
    let raw = egui::RawInput {
        screen_rect: Some(egui::Rect::from_min_size(
            egui::Pos2::ZERO,
            egui::Vec2::new(1280.0, 800.0),
        )),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| {
            let _ = CommonMarkViewer::new().show(inner, cache, "![]()");
        });
    });
    out.textures_delta.clear();
}

#[test]
fn data_url_bytes_load_synchronously() {
    let ctx = egui::Context::default();
    functora_egui::setup_image_loaders(&ctx);
    let mut cache = CommonMarkCache::default();
    run_viewer_once(&ctx, &mut cache);
    match ctx.try_load_bytes(DATA_URL) {
        Ok(BytesPoll::Ready { bytes, mime, .. }) => {
            assert!(!bytes.is_empty(), "decoded bytes must not be empty");
            assert!(
                mime.as_deref().is_some_and(|m| m.contains("image/png")),
                "mime must be image/png, got {mime:?}",
            );
        }
        Ok(BytesPoll::Pending { .. }) => {
            panic!("data URL must load synchronously without Pending/thread");
        }
        Err(LoadError::NotSupported | LoadError::NoMatchingBytesLoader) => {
            panic!("no bytes loader handles data: URLs");
        }
        Err(e) => panic!("data URL load failed: {e:?}"),
    }
}

#[test]
fn unsupported_scheme_is_not_supported() {
    let ctx = egui::Context::default();
    functora_egui::setup_image_loaders(&ctx);
    let mut cache = CommonMarkCache::default();
    run_viewer_once(&ctx, &mut cache);
    match ctx.try_load_bytes("notaurl") {
        Err(LoadError::NotSupported | LoadError::NoMatchingBytesLoader) => {}
        Ok(BytesPoll::Ready { .. } | BytesPoll::Pending { .. }) => {
            panic!("plain string must not decode as data URL");
        }
        Err(e) => panic!("unexpected error for plain string: {e:?}"),
    }
}
