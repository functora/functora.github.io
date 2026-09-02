//! Platform frame pumps shared by `CameraView` and `QrScanner`.

use super::camera_view_state::{FeedInner, FrameHandler};
#[cfg(any(all(target_arch = "wasm32", feature = "web"), target_os = "android"))]
use super::camera_view_state::{camera_epoch, map_camera_error};
#[cfg(any(all(target_arch = "wasm32", feature = "web"), target_os = "android"))]
use crate::camera::FrameData;
use crate::error::Error;
use crate::in_flight::InFlightGuard;
#[cfg(any(all(target_arch = "wasm32", feature = "web"), target_os = "android"))]
use std::ops::ControlFlow;
use std::sync::{Arc, Mutex};

#[cfg(any(all(target_arch = "wasm32", feature = "web"), target_os = "android"))]
fn set_error(feed: &Arc<Mutex<FeedInner>>, epoch: u64, err: &Error) {
    if camera_epoch() == epoch
        && let Ok(mut guard) = feed.lock()
        && guard.epoch == epoch
    {
        guard.error = Some(Arc::new(map_camera_error(err)));
        guard.running = false;
    }
}

#[cfg(any(all(target_arch = "wasm32", feature = "web"), target_os = "android"))]
fn store_frame(feed: &Arc<Mutex<FeedInner>>, epoch: u64, frame: &FrameData) {
    if let Some(rgba) = frame.preview_rgba.clone()
        && let Ok(mut guard) = feed.lock()
        && guard.epoch == epoch
    {
        guard.latest_rgba = Some((rgba, frame.width, frame.height));
    }
}

#[cfg(any(all(target_arch = "wasm32", feature = "web"), target_os = "android"))]
fn session_alive(feed: &Arc<Mutex<FeedInner>>, epoch: u64) -> bool {
    camera_epoch() == epoch
        && feed
            .lock()
            .ok()
            .is_some_and(|guard| guard.running && guard.epoch == epoch)
}

#[cfg(any(all(target_arch = "wasm32", feature = "web"), target_os = "android"))]
fn finish(_feed: &Arc<Mutex<FeedInner>>, ctx: &egui::Context, epoch: u64) {
    if camera_epoch() == epoch {
        drop(crate::camera::stop_camera());
    }
    crate::camera::stop_capture_worker();
    ctx.request_repaint();
}

#[cfg(all(target_arch = "wasm32", feature = "web"))]
pub(crate) fn spawn_web_pump(
    ctx: egui::Context,
    feed: Arc<Mutex<FeedInner>>,
    epoch: u64,
    interval_ms: u64,
    mut handler: FrameHandler,
    guard: InFlightGuard,
) {
    wasm_bindgen_futures::spawn_local(async move {
        let interval = interval_ms.max(16);
        if let Err(e) = crate::camera::check_camera().await {
            set_error(&feed, epoch, &e);
            ctx.request_repaint();
            drop(guard);
            return;
        }
        if !session_alive(&feed, epoch) {
            drop(guard);
            return;
        }
        if let Err(e) = crate::camera::start_camera().await {
            set_error(&feed, epoch, &e);
            ctx.request_repaint();
            drop(guard);
            return;
        }
        crate::camera::begin_capture_session();
        loop {
            if !session_alive(&feed, epoch) {
                break;
            }
            match crate::camera::capture_frame().await {
                Ok(frame) => {
                    store_frame(&feed, epoch, &frame);
                    if handler(&ctx, &frame) == ControlFlow::Break(()) {
                        break;
                    }
                    ctx.request_repaint();
                }
                Err(e) => {
                    set_error(&feed, epoch, &e);
                    ctx.request_repaint();
                    break;
                }
            }
            drop(crate::camera::sleep(interval).await);
        }
        finish(&feed, &ctx, epoch);
        drop(guard);
    });
}

#[cfg(target_os = "android")]
pub(crate) fn spawn_android_pump(
    ctx: egui::Context,
    feed: Arc<Mutex<FeedInner>>,
    epoch: u64,
    interval_ms: u64,
    mut handler: FrameHandler,
    guard: InFlightGuard,
) {
    let fail_feed = Arc::clone(&feed);
    let fail_ctx = ctx.clone();
    let spawned = std::thread::Builder::new()
        .name("functora-camera-pump".into())
        .spawn(move || {
            let interval = std::time::Duration::from_millis(interval_ms.max(16));
            if let Err(e) = crate::platform::android::check_camera_blocking() {
                set_error(&feed, epoch, &e);
                ctx.request_repaint();
                drop(guard);
                return;
            }
            if !session_alive(&feed, epoch) {
                drop(guard);
                return;
            }
            if let Err(e) = crate::platform::android::start_camera_blocking() {
                set_error(&feed, epoch, &e);
                ctx.request_repaint();
                drop(guard);
                return;
            }
            crate::camera::begin_capture_session();
            loop {
                if !session_alive(&feed, epoch) {
                    break;
                }
                match crate::platform::android::capture_frame_blocking() {
                    Ok(frame) => {
                        store_frame(&feed, epoch, &frame);
                        if handler(&ctx, &frame) == ControlFlow::Break(()) {
                            break;
                        }
                        ctx.request_repaint();
                    }
                    Err(e) => {
                        set_error(&feed, epoch, &e);
                        ctx.request_repaint();
                        break;
                    }
                }
                std::thread::sleep(interval);
            }
            finish(&feed, &ctx, epoch);
            drop(guard);
        });
    if let Err(e) = spawned {
        tracing::error!("camera pump thread spawn failed: {e}");
        set_error(
            &fail_feed,
            epoch,
            &Error::CameraNotAvailable(format!("Camera thread failed to start: {e}")),
        );
        fail_ctx.request_repaint();
    }
}

/// Spawns the pump for the current target.
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn spawn_pump(
    ctx: egui::Context,
    feed: Arc<Mutex<FeedInner>>,
    epoch: u64,
    interval_ms: u64,
    handler: FrameHandler,
    guard: InFlightGuard,
) {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    {
        spawn_web_pump(ctx, feed, epoch, interval_ms, handler, guard);
    }
    #[cfg(target_os = "android")]
    {
        spawn_android_pump(ctx, feed, epoch, interval_ms, handler, guard);
    }
    #[cfg(not(any(all(target_arch = "wasm32", feature = "web"), target_os = "android")))]
    {
        let _ = (epoch, interval_ms);
        if let Ok(mut feed_guard) = feed.lock() {
            feed_guard.error = Some(Arc::new(Error::CameraNotAvailable(
                "Live camera not available on this platform".into(),
            )));
            feed_guard.running = false;
        }
        drop((handler, guard));
        ctx.request_repaint();
    }
}
