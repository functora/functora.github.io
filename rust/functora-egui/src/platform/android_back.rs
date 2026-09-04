#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackOutcome {
    ConsumedNav,
    ConsumedNoop,
}

static CTX: std::sync::Mutex<Option<egui::Context>> = std::sync::Mutex::new(None);

pub fn store_context(ctx: &egui::Context) {
    let _ = CTX
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .replace(ctx.clone());
}

pub fn wake_via_repaint() {
    if let Some(ctx) = CTX
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .as_ref()
    {
        ctx.request_repaint();
    }
    #[cfg(target_os = "android")]
    crate::platform::android::wake_event_loop();
}

#[cfg(target_os = "android")]
#[unsafe(no_mangle)]
#[allow(non_snake_case)]
pub extern "system" fn Java_com_functora_Waker_wake(
    _env: jni::JNIEnv<'_>,
    _class: jni::objects::JClass<'_>,
) {
    wake_via_repaint();
}

fn system_back_pressed(ctx: &egui::Context) -> bool {
    let key_back = ctx.input(|i| i.key_pressed(egui::Key::BrowserBack));
    if key_back {
        ctx.input_mut(|i| {
            let _ = i.consume_key(egui::Modifiers::NONE, egui::Key::BrowserBack);
        });
        return true;
    }
    #[cfg(target_os = "android")]
    {
        if crate::platform::android::poll_back_pressed() {
            return true;
        }
    }
    false
}

#[must_use]
pub fn is_back_pressed(ctx: &egui::Context) -> bool {
    ctx.input(|i| i.key_pressed(egui::Key::BrowserBack)) || {
        #[cfg(target_os = "android")]
        {
            crate::platform::android::peek_back_pressed()
        }
        #[cfg(not(target_os = "android"))]
        {
            false
        }
    }
}

#[must_use]
pub fn consume_back_pressed(ctx: &egui::Context) -> bool {
    system_back_pressed(ctx)
}

pub fn handle_system_back(
    ctx: &egui::Context,
    can_go_back: bool,
    on_back: impl FnOnce(),
) -> Option<BackOutcome> {
    store_context(ctx);
    if !system_back_pressed(ctx) {
        return None;
    }
    let wants_keyboard = ctx.egui_wants_keyboard_input() || ctx.memory(|m| m.focused().is_some());
    if wants_keyboard {
        ctx.send_viewport_cmd(egui::ViewportCommand::CancelClose);
        ctx.request_repaint();
        return Some(BackOutcome::ConsumedNoop);
    }
    if can_go_back {
        on_back();
        ctx.send_viewport_cmd(egui::ViewportCommand::CancelClose);
        ctx.request_repaint();
        Some(BackOutcome::ConsumedNav)
    } else {
        ctx.send_viewport_cmd(egui::ViewportCommand::CancelClose);
        ctx.request_repaint();
        Some(BackOutcome::ConsumedNoop)
    }
}
