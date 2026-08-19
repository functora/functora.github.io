pub mod app;
pub mod archive;
pub mod crypto;
pub mod deep_link;
pub mod encoding;
pub mod error;
pub mod i18n;
pub mod messages;
pub mod platform;
pub mod progress;
pub mod screens;
pub mod state;
pub mod task;
pub mod theme;

pub use app::*;
pub use archive::*;
pub use crypto::*;
pub use deep_link::*;
pub use encoding::*;
pub use error::*;
pub use i18n::*;
pub use messages::*;
pub use platform::*;
pub use progress::*;
pub use screens::*;
pub use state::*;
pub use theme::*;

#[cfg(target_arch = "wasm32")]
pub mod web;

#[cfg(target_os = "android")]
mod android {
    use jni::objects::{JObject, JString};
    use jni::signature::ReturnType;
    use jni::JavaVM;

    use crate::app::CryptonoteApp;
    use crate::deep_link::store_url;

    fn intent_url(app: &winit::platform::android::activity::AndroidApp) -> Option<String> {
        // SAFETY: the pointers come from android_activity's own JVM bindings
        let vm = unsafe { JavaVM::from_raw(app.vm_as_ptr().cast()) }.ok()?;
        let mut env = vm.attach_current_thread().ok()?;
        // SAFETY: the activity reference is owned by android_activity, so the
        // wrapper is leaked below and never deletes the reference
        let activity: JObject = unsafe { JObject::from_raw(app.activity_as_ptr().cast()) };
        let activity_cls = env.get_object_class(&activity).ok()?;
        let get_intent = env
            .get_method_id(&activity_cls, "getIntent", "()Landroid/content/Intent;")
            .ok()?;
        // SAFETY: the method id and object are valid JNI handles
        let intent =
            unsafe { env.call_method_unchecked(&activity, get_intent, ReturnType::Object, &[]) }
                .ok()?
                .l()
                .ok()?;
        let intent_cls = env.get_object_class(&intent).ok()?;
        let get_data = env
            .get_method_id(&intent_cls, "getDataString", "()Ljava/lang/String;")
            .ok()?;
        // SAFETY: the method id and object are valid JNI handles
        let data = unsafe { env.call_method_unchecked(&intent, get_data, ReturnType::Object, &[]) }
            .ok()?
            .l()
            .ok()?;
        let data = JString::from(data);
        let url = env.get_string(&data).ok()?;
        std::mem::forget(activity);
        Some(String::from(url))
    }

    #[export_name = "android_main"]
    pub fn android_main(app: winit::platform::android::activity::AndroidApp) {
        if let Some(url) = intent_url(&app) {
            store_url(url);
        }
        let options = eframe::NativeOptions {
            android_app: Some(app),
            viewport: egui::ViewportBuilder::default(),
            ..Default::default()
        };
        let result = eframe::run_native(
            "Cryptonote",
            options,
            Box::new(|cc| Ok(Box::new(CryptonoteApp::new(cc)))),
        );
        if let Err(error) = result {
            eprintln!("eframe error: {error}");
        }
    }
}
