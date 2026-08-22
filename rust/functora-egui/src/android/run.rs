use android_activity::AndroidApp;

use crate::android::ime::store_app;

pub fn run<F>(app: AndroidApp, title: &str, creator: F)
where
    F: FnOnce(
            &eframe::CreationContext<'_>,
        ) -> Result<Box<dyn eframe::App>, Box<dyn std::error::Error + Send + Sync>>
        + 'static,
{
    let cloned = app.clone();
    store_app(app.clone());
    let options = eframe::NativeOptions {
        android_app: Some(app),
        viewport: egui::ViewportBuilder::default(),
        ..Default::default()
    };
    let _ = cloned;
    let result = eframe::run_native(title, options, Box::new(creator));
    if let Err(error) = result {
        eprintln!("eframe error: {error}");
    }
}
