use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

pub fn launch<F>(canvas_id: &str, app_creator: F) -> Result<(), JsValue>
where
    F: FnOnce(
            &eframe::CreationContext<'_>,
        ) -> Result<Box<dyn eframe::App>, Box<dyn std::error::Error + Send + Sync>>
        + 'static,
{
    _ = eframe::WebLogger::init(log::LevelFilter::Info).ok();
    let window = web_sys::window().ok_or_else(|| JsValue::from_str("no window"))?;
    let document = window
        .document()
        .ok_or_else(|| JsValue::from_str("no document"))?;
    let canvas = document
        .get_element_by_id(canvas_id)
        .and_then(|element| element.dyn_into::<web_sys::HtmlCanvasElement>().ok())
        .ok_or_else(|| JsValue::from_str("canvas not found"))?;
    let web_options = eframe::WebOptions::default();
    wasm_bindgen_futures::spawn_local(async move {
        let canvas_clone = canvas.clone();
        let result = eframe::WebRunner::new()
            .start(canvas, web_options, Box::new(app_creator))
            .await;
        if let Err(error) = result {
            web_sys::console::error_1(&error);
        } else {
            let _ = canvas_clone.focus();
        }
    });
    Ok(())
}
