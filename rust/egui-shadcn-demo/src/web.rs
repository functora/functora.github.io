use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

#[wasm_bindgen(start)]
pub fn start() -> Result<(), JsValue> {
    _ = eframe::WebLogger::init(log::LevelFilter::Info).ok();
    let window = web_sys::window().ok_or_else(|| JsValue::from_str("no window"))?;
    let document = window
        .document()
        .ok_or_else(|| JsValue::from_str("no document"))?;
    let canvas = document
        .get_element_by_id("the_canvas_id")
        .and_then(|element| element.dyn_into::<web_sys::HtmlCanvasElement>().ok())
        .ok_or_else(|| JsValue::from_str("canvas not found"))?;
    let web_options = eframe::WebOptions::default();
    wasm_bindgen_futures::spawn_local(async move {
        let result = eframe::WebRunner::new()
            .start(
                canvas,
                web_options,
                Box::new(|cc| Ok(Box::new(crate::ShowcaseApp::new(cc)))),
            )
            .await;
        if let Err(error) = result {
            web_sys::console::error_1(&error);
        }
    });
    Ok(())
}
