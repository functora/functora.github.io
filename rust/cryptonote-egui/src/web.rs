use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn start() -> Result<(), JsValue> {
    functora_egui::web::runner::launch("the_canvas_id", |cc| {
        Ok(Box::new(crate::CryptonoteApp::new(cc)) as Box<dyn eframe::App>)
    })
}
