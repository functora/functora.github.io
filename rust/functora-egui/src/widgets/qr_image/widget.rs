#[must_use]
pub struct QrImage<'a> {
    pub(crate) content: &'a str,
}

impl<'a> QrImage<'a> {
    pub fn new(content: &'a str) -> Self {
        Self { content }
    }

    pub fn show(self, ui: &mut egui::Ui) -> egui::Response {
        ui.add(self)
    }
}
