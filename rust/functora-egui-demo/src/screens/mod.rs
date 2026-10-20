mod data;
mod display;
mod feedback;
mod forms;
mod inputs;
mod layout;
mod overlay;
mod overview;
mod platform;
mod responsive;

use functora_egui::Typography;

pub(crate) fn intro(ui: &mut egui::Ui, text: impl Into<String>) {
    _ = Typography::muted(text).show(ui);
    ui.add_space(12.0);
}

pub(crate) fn section(ui: &mut egui::Ui, heading: impl Into<String>) {
    _ = Typography::small(heading).show(ui);
    ui.add_space(4.0);
}
