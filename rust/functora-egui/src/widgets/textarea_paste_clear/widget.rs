#[must_use]
pub struct TextareaPasteClear<'a> {
    pub(crate) text: &'a mut String,
    pub(crate) placeholder: String,
    pub(crate) default_value: String,
    pub(crate) min_height: f32,
    pub(crate) paste_icon: crate::icons::lucide_icon::LucideIcon,
    pub(crate) clear_icon: crate::icons::lucide_icon::LucideIcon,
}

impl<'a> TextareaPasteClear<'a> {
    pub fn new(text: &'a mut String) -> Self {
        Self {
            text,
            placeholder: String::new(),
            default_value: String::new(),
            min_height: 64.0,
            paste_icon: crate::icons::lucide_icon::LucideIcon::ClipboardPaste,
            clear_icon: crate::icons::lucide_icon::LucideIcon::X,
        }
    }

    pub fn placeholder(mut self, placeholder: impl Into<String>) -> Self {
        self.placeholder = placeholder.into();
        self
    }

    pub fn default_value(mut self, value: impl Into<String>) -> Self {
        self.default_value = value.into();
        self
    }

    pub fn min_height(mut self, height: f32) -> Self {
        self.min_height = height;
        self
    }

    pub fn paste_icon(mut self, icon: crate::icons::lucide_icon::LucideIcon) -> Self {
        self.paste_icon = icon;
        self
    }

    pub fn clear_icon(mut self, icon: crate::icons::lucide_icon::LucideIcon) -> Self {
        self.clear_icon = icon;
        self
    }

    pub fn show(self, ui: &mut egui::Ui) -> super::textarea_paste_clear_show::PasteClearResponse {
        super::textarea_paste_clear_show::show_textarea_paste_clear(ui, self)
    }
}
