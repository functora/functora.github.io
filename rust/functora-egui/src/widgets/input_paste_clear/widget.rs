#[must_use]
pub struct InputPasteClear<'a> {
    pub(crate) text: &'a mut String,
    pub(crate) placeholder: String,
    pub(crate) default_value: String,
    pub(crate) password: bool,
    pub(crate) paste_icon: crate::icons::lucide_icon::LucideIcon,
    pub(crate) clear_icon: crate::icons::lucide_icon::LucideIcon,
    pub(crate) copy: bool,
    pub(crate) copy_icon: crate::icons::lucide_icon::LucideIcon,
}

impl<'a> InputPasteClear<'a> {
    pub fn new(text: &'a mut String) -> Self {
        Self {
            text,
            placeholder: String::new(),
            default_value: String::new(),
            password: false,
            paste_icon: crate::icons::lucide_icon::LucideIcon::ClipboardPaste,
            clear_icon: crate::icons::lucide_icon::LucideIcon::X,
            copy: false,
            copy_icon: crate::icons::lucide_icon::LucideIcon::Copy,
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

    pub fn password(mut self) -> Self {
        self.password = true;
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

    pub fn copy(mut self) -> Self {
        self.copy = true;
        self
    }

    pub fn with_copy(mut self, show: bool) -> Self {
        self.copy = show;
        self
    }

    pub fn copy_icon(mut self, icon: crate::icons::lucide_icon::LucideIcon) -> Self {
        self.copy_icon = icon;
        self.copy = true;
        self
    }

    pub fn show(self, ui: &mut egui::Ui) -> super::input_paste_clear_show::PasteClearResponse {
        super::input_paste_clear_show::show_input_paste_clear(ui, self)
    }
}
