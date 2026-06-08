use crate::*;

pub fn write_clipboard(val: String, message: Signal<Option<String>>, lang: Language) {
    functora_dioxus::write_clipboard(val, message, lang);
}
