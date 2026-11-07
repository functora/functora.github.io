use std::sync::{Mutex, PoisonError};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PasteImage {
    pub name: String,
    pub data: Vec<u8>,
}

static SLOT: Mutex<Option<PasteImage>> = Mutex::new(None);

pub fn store(image: PasteImage) {
    *SLOT.lock().unwrap_or_else(PoisonError::into_inner) = Some(image);
}

#[must_use]
pub fn take() -> Option<PasteImage> {
    SLOT.lock().unwrap_or_else(PoisonError::into_inner).take()
}

#[must_use]
pub fn is_paste_image_intent(events: &[egui::Event]) -> bool {
    events.iter().any(|event| {
        matches!(
            event,
            egui::Event::Key {
                key: egui::Key::V,
                pressed: false,
                modifiers,
                ..
            } if modifiers.command
        )
    })
}
