use egui::{Event, Key, Modifiers};
#[cfg(all(
    feature = "clipboard",
    feature = "images",
    not(any(target_arch = "wasm32", target_os = "android"))
))]
use functora_egui::clipboard::rgba_to_png;
use functora_egui::paste_image::{PasteImage, is_paste_image_intent, store, take};

fn v_release(command: bool) -> Event {
    Event::Key {
        key: Key::V,
        physical_key: None,
        pressed: false,
        repeat: false,
        modifiers: Modifiers {
            command,
            ..Default::default()
        },
    }
}

#[test]
fn v_release_with_command_is_paste_intent() {
    assert!(is_paste_image_intent(&[v_release(true)]));
}

#[test]
fn v_press_with_command_is_not_paste_intent() {
    let events = [Event::Key {
        key: Key::V,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: Modifiers {
            command: true,
            ..Default::default()
        },
    }];
    assert!(!is_paste_image_intent(&events));
}

#[test]
fn v_release_without_command_is_not_paste_intent() {
    assert!(!is_paste_image_intent(&[v_release(false)]));
}

#[test]
fn other_key_release_with_command_is_not_paste_intent() {
    let events = [Event::Key {
        key: Key::C,
        physical_key: None,
        pressed: false,
        repeat: false,
        modifiers: Modifiers {
            command: true,
            ..Default::default()
        },
    }];
    assert!(!is_paste_image_intent(&events));
}

#[test]
fn empty_events_are_not_paste_intent() {
    assert!(!is_paste_image_intent(&[]));
}

#[test]
fn slot_roundtrip() {
    let image = PasteImage {
        name: "pasted-1.png".to_owned(),
        data: vec![1, 2, 3],
    };
    store(image.clone());
    assert_eq!(take(), Some(image));
    assert_eq!(take(), None);
}

#[cfg(all(
    feature = "clipboard",
    feature = "images",
    not(any(target_arch = "wasm32", target_os = "android"))
))]
#[test]
fn rgba_to_png_encodes_decodable_png() {
    let rgba = [
        255, 0, 0, 255, 0, 255, 0, 255, 0, 0, 255, 255, 255, 255, 0, 255,
    ];
    let png = rgba_to_png(2, 2, &rgba).unwrap_or_else(|err| panic!("encode must succeed: {err}"));
    assert_eq!(&png[..8], &[137, 80, 78, 71, 13, 10, 26, 10]);
    let decoded =
        image::load_from_memory(&png).unwrap_or_else(|err| panic!("png must decode: {err}"));
    assert_eq!((decoded.width(), decoded.height()), (2, 2));
}

#[cfg(all(
    feature = "clipboard",
    feature = "images",
    not(any(target_arch = "wasm32", target_os = "android"))
))]
#[test]
fn rgba_to_png_rejects_mismatched_buffer() {
    let err = rgba_to_png(2, 2, &[0u8; 8])
        .err()
        .unwrap_or_else(|| panic!("short buffer must fail"));
    assert!(!err.to_string().is_empty());
}

#[cfg(all(
    feature = "clipboard",
    feature = "images",
    not(any(target_arch = "wasm32", target_os = "android"))
))]
#[test]
fn read_image_treats_unavailable_clipboard_as_no_image() {
    let result = pollster::block_on(functora_egui::clipboard::read_image());
    let Ok(image) = result else {
        panic!("clipboard unavailability must not surface as an error");
    };
    if let Some(pasted) = image {
        assert_eq!(&pasted.data[..8], &[137, 80, 78, 71, 13, 10, 26, 10]);
    }
}
