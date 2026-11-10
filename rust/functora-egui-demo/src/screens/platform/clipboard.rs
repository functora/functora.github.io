use functora_egui::snippet;
use functora_egui::{
    Button, ButtonVariant, Flex, Input, Label, Textarea, ToastVariant, Typography, spawn_async,
};

impl crate::state::ShowcaseApp {
    pub fn demo_clipboard(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Clipboard read/write via arboard (desktop), navigator.clipboard (web), ClipboardManager (Android).",
        )
        .show(ui);
        ui.add_space(12.0);
        let w = ui.available_width();
        _ = Label::new("Write to clipboard").show(ui);
        ui.add_space(8.0);
        _ = Input::new(&mut self.platform.clipboard_write)
            .placeholder("text to copy")
            .show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f2| {
            let writing = self.platform.clipboard_write_rx.is_some();
            if f2
                .add(
                    Button::new(if writing { "Copying..." } else { "Copy" })
                        .icon(functora_egui::LucideIcon::Copy)
                        .enabled(!writing),
                )
                .inner
                .clicked()
            {
                let text = self.platform.clipboard_write.clone();
                self.platform.clipboard_write_rx = Some(spawn_async(async move {
                    functora_egui::clipboard::write(text)
                        .await
                        .map_err(|e| e.to_string())
                }));
            }
            let reading = self.platform.clipboard_rx.is_some();
            if f2
                .add(
                    Button::new(if reading { "Reading..." } else { "Paste" })
                        .variant(ButtonVariant::Outline)
                        .icon(functora_egui::LucideIcon::ClipboardPaste)
                        .enabled(!reading),
                )
                .inner
                .clicked()
            {
                self.platform.clipboard_rx = Some(spawn_async(async move {
                    functora_egui::clipboard::read()
                        .await
                        .map_err(|e| e.to_string())
                }));
            }
        });
        ui.add_space(8.0);
        _ = Label::new("Last pasted").show(ui);
        ui.add_space(8.0);
        _ = Textarea::new(&mut self.platform.clipboard_read)
            .placeholder("pasted text appears here")
            .desired_width(w)
            .show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "Image paste slot (functora_egui::paste_image)");
        let now = ui.ctx().input(|i| i.time);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f2| {
            if f2
                .add(Button::new("Store image").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                functora_egui::paste_image::store(functora_egui::paste_image::PasteImage {
                    name: "clipboard.png".to_owned(),
                    data: vec![0x89, b'P', b'N', b'G'],
                });
            }
            if f2
                .add(Button::new("Take image").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                let taken = functora_egui::paste_image::take();
                self.toast.add(
                    format!(
                        "paste image slot: {:?}",
                        taken
                            .as_ref()
                            .map(|image| (image.name.as_str(), image.data.len()))
                    ),
                    ToastVariant::Default,
                    now,
                );
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "is_paste_image_intent(&[]): {}",
            functora_egui::paste_image::is_paste_image_intent(&[])
        ))
        .show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "Clipboard images");
        #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
        {
            let png = functora_egui::clipboard::rgba_to_png(1, 1, &[0xFF, 0x00, 0x00, 0xFF]);
            _ = Typography::small(format!(
                "rgba_to_png(1, 1, 4 RGBA bytes) -> {}",
                png.as_ref().map_or_else(
                    |e| format!("error: {e}"),
                    |bytes| format!("{} bytes", bytes.len())
                )
            ))
            .show(ui);
            let wrong = functora_egui::clipboard::rgba_to_png(1, 1, &[0xFF, 0x00]);
            _ = Typography::small(format!(
                "rgba_to_png with a wrong buffer length -> {}",
                wrong.as_ref().map_or_else(
                    |e| format!("error: {e}"),
                    |bytes| format!("{} bytes", bytes.len())
                )
            ))
            .show(ui);
        }

        snippet(
            ui,
            "// Clipboard: write + read (async, polled each frame)\nuse functora_egui::clipboard::{write, read};\nuse functora_egui::spawn_async;\n\n// Write (non-blocking: result arrives via receiver)\nlet rx = spawn_async(async move {\n    write(\"hello clipboard\".to_owned()).await.map_err(|e| e.to_string())\n});\n\n// Read (non-blocking: result arrives via receiver)\nlet rx = spawn_async(async move {\n    read().await.map_err(|e| e.to_string())\n});\n// eprintln!(\"pasted: {text}\"); once the receiver is ready\n\n// Image paste slot: the platform hands a pasted image to the app\nuse functora_egui::paste_image::{PasteImage, is_paste_image_intent, store, take};\n\n// The platform stores the pasted image (web paste event, Android intent)\nstore(PasteImage { name: \"clipboard.png\".to_owned(), data: png_bytes });\n\n// The app takes it once per frame and clears the slot\nif let Some(image) = take() {\n    eprintln!(\"{} ({} bytes)\", image.name, image.data.len());\n}\n\n// Detect a paste shortcut in the raw event stream\nif is_paste_image_intent(&ctx.input(|i| i.events.clone())) {\n    eprintln!(\"paste requested\");\n}\n\n// Desktop: read an image from the clipboard and turn raw RGBA into PNG\n#[cfg(not(any(target_arch = \"wasm32\", target_os = \"android\")))]\n{\n    use functora_egui::clipboard::{read_image, rgba_to_png};\n\n    if let Some(image) = read_image().await? {\n        eprintln!(\"{} ({} bytes)\", image.name, image.data.len());\n    }\n\n    let png = rgba_to_png(width, height, &rgba)?;\n    assert!(png.starts_with(&[0x89, b'P', b'N', b'G']));\n}",
        );
    }
}
