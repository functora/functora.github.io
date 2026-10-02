use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Flex, Input, Label, Textarea, Typography, spawn_async};

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

        snippet(
            ui,
            "// Clipboard: write + read (async, polled each frame)\nuse functora_egui::clipboard::{write, read};\nuse functora_egui::spawn_async;\n\n// Write (non-blocking: result arrives via receiver)\nlet rx = spawn_async(async move {\n    write(\"hello clipboard\".to_owned()).await.map_err(|e| e.to_string())\n});\n\n// Read (non-blocking: result arrives via receiver)\nlet rx = spawn_async(async move {\n    read().await.map_err(|e| e.to_string())\n});\n// eprintln!(\"pasted: {text}\"); once the receiver is ready",
        );
    }
}
