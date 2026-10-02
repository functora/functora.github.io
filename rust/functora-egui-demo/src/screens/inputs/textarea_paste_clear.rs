use functora_egui::snippet;
use functora_egui::{LucideIcon, TextareaPasteClear, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_textarea_paste_clear(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Multi-line text area with paste on the left and clear on the right (toolbar).",
        )
        .show(ui);
        ui.add_space(12.0);

        _ = Typography::small("Default empty string").show(ui);
        ui.add_space(4.0);
        let resp = TextareaPasteClear::new(&mut self.textarea_paste_clear_text)
            .placeholder("Paste a long text...")
            .show(ui);
        if let Some(err) = &resp.clipboard_error {
            let msg = format!("Clipboard error: {err}");
            self.toast.add(
                msg,
                functora_egui::ToastVariant::Error,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Chars: {}  pasted={} cleared={}",
            self.textarea_paste_clear_text.len(),
            resp.pasted,
            resp.cleared
        ))
        .show(ui);
        if resp.pasted {
            ui.ctx().request_repaint();
        }

        ui.add_space(12.0);
        _ = Typography::small("Custom icons + min_height").show(ui);
        ui.add_space(4.0);
        let _ = TextareaPasteClear::new(&mut self.textarea_paste_clear_custom)
            .placeholder("Custom icons: Clipboard / Trash2")
            .paste_icon(LucideIcon::Clipboard)
            .clear_icon(LucideIcon::Trash2)
            .min_height(240.0)
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Custom textarea chars: {}",
            self.textarea_paste_clear_custom.len()
        ))
        .show(ui);

        ui.add_space(12.0);
        _ = Typography::small("With copy button").show(ui);
        ui.add_space(2.0);
        _ = Typography::muted(
            "Toolbar layout: [paste | copy | ... | clear]. Paste is always leftmost, copy is next to it (on by default). Disable with .with_copy(false).",
        )
        .show(ui);
        ui.add_space(4.0);
        let resp_copy = TextareaPasteClear::new(&mut self.textarea_paste_clear_copy)
            .placeholder("Copy enabled...")
            .show(ui);
        if let Some(err) = &resp_copy.clipboard_error {
            let msg = format!("Clipboard error: {err}");
            self.toast.add(
                msg,
                functora_egui::ToastVariant::Error,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Copy textarea - pasted={} copied={} cleared={} chars={}",
            resp_copy.pasted,
            resp_copy.copied,
            resp_copy.cleared,
            self.textarea_paste_clear_copy.len()
        ))
        .show(ui);

        ui.add_space(12.0);
        _ = Typography::small("Copy with custom icon").show(ui);
        ui.add_space(4.0);
        let _ = TextareaPasteClear::new(&mut self.textarea_paste_clear_copy_custom)
            .placeholder("Custom copy icon: CopyPlus")
            .copy_icon(LucideIcon::CopyPlus)
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Custom copy textarea chars: {}",
            self.textarea_paste_clear_copy_custom.len()
        ))
        .show(ui);

        snippet(
            ui,
            "// TextareaPasteClear: multi-line with paste + clear toolbar\n// Copy button next to paste: [paste | copy | ... | clear] (on by default, paste stays leftmost)\nuse functora_egui::{TextareaPasteClear, LucideIcon};\n\nlet mut text = String::new();\nlet resp = TextareaPasteClear::new(&mut text)\n    .placeholder(\"Paste a long text...\")\n    .show(ui);\nif resp.pasted { eprintln!(\"pasted\"); }\nif resp.copied { eprintln!(\"copied\"); }\nif resp.cleared { eprintln!(\"cleared\"); }\nif let Some(err) = resp.clipboard_error { eprintln!(\"clipboard error: {err}\"); }\n\n// Custom icons - paste stays clipboard-like (Clipboard), clear is Trash2, distinct from copy's Copy/CopyPlus\nTextareaPasteClear::new(&mut text)\n    .paste_icon(LucideIcon::Clipboard)\n    .clear_icon(LucideIcon::Trash2)\n    .show(ui);\n\n// Taller override (default is 192px)\nTextareaPasteClear::new(&mut text)\n    .min_height(240.0)\n    .show(ui);\n\n// Copy button is on by default (paste stays leftmost); opt out explicitly\nTextareaPasteClear::new(&mut text)\n    .with_copy(false) // -> [paste | ... | clear]\n    .show(ui);\n\n// Copy with custom icon - copy uses CopyPlus, distinct from paste's Clipboard\nTextareaPasteClear::new(&mut text)\n    .copy_icon(LucideIcon::CopyPlus) // -> [paste | copy(CopyPlus) | ... | clear]\n    .show(ui);",
        );
    }
}
