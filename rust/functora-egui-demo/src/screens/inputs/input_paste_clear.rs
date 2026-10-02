use functora_egui::snippet;
use functora_egui::{InputPasteClear, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_input_paste_clear(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Single-line input with paste on the left and clear on the right.")
            .show(ui);
        ui.add_space(12.0);

        _ = Typography::small("Default empty string").show(ui);
        ui.add_space(4.0);
        let resp = InputPasteClear::new(&mut self.input_paste_clear_text)
            .placeholder("Paste something...")
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
            "Value: \"{}\"  pasted={} cleared={}",
            self.input_paste_clear_text, resp.pasted, resp.cleared
        ))
        .show(ui);
        if resp.pasted {
            ui.ctx().request_repaint();
        }

        ui.add_space(12.0);
        _ = Typography::small("Custom default value").show(ui);
        ui.add_space(4.0);
        let resp2 = InputPasteClear::new(&mut self.input_paste_clear_custom_default)
            .placeholder("Custom default is \"default value\"")
            .default_value("default value")
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Value: \"{}\"  cleared={}",
            self.input_paste_clear_custom_default, resp2.cleared
        ))
        .show(ui);

        ui.add_space(12.0);
        _ = Typography::small("Password with eye toggle before clear").show(ui);
        ui.add_space(2.0);
        _ = Typography::muted("Layout: [paste | text | eye | clear]. Eye (Eye / EyeOff) appears only with .password() and sits immediately left of X.")
            .show(ui);
        ui.add_space(4.0);
        let _ = InputPasteClear::new(&mut self.input_paste_clear_password)
            .placeholder("secret")
            .password()
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Password len: {}  (click eye to reveal, X to clear, paste to fill)",
            self.input_paste_clear_password.len()
        ))
        .show(ui);

        ui.add_space(12.0);
        _ = Typography::small("Password + custom icons").show(ui);
        ui.add_space(4.0);
        let _ = InputPasteClear::new(&mut self.input_paste_clear_password_custom)
            .placeholder("secret with custom icons")
            .password()
            .paste_icon(LucideIcon::Clipboard)
            .clear_icon(LucideIcon::Trash)
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Password custom len: {}",
            self.input_paste_clear_password_custom.len()
        ))
        .show(ui);

        ui.add_space(12.0);
        _ = Typography::small("Custom icons").show(ui);
        ui.add_space(4.0);
        let _ = InputPasteClear::new(&mut self.input_paste_clear_custom_icons)
            .placeholder("Custom icons: Clipboard / Trash")
            .paste_icon(LucideIcon::Clipboard)
            .clear_icon(LucideIcon::Trash)
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Custom icons value: \"{}\"",
            self.input_paste_clear_custom_icons
        ))
        .show(ui);

        ui.add_space(12.0);
        _ = Typography::small("With copy button").show(ui);
        ui.add_space(2.0);
        _ = Typography::muted(
            "Layout: [paste | copy | text | eye | clear]. Paste is always leftmost, copy is next to it (on by default). Disable with .with_copy(false).",
        )
        .show(ui);
        ui.add_space(4.0);
        let resp_copy = InputPasteClear::new(&mut self.input_paste_clear_copy)
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
            "Copy demo - pasted={} copied={} cleared={} len={}",
            resp_copy.pasted,
            resp_copy.copied,
            resp_copy.cleared,
            self.input_paste_clear_copy.len()
        ))
        .show(ui);

        ui.add_space(12.0);
        _ = Typography::small("Copy with custom icon").show(ui);
        ui.add_space(4.0);
        let _ = InputPasteClear::new(&mut self.input_paste_clear_copy_custom)
            .placeholder("Custom copy icon: CopyPlus")
            .copy_icon(LucideIcon::CopyPlus)
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Custom copy value: \"{}\" len={}",
            self.input_paste_clear_copy_custom,
            self.input_paste_clear_copy_custom.len()
        ))
        .show(ui);

        snippet(
            ui,
            "// InputPasteClear: single-line with paste (left) + clear (right)\n// Password adds eye toggle before clear: [paste | text | eye | clear]\n// Copy button next to paste: [paste | copy | text | eye | clear] (on by default, paste stays leftmost)\nuse functora_egui::{InputPasteClear, LucideIcon};\n\nlet mut text = String::new();\nlet resp = InputPasteClear::new(&mut text)\n    .placeholder(\"Paste something...\")\n    .show(ui);\nif resp.pasted { eprintln!(\"pasted\"); }\nif resp.copied { eprintln!(\"copied\"); }\nif resp.cleared { eprintln!(\"cleared to default\"); }\nif let Some(err) = resp.clipboard_error { eprintln!(\"clipboard error: {err}\"); }\n\n// Custom default (clears to \"default value\" instead of \"\")\nlet mut with_default = \"default value\".to_owned();\nInputPasteClear::new(&mut with_default)\n    .default_value(\"default value\")\n    .show(ui);\n\n// Password: eye (Eye/EyeOff) appears immediately left of X\nlet mut secret = String::new();\nInputPasteClear::new(&mut secret)\n    .password() // -> [paste | •••• | eye | X ]\n    .show(ui);\n\n// Password + custom paste/clear icons (eye stays Eye/EyeOff) - paste stays clipboard-like, clear is trash\nInputPasteClear::new(&mut secret)\n    .password()\n    .paste_icon(LucideIcon::Clipboard)\n    .clear_icon(LucideIcon::Trash)\n    .show(ui);\n\n// Copy button is on by default (paste stays leftmost); opt out explicitly\nInputPasteClear::new(&mut text)\n    .with_copy(false) // -> [paste | text | X ]\n    .show(ui);\n\n// Copy with custom icon - copy uses CopyPlus, distinct from paste's Clipboard\nInputPasteClear::new(&mut text)\n    .copy_icon(LucideIcon::CopyPlus) // -> [paste | copy(CopyPlus) | text | X ]\n    .show(ui);",
        );
    }
}
