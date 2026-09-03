//! Inputs: buttons, checkboxes, switches, radios, toggles, sliders,
//! text fields, selects, comboboxes, OTP, date picker, color swatch.

use functora_egui::{
    Badge, Button, ButtonGroup, ButtonVariant, Checkbox, ColorSwatch, Combobox, ComponentSize,
    DatePicker, Flex, Input, InputGroup, InputOtp, InputPasteClear, LucideIcon, NumberInput, Radio,
    RadioGroup, Select, SelectValue, Slider, Switch, Textarea, TextareaPasteClear, Toggle,
    ToggleGroup, ToggleVariant, Typography,
};

use functora_egui::snippet;

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_button(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Clickable buttons with variant styles and sizes.").show(ui);
        ui.add_space(12.0);

        _ = Typography::small("Variants").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            _ = f.add(Button::new("Default"));
            _ = f.add(Button::new("Destructive").variant(ButtonVariant::Destructive));
            _ = f.add(Button::new("Outline").variant(ButtonVariant::Outline));
            _ = f.add(Button::new("Secondary").variant(ButtonVariant::Secondary));
            _ = f.add(Button::new("Ghost").variant(ButtonVariant::Ghost));
            _ = f.add(Button::new("Link").variant(ButtonVariant::Link));
        });

        ui.add_space(12.0);
        _ = Typography::small("Sizes").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(Button::new("XS").size(ComponentSize::Xs));
            _ = f.add(Button::new("Small").size(ComponentSize::Sm));
            _ = f.add(Button::new("Default"));
            _ = f.add(Button::new("Large").size(ComponentSize::Lg));
        });

        ui.add_space(12.0);
        _ = Typography::small("Icon only").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Button::icon_only(LucideIcon::Plus));
            _ = f.add(Button::icon_only(LucideIcon::Settings).variant(ButtonVariant::Outline));
            _ = f.add(Button::icon_only(LucideIcon::Trash).variant(ButtonVariant::Destructive));
            _ = f.add(Button::icon_only(LucideIcon::Heart).variant(ButtonVariant::Ghost));
            _ = f.add(
                Button::icon_only(LucideIcon::Search)
                    .variant(ButtonVariant::Secondary)
                    .size(ComponentSize::Sm),
            );
            _ = f.add(
                Button::icon_only(LucideIcon::Star)
                    .variant(ButtonVariant::Outline)
                    .size(ComponentSize::Lg),
            );
        });

        ui.add_space(12.0);
        _ = Typography::small("Icon + text").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Button::new("Download").icon(LucideIcon::Download));
            _ = f.add(
                Button::new("Upload")
                    .icon(LucideIcon::Upload)
                    .variant(ButtonVariant::Outline),
            );
            _ = f.add(
                Button::new("Mail")
                    .icon(LucideIcon::Mail)
                    .variant(ButtonVariant::Secondary),
            );
            _ = f.add(
                Button::new("Copy")
                    .icon(LucideIcon::Copy)
                    .variant(ButtonVariant::Ghost)
                    .size(ComponentSize::Sm),
            );
        });

        ui.add_space(12.0);
        _ = Typography::small("Shortcut text").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(
                Button::new("Save")
                    .variant(ButtonVariant::Outline)
                    .shortcut_text("Ctrl+S"),
            );
            _ = f.add(
                Button::new("Open")
                    .variant(ButtonVariant::Outline)
                    .shortcut_text("Ctrl+O"),
            );
        });

        ui.add_space(12.0);
        _ = Typography::small("Selected (toggle)").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(
                Button::new("Toggle Me")
                    .variant(ButtonVariant::Outline)
                    .selected(self.toolbar.toolbar_snap),
            )
            .response
            .clicked()
            {
                self.toolbar.toolbar_snap = !self.toolbar.toolbar_snap;
            }
        });

        ui.add_space(12.0);
        _ = Typography::small("Disabled").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Button::new("Disabled").enabled(false));
            _ = f.add(
                Button::new("Disabled Outline")
                    .variant(ButtonVariant::Outline)
                    .enabled(false),
            );
        });

        ui.add_space(12.0);
        _ = Typography::small("Button group").show(ui);
        ui.add_space(4.0);
        _ = ButtonGroup::show(ui, |ui46| {
            _ = Button::new("Left")
                .variant(ButtonVariant::Outline)
                .show(ui46);
            _ = Button::new("Center")
                .variant(ButtonVariant::Outline)
                .show(ui46);
            _ = Button::new("Right")
                .variant(ButtonVariant::Outline)
                .show(ui46);
        });

        snippet(
            ui,
            "// Button: variants + sizes + icons + shortcuts\nuse functora_egui::{Button, ButtonVariant, ComponentSize, LucideIcon};\n\n// Variants\nButton::new(\"Default\").show(ui);\nButton::new(\"Destructive\").variant(ButtonVariant::Destructive).show(ui);\nButton::new(\"Outline\").variant(ButtonVariant::Outline).show(ui);\nButton::new(\"Secondary\").variant(ButtonVariant::Secondary).show(ui);\nButton::new(\"Ghost\").variant(ButtonVariant::Ghost).show(ui);\nButton::new(\"Link\").variant(ButtonVariant::Link).show(ui);\n\n// Sizes\nButton::new(\"XS\").size(ComponentSize::Xs).show(ui);\nButton::new(\"Small\").size(ComponentSize::Sm).show(ui);\nButton::new(\"Default\").show(ui);\nButton::new(\"Large\").size(ComponentSize::Lg).show(ui);\n\n// Icon only\nButton::icon_only(LucideIcon::Plus).show(ui);\nButton::icon_only(LucideIcon::Settings).variant(ButtonVariant::Outline).show(ui);\n\n// Icon + text\nButton::new(\"Download\").icon(LucideIcon::Download).show(ui);\nButton::new(\"Upload\").icon(LucideIcon::Upload).variant(ButtonVariant::Outline).show(ui);\n\n// Shortcut text\nButton::new(\"Save\").shortcut_text(\"Ctrl+S\").variant(ButtonVariant::Outline).show(ui);\n\n// Disabled\nButton::new(\"Disabled\").enabled(false).show(ui);",
        );
    }

    pub(crate) fn demo_checkbox(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Toggle boolean values with a checkbox.").show(ui);
        ui.add_space(12.0);
        _ = ui
            .add(Checkbox::new(&mut self.checks.checkbox_val).label("Accept terms and conditions"));
        ui.add_space(4.0);
        _ = Typography::small(format!("Checked: {}", self.checks.checkbox_val)).show(ui);

        snippet(
            ui,
            "// Checkbox: bound to boolean\nuse functora_egui::Checkbox;\n\nlet mut checked = false;\nui.add(Checkbox::new(&mut checked).label(\"Accept terms and conditions\"));\n\n// checked is now true/false based on user interaction",
        );
    }

    pub(crate) fn demo_switch(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A toggle switch for on/off states.").show(ui);
        ui.add_space(12.0);
        _ = ui.add(Switch::new(&mut self.checks.switch_val).label("Airplane mode"));
        ui.add_space(4.0);
        _ = Typography::small(format!("Enabled: {}", self.checks.switch_val)).show(ui);

        snippet(
            ui,
            "// Switch: toggle on/off with label\nuse functora_egui::Switch;\n\nlet mut enabled = false;\nui.add(Switch::new(&mut enabled).label(\"Airplane mode\"));\n\n// enabled is now true/false",
        );
    }

    pub(crate) fn demo_radio(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Individual radio buttons for exclusive selection.").show(ui);
        ui.add_space(12.0);
        if ui
            .add(Radio::new(&mut self.radios.radio_a).label("Option A"))
            .clicked()
        {
            self.radios.radio_b = false;
            self.radios.radio_c = false;
        }
        if ui
            .add(Radio::new(&mut self.radios.radio_b).label("Option B"))
            .clicked()
        {
            self.radios.radio_a = false;
            self.radios.radio_c = false;
        }
        if ui
            .add(Radio::new(&mut self.radios.radio_c).label("Option C"))
            .clicked()
        {
            self.radios.radio_a = false;
            self.radios.radio_b = false;
        }

        snippet(
            ui,
            "// Radio: individual buttons for exclusive selection\nuse functora_egui::Radio;\n\nlet mut option_a = true;\nlet mut option_b = false;\nlet mut option_c = false;\n\nif ui.add(Radio::new(&mut option_a).label(\"Option A\")).clicked() {\n    option_b = false;\n    option_c = false;\n}\nif ui.add(Radio::new(&mut option_b).label(\"Option B\")).clicked() {\n    option_a = false;\n    option_c = false;\n}\nif ui.add(Radio::new(&mut option_c).label(\"Option C\")).clicked() {\n    option_a = false;\n    option_b = false;\n}",
        );
    }

    pub(crate) fn demo_radio_group(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A group of radio buttons managed together.").show(ui);
        ui.add_space(12.0);
        let options = [
            "Option A".to_owned(),
            "Option B".to_owned(),
            "Option C".to_owned(),
        ];
        _ = RadioGroup::new(&mut self.radio_group_val, &options).show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected: {}", self.radio_group_val)).show(ui);

        snippet(
            ui,
            "// RadioGroup: managed group of radio buttons\nuse functora_egui::RadioGroup;\n\nlet options = [\"Option A\", \"Option B\", \"Option C\"];\nlet mut selected = \"Option A\".to_owned();\n\nRadioGroup::new(&mut selected, &options).show(ui);\n\n// selected now contains the chosen option",
        );
    }

    pub(crate) fn demo_toggle(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Toggle buttons with default and outline variants.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(Toggle::new(&mut self.text_style.toggle_bold, "B"));
            _ = f.add(
                Toggle::new(&mut self.text_style.toggle_italic, "I")
                    .variant(ToggleVariant::Outline),
            );
            _ = f.add(
                Toggle::new(&mut self.text_style.toggle_underline, "U")
                    .variant(ToggleVariant::Outline),
            );
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Bold: {}, Italic: {}, Underline: {}",
            self.text_style.toggle_bold,
            self.text_style.toggle_italic,
            self.text_style.toggle_underline
        ))
        .show(ui);

        snippet(
            ui,
            "// Toggle: pressable button for boolean state\nuse functora_egui::{Toggle, ToggleVariant};\n\nlet mut bold = false;\nlet mut italic = false;\nlet mut underline = false;\n\nToggle::new(&mut bold, \"B\").show(ui);\nToggle::new(&mut italic, \"I\").variant(ToggleVariant::Outline).show(ui);\nToggle::new(&mut underline, \"U\").variant(ToggleVariant::Outline).show(ui);",
        );
    }

    pub(crate) fn demo_toggle_group(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Exclusive toggle group: only one active at a time.").show(ui);
        ui.add_space(12.0);
        _ = ToggleGroup::new(vec![
            "Left".to_owned(),
            "Center".to_owned(),
            "Right".to_owned(),
        ])
        .show(ui, &mut self.toggle_group_idx);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected index: {}", self.toggle_group_idx)).show(ui);

        snippet(
            ui,
            "// ToggleGroup: exclusive selection (only one active)\nuse functora_egui::ToggleGroup;\n\nlet items = vec![\"Left\", \"Center\", \"Right\"];\nlet mut selected_idx = 0;\n\nToggleGroup::new(items).show(ui, &mut selected_idx);\n\n// selected_idx now contains the chosen index",
        );
    }

    pub(crate) fn demo_slider(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Drag to select a numeric value within a range.").show(ui);
        ui.add_space(12.0);
        _ = Slider::new(&mut self.slider_val, 0.0..=100.0)
            .step(1.0)
            .width(ui.available_width().min(400.0))
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Value: {:.0}", self.slider_val)).show(ui);

        ui.add_space(16.0);
        _ = Typography::small("With suffix").show(ui);
        ui.add_space(4.0);
        _ = Slider::new(&mut self.slider_price, 0.0..=1000.0)
            .step(10.0)
            .suffix(" USD")
            .width(ui.available_width().min(400.0))
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Budget: ${:.0}", self.slider_price)).show(ui);

        snippet(
            ui,
            "// Slider: drag to select value in range\nuse functora_egui::Slider;\n\nlet mut value = 50.0;\nSlider::new(&mut value, 0.0..=100.0)\n    .step(1.0)\n    .width(400.0)\n    .show(ui);\n\n// With suffix\nlet mut price = 200.0;\nSlider::new(&mut price, 0.0..=1000.0)\n    .step(10.0)\n    .suffix(\" USD\")\n    .width(400.0)\n    .show(ui);",
        );
    }

    pub(crate) fn demo_input(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Single-line text input field.").show(ui);
        ui.add_space(12.0);
        _ = Input::new(&mut self.input_text)
            .placeholder("Type something...")
            .desired_width(ui.available_width())
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Value: \"{}\"", self.input_text)).show(ui);

        ui.add_space(12.0);
        _ = Typography::small("Password").show(ui);
        ui.add_space(4.0);
        _ = Input::new(&mut self.input_text)
            .password()
            .placeholder("secret")
            .desired_width(ui.available_width())
            .show(ui);

        snippet(
            ui,
            "// Input: single-line text field\nuse functora_egui::Input;\n\nlet mut text = String::new();\nInput::new(&mut text)\n    .placeholder(\"Type something...\")\n    .desired_width(ui.available_width())\n    .show(ui);\n\n// Password\nlet mut secret = String::new();\nInput::new(&mut secret)\n    .password()\n    .placeholder(\"secret\")\n    .desired_width(ui.available_width())\n    .show(ui);",
        );
    }

    pub(crate) fn demo_number_input(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Numeric input with drag, range, prefix/suffix.").show(ui);
        ui.add_space(12.0);

        _ = Typography::small("f64 with range and suffix").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(
                NumberInput::new(&mut self.number_f64)
                    .range(0.0..=100.0)
                    .speed(0.5)
                    .suffix("px")
                    .width(110.0),
            );
            _ = f.ui(|ui65| {
                _ = Typography::small(format!("{:.1}", self.number_f64)).show(ui65);
            });
        });

        ui.add_space(12.0);
        _ = Typography::small("f32 with decimals and prefix").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(
                NumberInput::f32(&mut self.number_f32)
                    .decimals(2)
                    .prefix("$")
                    .width(90.0),
            );
            _ = f.ui(|ui66| {
                _ = Typography::small(format!("{:.2}", self.number_f32)).show(ui66);
            });
        });

        ui.add_space(12.0);
        _ = Typography::small("i32 integer").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(
                NumberInput::i32(&mut self.number_i32)
                    .range(0.0..=50.0)
                    .width(70.0),
            );
            _ = f.ui(|ui67| {
                _ = Typography::small(format!("{}", self.number_i32)).show(ui67);
            });
        });

        snippet(
            ui,
            "// NumberInput: numeric input with drag, range, prefix/suffix\nuse functora_egui::NumberInput;\n\n// f64 with range and suffix\nlet mut px = 0.0;\nNumberInput::new(&mut px)\n    .range(0.0..=100.0)\n    .speed(0.5)\n    .suffix(\"px\")\n    .width(110.0)\n    .show(ui);\n\n// f32 with decimals and prefix\nlet mut price = 0.0_f32;\nNumberInput::f32(&mut price)\n    .decimals(2)\n    .prefix(\"$\")\n    .width(90.0)\n    .show(ui);\n\n// i32 integer\nlet mut count = 0i32;\nNumberInput::i32(&mut count)\n    .range(0.0..=50.0)\n    .width(70.0)\n    .show(ui);",
        );
    }

    pub(crate) fn demo_input_group(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Input with prefix text and suffix addons.").show(ui);
        ui.add_space(12.0);

        _ = Typography::small("With prefix").show(ui);
        ui.add_space(4.0);
        _ = InputGroup::show(
            ui,
            &mut self.input_group_text,
            "example.com",
            Some("https://"),
            None::<fn(&mut egui::Ui)>,
        );

        ui.add_space(12.0);
        _ = Typography::small("With prefix and suffix button").show(ui);
        ui.add_space(4.0);
        _ = InputGroup::show(
            ui,
            &mut self.input_group_text,
            "Search...",
            None,
            Some(|ui68: &mut egui::Ui| {
                _ = Button::icon_only(LucideIcon::Search)
                    .variant(ButtonVariant::Ghost)
                    .size(ComponentSize::Sm)
                    .show(ui68);
            }),
        );

        snippet(
            ui,
            "// InputGroup: input with prefix text and/or suffix addon\nuse functora_egui::{InputGroup, Button, ButtonVariant, LucideIcon, ComponentSize};\n\n// With prefix\nlet mut url = String::new();\nInputGroup::show(\n    ui,\n    &mut url,\n    \"example.com\",\n    Some(\"https://\"),\n    None::<fn(&mut egui::Ui)>,\n);\n\n// With prefix and suffix button\nlet mut search = String::new();\nInputGroup::show(\n    ui,\n    &mut search,\n    \"Search...\",\n    None,\n    Some(|ui| {\n        Button::icon_only(LucideIcon::Search)\n            .variant(ButtonVariant::Ghost)\n            .size(ComponentSize::Sm)\n            .show(ui);\n    }),\n);",
        );
    }

    pub(crate) fn demo_input_paste_clear(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Single-line input with paste on the left and clear on the right.")
            .show(ui);
        ui.add_space(12.0);

        _ = Typography::small("Default empty string").show(ui);
        ui.add_space(4.0);
        let resp = InputPasteClear::new(&mut self.input_paste_clear_text)
            .placeholder("Paste something...")
            .show(ui);
        if let Some(err) = &resp.clipboard_error {
            let msg = err.to_string();
            ui.add_space(4.0);
            _ = ui.add(Badge::new(format!("Clipboard error: {msg}")));
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
        _ = Typography::small("Custom icons").show(ui);
        ui.add_space(4.0);
        let _ = InputPasteClear::new(&mut self.input_paste_clear_custom_icons)
            .placeholder("Custom icons: Copy / Trash")
            .paste_icon(LucideIcon::Copy)
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
            "Layout: [paste | copy | text | eye | clear]. Paste is always leftmost, copy is optional next to it (off by default). Enable with .copy().",
        )
        .show(ui);
        ui.add_space(4.0);
        let resp_copy = InputPasteClear::new(&mut self.input_paste_clear_copy)
            .placeholder("Copy enabled...")
            .copy()
            .show(ui);
        if let Some(err) = &resp_copy.clipboard_error {
            let msg = err.to_string();
            ui.add_space(4.0);
            _ = ui.add(Badge::new(format!("Clipboard error: {msg}")));
        }
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Copy demo - pasted={} copied={} cleared={} len={}",
            resp_copy.copied,
            resp_copy.pasted,
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
            "// InputPasteClear: single-line with paste (left) + clear (right)\n// Password adds eye toggle before clear: [paste | text | eye | clear]\n// Copy adds button next to paste: [paste | copy | text | eye | clear] (off by default, paste stays leftmost)\nuse functora_egui::{InputPasteClear, LucideIcon};\n\nlet mut text = String::new();\nlet resp = InputPasteClear::new(&mut text)\n    .placeholder(\"Paste something...\")\n    .show(ui);\nif resp.pasted { eprintln!(\"pasted\"); }\nif resp.copied { eprintln!(\"copied\"); }\nif resp.cleared { eprintln!(\"cleared to default\"); }\nif let Some(err) = resp.clipboard_error { eprintln!(\"clipboard error: {err}\"); }\n\n// Custom default (clears to \"default value\" instead of \"\")\nlet mut with_default = \"default value\".to_owned();\nInputPasteClear::new(&mut with_default)\n    .default_value(\"default value\")\n    .show(ui);\n\n// Password: eye (Eye/EyeOff) appears immediately left of X\nlet mut secret = String::new();\nInputPasteClear::new(&mut secret)\n    .password() // -> [paste | •••• | eye | X ]\n    .show(ui);\n\n// Password + custom paste/clear icons (eye stays Eye/EyeOff)\nInputPasteClear::new(&mut secret)\n    .password()\n    .paste_icon(LucideIcon::Copy)\n    .clear_icon(LucideIcon::Trash)\n    .show(ui);\n\n// Copy button next to paste (paste stays leftmost, off by default)\nInputPasteClear::new(&mut text)\n    .copy() // -> [paste | copy | text | X ]\n    .show(ui);\n\n// Copy with custom icon (also enables copy)\nInputPasteClear::new(&mut text)\n    .copy_icon(LucideIcon::CopyPlus) // -> [paste | copy(CopyPlus) | text | X ]\n    .show(ui);",
        );
    }

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
            .min_height(80.0)
            .show(ui);
        if let Some(err) = &resp.clipboard_error {
            let msg = err.to_string();
            ui.add_space(4.0);
            _ = ui.add(Badge::new(format!("Clipboard error: {msg}")));
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
            .placeholder("Custom icons")
            .paste_icon(LucideIcon::Copy)
            .clear_icon(LucideIcon::Trash2)
            .min_height(100.0)
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
            "Toolbar layout: [paste | copy | ... | clear]. Paste is always leftmost, copy is optional next to it (off by default). Enable with .copy().",
        )
        .show(ui);
        ui.add_space(4.0);
        let resp_copy = TextareaPasteClear::new(&mut self.textarea_paste_clear_copy)
            .placeholder("Copy enabled...")
            .copy()
            .min_height(80.0)
            .show(ui);
        if let Some(err) = &resp_copy.clipboard_error {
            let msg = err.to_string();
            ui.add_space(4.0);
            _ = ui.add(Badge::new(format!("Clipboard error: {msg}")));
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
            .min_height(100.0)
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Custom copy textarea chars: {}",
            self.textarea_paste_clear_copy_custom.len()
        ))
        .show(ui);

        snippet(
            ui,
            "// TextareaPasteClear: multi-line with paste + clear toolbar\n// Copy adds button next to paste: [paste | copy | ... | clear] (off by default, paste stays leftmost)\nuse functora_egui::{TextareaPasteClear, LucideIcon};\n\nlet mut text = String::new();\nlet resp = TextareaPasteClear::new(&mut text)\n    .placeholder(\"Paste a long text...\")\n    .min_height(80.0)\n    .show(ui);\nif resp.pasted { eprintln!(\"pasted\"); }\nif resp.copied { eprintln!(\"copied\"); }\nif resp.cleared { eprintln!(\"cleared\"); }\nif let Some(err) = resp.clipboard_error { eprintln!(\"clipboard error: {err}\"); }\n\n// Custom icons\nTextareaPasteClear::new(&mut text)\n    .paste_icon(LucideIcon::Copy)\n    .clear_icon(LucideIcon::Trash2)\n    .min_height(100.0)\n    .show(ui);\n\n// Copy button next to paste (paste stays leftmost, off by default)\nTextareaPasteClear::new(&mut text)\n    .copy() // -> [paste | copy | ... | clear]\n    .min_height(80.0)\n    .show(ui);\n\n// Copy with custom icon (also enables copy)\nTextareaPasteClear::new(&mut text)\n    .copy_icon(LucideIcon::CopyPlus) // -> [paste | copy(CopyPlus) | ... | clear]\n    .min_height(80.0)\n    .show(ui);",
        );
    }

    pub(crate) fn demo_textarea(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Multi-line text area.").show(ui);
        ui.add_space(12.0);
        _ = Textarea::new(&mut self.textarea_text)
            .placeholder("Write a message...")
            .desired_width(ui.available_width().min(420.0))
            .min_height(80.0)
            .show(ui);

        snippet(
            ui,
            "// Textarea: multi-line text area\nuse functora_egui::Textarea;\n\nlet mut msg = String::new();\nTextarea::new(&mut msg)\n    .placeholder(\"Write a message...\")\n    .desired_width(ui.available_width().min(420.0))\n    .min_height(80.0)\n    .show(ui);",
        );
    }

    pub(crate) fn demo_select(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Dropdown selection from a list.").show(ui);
        ui.add_space(12.0);
        let fruits = vec![
            "Apple".to_owned(),
            "Banana".to_owned(),
            "Cherry".to_owned(),
            "Grape".to_owned(),
            "Mango".to_owned(),
        ];
        _ = Select::new(&mut self.select_val, &fruits)
            .placeholder("Pick a fruit...")
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected: {:?}", self.select_val)).show(ui);

        snippet(
            ui,
            "// Select: dropdown selection from a list (Option value)\nuse functora_egui::Select;\n\nlet fruits = vec![\"Apple\", \"Banana\", \"Cherry\", \"Grape\", \"Mango\"];\nlet mut fruit: Option<String> = None;\n\nSelect::new(&mut fruit, &fruits)\n    .placeholder(\"Pick a fruit...\")\n    .show(ui);\n\n// fruit is now Some(\"Apple\") or None",
        );
    }

    pub(crate) fn demo_select_value(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Select bound to a non-Option string value.").show(ui);
        ui.add_space(12.0);
        let blend_modes = vec![
            "Normal".to_owned(),
            "Multiply".to_owned(),
            "Screen".to_owned(),
            "Overlay".to_owned(),
        ];
        _ = SelectValue::new(&mut self.select_blend, &blend_modes).show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Blend mode: {}", self.select_blend)).show(ui);

        snippet(
            ui,
            "// SelectValue: dropdown bound to non-Option string\nuse functora_egui::SelectValue;\n\nlet blend_modes = vec![\"Normal\", \"Multiply\", \"Screen\", \"Overlay\"];\nlet mut blend_mode = \"Normal\".to_owned();\n\nSelectValue::new(&mut blend_mode, &blend_modes).show(ui);\n\n// blend_mode always has a valid value (never None)",
        );
    }

    pub(crate) fn demo_combobox(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Searchable dropdown with type-ahead filtering.").show(ui);
        ui.add_space(12.0);
        let frameworks = vec![
            "React".to_owned(),
            "Vue".to_owned(),
            "Angular".to_owned(),
            "Svelte".to_owned(),
            "Solid".to_owned(),
        ];
        _ = Combobox::new(frameworks)
            .placeholder("Select framework...")
            .show(ui, &mut self.combobox_selected, &mut self.combobox_search);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected index: {:?}", self.combobox_selected)).show(ui);

        snippet(
            ui,
            "// Combobox: searchable dropdown with type-ahead filtering\nuse functora_egui::Combobox;\n\nlet frameworks = vec![\"React\", \"Vue\", \"Angular\", \"Svelte\", \"Solid\"];\nlet mut selected_idx: Option<usize> = None;\nlet mut search = String::new();\n\nCombobox::new(frameworks)\n    .placeholder(\"Select framework...\")\n    .show(ui, &mut selected_idx, &mut search);\n\n// selected_idx is Some(index) or None",
        );
    }

    pub(crate) fn demo_input_otp(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("One-time passcode digit input boxes.").show(ui);
        ui.add_space(12.0);
        _ = InputOtp::new(6).show(ui, &mut self.otp_value);
        ui.add_space(4.0);
        _ = Typography::small(format!("OTP: \"{}\"", self.otp_value)).show(ui);

        snippet(
            ui,
            "// InputOtp: one-time passcode digit boxes\nuse functora_egui::InputOtp;\n\nlet mut otp = String::new();\nInputOtp::new(6).show(ui, &mut otp);\n\n// otp now contains the 6-digit code",
        );
    }

    pub(crate) fn demo_date_picker(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Date picker with a popover calendar.").show(ui);
        ui.add_space(12.0);
        _ = DatePicker::new()
            .placeholder("Pick a date")
            .show(ui, &mut self.date_picker);
        ui.add_space(4.0);
        if self.date_picker.is_set() {
            _ = Typography::small(format!("Selected: {}", self.date_picker.format())).show(ui);
        } else {
            _ = Typography::small("No date selected.").show(ui);
        }

        snippet(
            ui,
            "// DatePicker: popover calendar\nuse functora_egui::{DatePicker, DatePickerState};\n\nlet mut state = DatePickerState::default();\n\nDatePicker::new()\n    .placeholder(\"Pick a date\")\n    .show(ui, &mut state);\n\nif state.is_set() {\n    let date = state.format(); // e.g. \"2026-08-27\"\n    eprintln!(\"Selected: {date}\");\n}",
        );
    }

    pub(crate) fn demo_color_swatch(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Clickable color swatches for palettes and style controls.").show(ui);
        ui.add_space(12.0);

        let palette = [
            ("Signal", egui::Color32::from_rgb(25, 113, 194)),
            ("Mint", egui::Color32::from_rgb(18, 184, 134)),
            ("Amber", egui::Color32::from_rgb(245, 159, 0)),
            ("Rose", egui::Color32::from_rgb(224, 49, 49)),
            ("Ink", egui::Color32::from_rgb(33, 37, 41)),
        ];
        _ = Typography::small("Palette").show(ui);
        ui.add_space(6.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            for (idx, (label, color)) in palette.iter().enumerate() {
                if f.add(
                    ColorSwatch::new(*color)
                        .label(*label)
                        .selected(self.color_swatch_idx == idx)
                        .show_hex(),
                )
                .response
                .clicked()
                {
                    self.color_swatch_idx = idx;
                }
            }
        });

        ui.add_space(16.0);
        _ = Typography::small("Compact states").show(ui);
        ui.add_space(6.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(ColorSwatch::new(egui::Color32::from_rgb(25, 113, 194)).selected(true));
            _ = f.add(ColorSwatch::new(egui::Color32::from_rgba_unmultiplied(
                25, 113, 194, 120,
            )));
            _ = f.add(
                ColorSwatch::new(egui::Color32::TRANSPARENT)
                    .label("Transparent")
                    .show_hex(),
            );
        });

        snippet(
            ui,
            "// ColorSwatch: clickable color swatches\nuse functora_egui::ColorSwatch;\nuse egui::Color32;\n\nlet palette = [\n    (\"Signal\", Color32::from_rgb(25, 113, 194)),\n    (\"Mint\", Color32::from_rgb(18, 184, 134)),\n    (\"Amber\", Color32::from_rgb(245, 159, 0)),\n    (\"Rose\", Color32::from_rgb(224, 49, 49)),\n    (\"Ink\", Color32::from_rgb(33, 37, 41)),\n];\n\nfor (idx, (label, color)) in palette.iter().enumerate() {\n    if ColorSwatch::new(*color)\n        .label(*label)\n        .selected(selected_idx == idx)\n        .show_hex()\n        .show(ui)\n        .clicked()\n    {\n        selected_idx = idx;\n    }\n}\n\n// Compact states\nColorSwatch::new(Color32::from_rgb(25, 113, 194)).selected(true).show(ui);\nColorSwatch::new(Color32::from_rgba_unmultiplied(25, 113, 194, 120)).show(ui);\nColorSwatch::new(Color32::TRANSPARENT).label(\"Transparent\").show_hex().show(ui);",
        );
    }
}
