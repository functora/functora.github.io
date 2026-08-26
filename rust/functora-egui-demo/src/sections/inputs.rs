//! Inputs: buttons, checkboxes, switches, radios, toggles, sliders,
//! text fields, selects, comboboxes, OTP, date picker, color swatch.

use functora_egui::{
    Button, ButtonGroup, ButtonVariant, Checkbox, ColorSwatch, Combobox, ComponentSize, DatePicker,
    Flex, Input, InputGroup, InputOtp, LucideIcon, NumberInput, Radio, RadioGroup, Select,
    SelectValue, Slider, Switch, Textarea, Toggle, ToggleGroup, ToggleVariant, Typography,
};

use super::code::snippet;

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

        super::code::snippet(
            ui,
            "Button::new(\"Default\").show(ui);\nButton::new(\"Destructive\").variant(ButtonVariant::Destructive).show(ui);\nButton::icon_only(LucideIcon::Plus).show(ui);\nButton::new(\"Download\").icon(LucideIcon::Download).show(ui);\nButton::new(\"Save\").shortcut_text(\"Ctrl+S\").show(ui);\nButton::new(\"Disabled\").enabled(false).show(ui);",
        );
    }

    pub(crate) fn demo_checkbox(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Toggle boolean values with a checkbox.").show(ui);
        ui.add_space(12.0);
        _ = ui
            .add(Checkbox::new(&mut self.checks.checkbox_val).label("Accept terms and conditions"));
        ui.add_space(4.0);
        _ = Typography::small(format!("Checked: {}", self.checks.checkbox_val)).show(ui);

        super::code::snippet(
            ui,
            "ui.add(Checkbox::new(&mut checked).label(\"Accept terms\"));",
        );
    }

    pub(crate) fn demo_switch(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A toggle switch for on/off states.").show(ui);
        ui.add_space(12.0);
        _ = ui.add(Switch::new(&mut self.checks.switch_val).label("Airplane mode"));
        ui.add_space(4.0);
        _ = Typography::small(format!("Enabled: {}", self.checks.switch_val)).show(ui);

        super::code::snippet(
            ui,
            "ui.add(Switch::new(&mut enabled).label(\"Airplane mode\"));",
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

        snippet(ui, "ui.add(Radio::new(&mut option_a).label(\"Option A\"));");
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

        snippet(ui, "RadioGroup::new(&mut selected, &options).show(ui);");
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

        super::code::snippet(
            ui,
            "Toggle::new(&mut bold, \"B\").show(ui);\nToggle::new(&mut italic, \"I\").variant(ToggleVariant::Outline).show(ui);",
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

        snippet(ui, "ToggleGroup::new(items).show(ui, &mut selected_idx);");
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

        super::code::snippet(
            ui,
            "Slider::new(&mut value, 0.0..=100.0).step(1.0).show(ui);\nSlider::new(&mut price, 0.0..=1000.0).suffix(\" USD\").show(ui);",
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

        super::code::snippet(
            ui,
            "Input::new(&mut text).placeholder(\"...\").show(ui);\nInput::new(&mut secret).password().show(ui);",
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

        super::code::snippet(
            ui,
            "NumberInput::new(&mut px).range(0.0..=100.0).suffix(\"px\").show(ui);\nNumberInput::f32(&mut price).decimals(2).prefix(\"$\").show(ui);\nNumberInput::i32(&mut count).range(0.0..=50.0).show(ui);",
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

        super::code::snippet(
            ui,
            "InputGroup::show(ui, &mut url, \"example.com\", Some(\"https://\"), None);",
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

        super::code::snippet(
            ui,
            "Textarea::new(&mut msg).placeholder(\"Write...\").min_height(80.0).show(ui);",
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

        super::code::snippet(
            ui,
            "Select::new(&mut fruit, &fruits).placeholder(\"Pick a fruit...\").show(ui);",
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

        super::code::snippet(
            ui,
            "SelectValue::new(&mut blend_mode, &blend_modes).show(ui);",
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

        super::code::snippet(
            ui,
            "Combobox::new(items).placeholder(\"Search...\")\n    .show(ui, &mut selected_idx, &mut search);",
        );
    }

    pub(crate) fn demo_input_otp(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("One-time passcode digit input boxes.").show(ui);
        ui.add_space(12.0);
        _ = InputOtp::new(6).show(ui, &mut self.otp_value);
        ui.add_space(4.0);
        _ = Typography::small(format!("OTP: \"{}\"", self.otp_value)).show(ui);

        snippet(ui, "InputOtp::new(6).show(ui, &mut code);");
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

        super::code::snippet(
            ui,
            "DatePicker::new().placeholder(\"Pick a date\").show(ui, &mut state);",
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

        super::code::snippet(
            ui,
            "ColorSwatch::new(color).label(\"Signal\").selected(true).show_hex();",
        );
    }
}
