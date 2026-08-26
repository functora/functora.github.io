//! Forms: field groups, field sets, legends, descriptions, property grids.

use functora_egui::{
    Badge, Button, ButtonVariant, FieldDescription, FieldGroup, FieldLegend, FieldSet, Flex, Input,
    Label, NumberInput, PropertyGrid, PropertyRow, Select, Typography,
};

use super::code::snippet;

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_field_group(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Groups related fields with a legend and description.").show(ui);
        ui.add_space(12.0);
        _ = FieldGroup::show(ui, |ui27| {
            _ = Flex::column().gap(8.0).show(ui27, |f| {
                _ = f.ui(|ui28| {
                    _ = Label::new("Card number").show(ui28);
                });
                _ = f.add(Input::new(&mut self.form.form_card).placeholder("4242 4242 4242 4242"));
                _ = f.ui(|ui29| {
                    _ = Label::new("Expiry").show(ui29);
                });
                _ = f.nested(Flex::row().gap(8.0), |f3| {
                    _ = f3.add(
                        Select::new(
                            &mut self.form.form_month,
                            &[
                                "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
                                "12",
                            ]
                            .map(str::to_owned),
                        )
                        .placeholder("Month"),
                    );
                    _ = f3.add(
                        Select::new(
                            &mut self.form.form_year,
                            &["2026", "2027", "2028", "2029", "2030"].map(str::to_owned),
                        )
                        .placeholder("Year"),
                    );
                });
                _ = f.ui(|ui30| {
                    _ = Label::new("CVV").show(ui30);
                });
                _ = f.add(Input::new(&mut self.form.form_cvv).placeholder("123"));
            });
        });

        snippet(ui, "FieldGroup::show(ui, |group| { ... });");
    }

    pub(crate) fn demo_field_set(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A bordered fieldset container for grouped controls.").show(ui);
        ui.add_space(12.0);
        _ = FieldSet::show(ui, "Shipping address", |ui31| {
            _ = Flex::column().gap(8.0).show(ui31, |f| {
                _ = f.ui(|ui32| {
                    _ = Label::new("Full name").show(ui32);
                });
                _ = f.add(Input::new(&mut self.form.form_name).placeholder("Ada Lovelace"));
                _ = f.ui(|ui33| {
                    _ = Label::new("Email").show(ui33);
                });
                _ = f.add(Input::new(&mut self.flex_email).placeholder("ada@example.com"));
            });
        });

        super::code::snippet(
            ui,
            "FieldSet::show(ui, \"Shipping address\", |body| { ... });",
        );
    }

    pub(crate) fn demo_field_legend(ui: &mut egui::Ui) {
        _ = Typography::muted("A legend heading for a field group.").show(ui);
        ui.add_space(12.0);
        FieldLegend::show(ui, "Payment details");
        ui.add_space(4.0);
        FieldDescription::show(ui, "All transactions are secure and encrypted.");
        ui.add_space(8.0);
        FieldLegend::show(ui, "Billing address");
        ui.add_space(4.0);
        FieldDescription::show(ui, "Used only for invoices and receipts.");

        super::code::snippet(
            ui,
            "FieldLegend::show(ui, \"Payment details\");\nFieldDescription::show(ui, \"All transactions are secure.\");",
        );
    }

    pub(crate) fn demo_field_description(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Supporting helper text under a field.").show(ui);
        ui.add_space(12.0);
        _ = Flex::column().gap(8.0).show(ui, |f| {
            _ = f.ui(|ui63| {
                _ = Label::new("Password").show(ui63);
            });
            _ = f.add(Input::new(&mut self.flex_input).password());
            _ = f.ui(|ui64| {
                FieldDescription::show(ui64, "Use at least 8 characters with numbers and symbols.");
            });
        });

        super::code::snippet(
            ui,
            "FieldDescription::show(ui, \"Use at least 8 characters.\");",
        );
    }

    pub(crate) fn demo_property_grid(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A label/value grid for inspectors and settings.").show(ui);
        ui.add_space(12.0);
        _ = PropertyGrid::new()
            .label_width(96.0)
            .row_gap(4.0)
            .show(ui, |ui34| {
                _ = PropertyRow::new("X").show(ui34, |ui35| {
                    _ = ui35.add(
                        NumberInput::new(&mut self.prop_x)
                            .range(-500.0..=500.0)
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Y").show(ui34, |ui36| {
                    _ = ui36.add(
                        NumberInput::new(&mut self.prop_y)
                            .range(-500.0..=500.0)
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Width").show(ui34, |ui37| {
                    _ = ui37.add(
                        NumberInput::new(&mut self.prop_width)
                            .range(0.0..=2000.0)
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Height").show(ui34, |ui38| {
                    _ = ui38.add(
                        NumberInput::new(&mut self.prop_height)
                            .range(0.0..=2000.0)
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Rotation").show(ui34, |ui39| {
                    _ = ui39.add(
                        NumberInput::new(&mut self.prop_rotation)
                            .range(-180.0..=180.0)
                            .suffix(" deg")
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Opacity").show(ui34, |ui40| {
                    _ = ui40.add(
                        NumberInput::new(&mut self.prop_opacity)
                            .range(0.0..=100.0)
                            .suffix("%")
                            .width(110.0),
                    );
                });
            });

        super::code::snippet(
            ui,
            "PropertyGrid::new().label_width(96.0).show(ui, |grid| {\n    PropertyRow::new(\"X\").show(grid, |row| { ... });\n});",
        );
    }

    pub(crate) fn demo_property_row(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A single labeled row: text, badges, or inputs.").show(ui);
        ui.add_space(12.0);
        _ = PropertyGrid::new().label_width(96.0).show(ui, |ui41| {
            _ = PropertyRow::new("Mode").show(ui41, |ui42| {
                _ = Badge::new("Auto")
                    .variant(functora_egui::BadgeVariant::Secondary)
                    .show(ui42);
            });
            _ = PropertyRow::new("Blend").show(ui41, |ui43| {
                let blend_modes = vec![
                    "Normal".to_owned(),
                    "Multiply".to_owned(),
                    "Screen".to_owned(),
                    "Overlay".to_owned(),
                ];
                _ = ui43.add(functora_egui::SelectValue::new(
                    &mut self.select_blend,
                    &blend_modes,
                ));
            });
            _ = PropertyRow::new("Visible").show(ui41, |ui44| {
                _ = ui44.add(functora_egui::Switch::new(&mut self.form.form_billing).label("Show"));
            });
            _ = PropertyRow::new("Actions").show(ui41, |ui45| {
                _ = Flex::row().gap(8.0).show(ui45, |f| {
                    _ = f.add(
                        Button::new("Reset")
                            .variant(ButtonVariant::Outline)
                            .size(functora_egui::ComponentSize::Sm),
                    );
                    _ = f.add(
                        Button::new("Apply")
                            .size(functora_egui::ComponentSize::Sm)
                            .icon(functora_egui::LucideIcon::Check),
                    );
                });
            });
        });

        snippet(ui, "PropertyRow::new(\"Mode\").show(grid, |row| { ... });");
    }
}
