use crate::catalog::{Month, Year};
use functora_egui::snippet;
use functora_egui::{FieldGroup, Flex, Input, Label, ResponsiveExt, SelectLabeled, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_field_group(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Groups related fields with a legend and description.").show(ui);
        ui.add_space(12.0);
        _ = FieldGroup::show(ui, |ui27| {
            _ = Label::new("Card number").show(ui27);
            ui27.add_space(8.0);
            _ = Input::new(&mut self.form.form_card)
                .placeholder("4242 4242 4242 4242")
                .show(ui27);
            ui27.add_space(8.0);
            _ = Label::new("Expiry").show(ui27);
            ui27.add_space(8.0);
            if ui27.on_mobile() {
                let months = Month::ALL.map(|month| (month, month.label()));
                _ = SelectLabeled::new(&mut self.form.form_month, &months)
                    .placeholder("Month")
                    .show(ui27);
                ui27.add_space(8.0);
                let years = Year::ALL.map(|year| (year, year.label()));
                _ = SelectLabeled::new(&mut self.form.form_year, &years)
                    .placeholder("Year")
                    .show(ui27);
            } else {
                let half = (ui27.available_width() - 8.0) / 2.0;
                _ = Flex::row().gap(8.0).show(ui27, |f3| {
                    let months = Month::ALL.map(|month| (month, month.label()));
                    _ = f3.add(
                        SelectLabeled::new(&mut self.form.form_month, &months)
                            .placeholder("Month")
                            .width(half),
                    );
                    let years = Year::ALL.map(|year| (year, year.label()));
                    _ = f3.add(
                        SelectLabeled::new(&mut self.form.form_year, &years)
                            .placeholder("Year")
                            .width(half),
                    );
                });
            }
            ui27.add_space(8.0);
            _ = Label::new("CVV").show(ui27);
            ui27.add_space(8.0);
            _ = Input::new(&mut self.form.form_cvv)
                .placeholder("123")
                .show(ui27);
        });

        snippet(
            ui,
            "// FieldGroup: groups related fields in a card container\nuse functora_egui::{FieldGroup, Label, Input, SelectLabeled};\n\n#[derive(Debug, Clone, Copy, PartialEq, Eq)]\nstruct Month(u8);\n\nimpl Month {\n    const ALL: [Self; 12] = [Self(1), Self(2), Self(3), Self(4), Self(5), Self(6), Self(7), Self(8), Self(9), Self(10), Self(11), Self(12)];\n    fn label(self) -> String { format!(\"{:02}\", self.0) }\n}\n\n#[derive(Debug, Clone, Copy, PartialEq, Eq)]\nstruct Year(i32);\n\nimpl Year {\n    const ALL: [Self; 5] = [Self(2026), Self(2027), Self(2028), Self(2029), Self(2030)];\n    fn label(self) -> String { self.0.to_string() }\n}\n\nFieldGroup::show(ui, |group| {\n    Label::new(\"Card number\").show(group);\n    Input::new(&mut card).placeholder(\"4242 4242 4242 4242\").show(group);\n    Label::new(\"Expiry\").show(group);\n    let months = Month::ALL.map(|month| (month, month.label()));\n    let mut month: Option<Month> = None;\n    SelectLabeled::new(&mut month, &months).placeholder(\"Month\").show(group);\n    let years = Year::ALL.map(|year| (year, year.label()));\n    let mut year: Option<Year> = None;\n    SelectLabeled::new(&mut year, &years).placeholder(\"Year\").show(group);\n    Label::new(\"CVV\").show(group);\n    Input::new(&mut cvv).placeholder(\"123\").show(group);\n});",
        );
    }
}
