//! Widget trait implementation for `RadioGroup`.

impl<T: Clone + PartialEq + std::fmt::Display> egui::Widget for super::widget::RadioGroup<'_, T> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let entries: Vec<(T, String)> = self
            .options
            .iter()
            .map(|option| (option.clone(), option.to_string()))
            .collect();
        super::widget::RadioGroupLabeled::new(self.selected, &entries).show(ui)
    }
}

impl<T: Clone + PartialEq> egui::Widget for super::widget::RadioGroupLabeled<'_, T> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let mut any_clicked = false;

        let response = ui.vertical(|inner_ui| {
            for (value, label) in self.entries {
                let is_selected = *value == *self.selected;
                let mut sel = is_selected;
                let r = inner_ui
                    .add(crate::widgets::radio::widget::Radio::new(&mut sel).label(label.clone()));
                if r.clicked() && !is_selected {
                    *self.selected = value.clone();
                    any_clicked = true;
                }
            }
        });

        if any_clicked {
            ui.ctx().request_repaint();
        }

        response.response
    }
}
