use functora_egui::snippet;
use functora_egui::{Button, Flex, ToastVariant, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_in_flight(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("InFlight guard: prevents concurrent async actions (share/pick), auto-releases on drop.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small(format!(
            "In flight: {}",
            self.platform.in_flight.is_in_flight()
        ))
        .show(ui);
        ui.add_space(8.0);
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Try claim").icon(functora_egui::LucideIcon::ShieldCheck))
                .inner
                .clicked()
            {
                if self.try_claim_in_flight(&ctx) {
                    self.toast.add(
                        "Claimed! holding for 2s...",
                        ToastVariant::Success,
                        ctx.input(|i| i.time),
                    );
                } else {
                    self.toast.add(
                        "Already in flight - rejected",
                        ToastVariant::Error,
                        ctx.input(|i| i.time),
                    );
                }
            }
        });

        snippet(
            ui,
            "// InFlight: prevents concurrent async actions\nuse functora_egui::in_flight::InFlight;\n\nlet in_flight = InFlight::new();\n\n// Try to claim exclusive access\nif let Some(_guard) = in_flight.claim() {\n    // Exclusive access granted\n    // Do async work (share/pick/download)...\n    // Guard auto-releases on drop\n} else {\n    // Already in flight - reject or queue\n    eprintln!(\"Action already in progress\");\n}\n\n// Check status\nin_flight.is_in_flight();",
        );
    }
}
