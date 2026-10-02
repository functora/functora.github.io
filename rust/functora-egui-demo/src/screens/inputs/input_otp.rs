use functora_egui::snippet;
use functora_egui::{InputOtp, Typography};

impl crate::state::ShowcaseApp {
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
}
