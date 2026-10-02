use functora_egui::snippet;
use functora_egui::{Avatar, Flex, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_avatar(ui: &mut egui::Ui) {
        _ = Typography::muted("Initials-based avatars with adjustable sizes.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(16.0).align_center().show(ui, |f| {
            _ = f.add(Avatar::new("AL").size(24.0));
            _ = f.add(Avatar::new("CM").size(32.0));
            _ = f.add(Avatar::new("DA").size(40.0));
            _ = f.add(Avatar::new("FN").size(56.0));
        });
        ui.add_space(8.0);
        _ = Typography::small("Colors come from the theme's primary palette.").show(ui);

        snippet(
            ui,
            "// Avatar: initials-based avatar with adjustable sizes\nuse functora_egui::{Avatar, Flex};\n\nFlex::row().gap(16.0).align_center().show(ui, |f| {\n    f.add(Avatar::new(\"AL\").size(24.0));\n    f.add(Avatar::new(\"CM\").size(32.0));\n    f.add(Avatar::new(\"DA\").size(40.0));\n    f.add(Avatar::new(\"FN\").size(56.0));\n});",
        );
    }
}
