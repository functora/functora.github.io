use functora_egui::snippet;
use functora_egui::{Flex, Skeleton, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_skeleton(ui: &mut egui::Ui) {
        _ = Typography::muted("Placeholder shimmering blocks while content loads.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(Skeleton::new(48.0, 48.0).circle());
            _ = f.grow_nested(1.0, Flex::column().gap(4.0).align_start(), |f2| {
                _ = f2.add(Skeleton::new(200.0, 16.0));
                _ = f2.add(Skeleton::new(180.0, 16.0));
                _ = f2.add(Skeleton::new(120.0, 16.0));
            });
        });

        snippet(
            ui,
            "// Skeleton: placeholder shimmering blocks\nuse functora_egui::{Skeleton, Flex};\n\nFlex::row().gap(8.0).align_center().show(ui, |f| {\n    f.add(Skeleton::new(48.0, 48.0).circle());\n    f.grow_nested(1.0, Flex::column().gap(4.0).align_start(), |f2| {\n        f2.add(Skeleton::new(200.0, 16.0));\n        f2.add(Skeleton::new(180.0, 16.0));\n        f2.add(Skeleton::new(120.0, 16.0));\n    });\n});",
        );
    }
}
