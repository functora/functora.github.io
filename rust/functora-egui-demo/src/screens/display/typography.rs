use functora_egui::snippet;
use functora_egui::{Typography, TypographyVariant};

impl crate::state::ShowcaseApp {
    pub fn demo_typography(ui: &mut egui::Ui) {
        _ = Typography::muted("Text styles: headings, lead, muted, and small.").show(ui);
        ui.add_space(12.0);
        _ = Typography::h1("The Joke Tax Chronicles").show(ui);
        ui.add_space(4.0);
        _ = Typography::new(
            "Once upon a time, in a far-off land, there was a very lazy king who spent all day \
             lounging on his throne. One day, his advisors came to him with a problem.",
        )
        .show(ui);
        ui.add_space(8.0);
        _ = Typography::h2("The King's Plan").show(ui);
        ui.add_space(4.0);
        _ = Typography::new(
            "The king thought long and hard, and finally came up with a brilliant plan.",
        )
        .show(ui);
        ui.add_space(8.0);
        _ = Typography::h3("The Joke").show(ui);
        ui.add_space(4.0);
        _ = Typography::new("Why did the chicken cross the road? To get to the other side.")
            .show(ui);
        ui.add_space(8.0);
        _ = Typography::h4("People stopped telling jokes").show(ui);
        ui.add_space(4.0);
        _ = Typography::small("The moral of the story is: this is a typography demo.").show(ui);
        ui.add_space(8.0);
        _ = Typography::lead("This is a lead paragraph: slightly larger and muted.").show(ui);
        ui.add_space(8.0);
        _ = Typography::muted("Muted text is dimmer for secondary content.").show(ui);
        ui.add_space(8.0);
        _ = Typography::new("Plain paragraph style with a custom variant.")
            .variant(TypographyVariant::Large)
            .show(ui);

        snippet(
            ui,
            "// Typography: styled text with variants\nuse functora_egui::{Typography, TypographyVariant};\n\nTypography::h1(\"The Joke Tax Chronicles\").show(ui);\nTypography::h2(\"The King's Plan\").show(ui);\nTypography::h3(\"The Joke\").show(ui);\nTypography::h4(\"People stopped telling jokes\").show(ui);\nTypography::small(\"The moral of the story is: this is a typography demo.\").show(ui);\nTypography::lead(\"This is a lead paragraph: slightly larger and muted.\").show(ui);\nTypography::muted(\"Muted text is dimmer for secondary content.\").show(ui);\n\n// Custom variant\nTypography::new(\"Plain paragraph style with a custom variant.\")\n    .variant(TypographyVariant::Large)\n    .show(ui);",
        );
    }
}
