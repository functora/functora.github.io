use functora_egui::{snippet, snippet_break_long_words};

impl crate::state::ShowcaseApp {
    pub fn demo_code_snippet(ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Themed monospace block for displaying code examples.");
        crate::screens::section(ui, "snippet(): normal wrapping");
        snippet(ui, "fn main() {\n    println!(\"Hello, world!\");\n}");
        ui.add_space(12.0);
        crate::screens::section(
            ui,
            "snippet_break_long_words(): breaks long unbroken strings",
        );
        snippet_break_long_words(
            ui,
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
        );

        snippet(
            ui,
            "// CodeSnippet: themed code blocks\nuse functora_egui::{snippet, snippet_break_long_words};\n\n// Normal wrapping\nsnippet(ui, \"fn main() {\\n    println!(\\\"Hello, world!\\\");\\n}\");\n\n// Break long words (base64, minified JSON)\nsnippet_break_long_words(ui, \"data:image/png;base64,iVBORw0KGgo...\");",
        );
    }
}
