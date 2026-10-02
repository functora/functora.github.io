use functora_egui::snippet;
use functora_egui::{LucideIcon, Navbar, ToastVariant, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_navbar(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Top navigation bar with brand, version, search, theme toggle, and language switcher.",
        )
        .show(ui);
        ui.add_space(12.0);
        let mut collapsed = self.demo.navbar_collapsed;
        let mut theme = self.persistent.theme;
        let language = self.navbar_language.clone();
        let time = ui.ctx().input(|i| i.time);
        let action = std::cell::Cell::new("");
        let mut on_brand = || {
            action.set("brand");
        };
        let mut on_search = || {
            action.set("search");
        };
        _ = Navbar::new("functora-egui")
            .brand_icon(Some(LucideIcon::Sparkles))
            .version("0.2")
            .search("Search...", Some("Ctrl K"))
            .show(
                ui,
                &mut collapsed,
                Some(&mut theme),
                Some(&language),
                Some(&mut on_brand),
                Some(&mut on_search),
            );
        self.demo.navbar_collapsed = collapsed;
        self.persistent.theme = theme;
        self.navbar_language = language;
        match action.get() {
            "brand" => {
                self.toast.add("Brand clicked", ToastVariant::Default, time);
            }
            "search" => {
                self.toast
                    .add("Search clicked", ToastVariant::Default, time);
            }
            _ => {}
        }

        snippet(
            ui,
            "// Navbar: top navigation bar\nuse functora_egui::{Navbar, LucideIcon};\nuse functora_egui::i18n::Language;\nuse functora_egui::theme_extra::Theme;\nuse std::cell::Cell;\n\nlet mut collapsed = false;\nlet mut theme = persistent.theme;\nlet language = Cell::new(Language::default());\nlet action = Cell::new(\"\");\nlet mut on_brand = || { action.set(\"brand\"); };\nlet mut on_search = || { action.set(\"search\"); };\n\nNavbar::new(\"functora-egui\")\n    .brand_icon(Some(LucideIcon::Sparkles))\n    .version(\"0.2\")\n    .search(\"Search...\", Some(\"Ctrl K\"))\n    .show(ui, &mut collapsed, Some(&mut theme), Some(&language), Some(&mut on_brand), Some(&mut on_search));\n\nmatch action.get() {\n    \"brand\" => toast.add(\"Brand clicked\", ToastVariant::Default, now),\n    \"search\" => toast.add(\"Search clicked\", ToastVariant::Default, now),\n    _ => {}\n}",
        );
    }
}
