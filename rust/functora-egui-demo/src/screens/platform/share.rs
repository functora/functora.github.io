use functora_egui::snippet;
use functora_egui::{Button, Input, Typography, spawn_async};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_share(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Social share via navigator.share (web), Intent.createChooser (Android), clipboard fallback (desktop).",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(Input::new(&mut self.platform.share_title).placeholder("Title"));
        ui.add_space(4.0);
        _ = ui.add(Input::new(&mut self.platform.share_text).placeholder("Text"));
        ui.add_space(4.0);
        _ = ui.add(Input::new(&mut self.platform.share_url).placeholder("https://example.com"));
        ui.add_space(8.0);
        let sharing = self.platform.share_rx.is_some();
        if ui
            .add_enabled(
                !sharing,
                Button::new(if sharing { "Sharing..." } else { "Share" })
                    .icon(functora_egui::LucideIcon::Share2),
            )
            .clicked()
        {
            let data = functora_egui::share::ShareData {
                title: self.platform.share_title.clone(),
                text: self.platform.share_text.clone(),
                url: self.platform.share_url.clone(),
            };
            self.platform.share_rx = Some(spawn_async(async move {
                functora_egui::share::share(data)
                    .await
                    .map_err(|e| e.to_string())
            }));
        }

        snippet(
            ui,
            "// Share: title + text + url (async, polled each frame)\nuse functora_egui::share::{share, ShareData};\nuse functora_egui::spawn_async;\n\nlet data = ShareData {\n    title: \"My App\".to_owned(),\n    text: \"Check this out!\".to_owned(),\n    url: \"https://example.com\".to_owned(),\n};\nlet rx = spawn_async(async move {\n    share(data).await.map_err(|e| e.to_string())\n});",
        );
    }
}
