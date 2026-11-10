use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Flex, Input, ToastVariant, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_deep_link(&mut self, ui: &mut egui::Ui) {
        if let Some(url) = functora_egui::deep_link::poll_deep_link() {
            self.platform.deep_link_current = url;
        }
        #[cfg(target_arch = "wasm32")]
        {
            if let Some(href) = functora_egui::platform::web::location_href()
                && self.platform.deep_link_current.is_empty()
            {
                self.platform.deep_link_current = href;
            }
        }
        _ = Typography::muted(
            "Deep linking: `store_url`/`take_url`/`poll_deep_link` + `url_to_route` + `set_schedule_update`/`trigger_update`. On Android via JNI intent, on web via location href.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(format!(
            "Current polled: {}",
            self.platform.deep_link_current
        ))
        .show(ui);
        ui.add_space(8.0);
        _ = ui.add(
            Input::new(&mut self.platform.deep_link_input)
                .placeholder("https://example.com/?page=about")
                .desired_width(ui.available_width()),
        );
        ui.add_space(8.0);
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Store URL").icon(functora_egui::LucideIcon::Link))
                .inner
                .clicked()
            {
                functora_egui::deep_link::store_url(self.platform.deep_link_input.clone());
                self.toast
                    .add("Stored", ToastVariant::Success, ctx.input(|i| i.time));
            }
            if f.add(Button::new("Take").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                let taken = functora_egui::deep_link::take_url();
                self.toast.add(
                    format!("Take: {taken:?}"),
                    ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            }
            if f.add(Button::new("Poll").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                let polled = functora_egui::deep_link::poll_deep_link();
                self.toast.add(
                    format!("Poll: {polled:?}"),
                    ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            }
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("url_to_route")).inner.clicked() {
                let route = functora_egui::deep_link::url_to_route(&self.platform.deep_link_input);
                self.toast.add(
                    format!("Route: {route:?}"),
                    ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            }
            if f.add(Button::new("Schedule update").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                let repaint = ctx.clone();
                functora_egui::deep_link::set_schedule_update(std::sync::Arc::new(move || {
                    repaint.request_repaint();
                }));
                self.toast.add(
                    "Scheduled update registered",
                    ToastVariant::Success,
                    ctx.input(|i| i.time),
                );
            }
            if f.add(Button::new("Trigger update").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                functora_egui::deep_link::trigger_update();
                self.toast.add(
                    "Update triggered",
                    ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            }
        });
        #[cfg(target_arch = "wasm32")]
        {
            ui.add_space(8.0);
            if let Some(hash) = functora_egui::platform::web::location_hash() {
                _ = Typography::small(format!("location.hash: {hash}")).show(ui);
            }
            if let Some(href) = functora_egui::platform::web::location_href() {
                _ = Typography::small(format!("location.href: {href}")).show(ui);
            }
            _ = Typography::small(format!(
                "poll_deep_link_web(): {:?}",
                functora_egui::deep_link::poll_deep_link_web()
            ))
            .show(ui);
        }

        snippet(
            ui,
            "// Deep links: store + take + route parsing\nuse functora_egui::deep_link::{store_url, take_url, url_to_route, set_schedule_update, trigger_update};\nuse std::sync::Arc;\n\n// Store a URL (e.g. from push notification)\nlet url = \"https://myapp.com/?page=settings&tab=notifications\".to_owned();\nstore_url(url);\n\n// Later, take it back (returns None when nothing is pending)\nlet url = take_url();\n\n// Parse the query part into an in-app route\nlet route = url.as_deref().and_then(url_to_route);\n// route = Some(\"/?page=settings&tab=notifications\")\n\n// Ask the platform to schedule a repaint whenever a URL arrives\nlet ctx = ctx.clone();\nset_schedule_update(Arc::new(move || ctx.request_repaint()));\n\n// ...or trigger the scheduled callback right away\ntrigger_update();\n\n// Web only: read the deep link straight from the location bar\n#[cfg(target_arch = \"wasm32\")]\nlet web_link = functora_egui::deep_link::poll_deep_link_web();",
        );
    }
}
