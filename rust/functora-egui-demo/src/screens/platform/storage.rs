use functora_egui::snippet;
use functora_egui::{
    Button, ButtonVariant, Flex, Input, Label, Separator, ToastVariant, Typography,
};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_storage(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Unified persistent storage: localStorage on web, per-app storage.json via ProjectDirs on desktop, MediaStore dir on Android. Single API `Storage::load`/`Storage::persist`.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Label::new("Key").show(ui);
        ui.add_space(8.0);
        _ = ui.add(Input::new(&mut self.platform.storage_key).placeholder("demo_key"));
        ui.add_space(8.0);
        _ = Label::new("Value").show(ui);
        ui.add_space(8.0);
        _ = ui.add(Input::new(&mut self.platform.storage_value).placeholder("hello"));
        ui.add_space(8.0);
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Save").icon(functora_egui::LucideIcon::Save))
                .inner
                .clicked()
            {
                let key = self.platform.storage_key.clone();
                let val = self.platform.storage_value.clone();
                self.platform.storage.persist(&key, &val);
                self.toast.add(
                    format!("Saved {key} = {val}"),
                    ToastVariant::Success,
                    ctx.input(|i| i.time),
                );
            }
            if f.add(Button::new("Load").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                let key = self.platform.storage_key.clone();
                let loaded: Option<String> = self.platform.storage.load(&key);
                match loaded {
                    Some(v) => {
                        self.platform.storage_value.clone_from(&v);
                        self.toast.add(
                            format!("Loaded {key} = {v}"),
                            ToastVariant::Success,
                            ctx.input(|i| i.time),
                        );
                    }
                    None => self.toast.add(
                        format!("No value for {key}"),
                        ToastVariant::Default,
                        ctx.input(|i| i.time),
                    ),
                }
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform
                    .storage
                    .persist(&self.platform.storage_key, &String::new());
                self.toast.add(
                    "Cleared (set to empty)",
                    ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            }
        });
        ui.add_space(12.0);
        let _ = Separator::horizontal().show(ui);
        ui.add_space(8.0);
        _ = Typography::small("Persistent wrapper (auto-load via `Persistent::new`)").show(ui);
        ui.add_space(4.0);
        _ = ui
            .add(Input::new(&mut self.platform.storage_persistent_text).placeholder("persistent"));
        ui.add_space(4.0);
        if ui.add(Button::new("Persist")).clicked() {
            self.platform.storage.persist(
                "demo_persistent",
                &self.platform.storage_persistent_text.clone(),
            );
            self.toast.add(
                "Persistent saved",
                ToastVariant::Success,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(4.0);
        if let Some(v) = self.platform.storage.load::<String>("demo_persistent") {
            _ = Typography::small(format!("Stored persistent: {v}")).show(ui);
        }
        ui.add_space(4.0);
        match self.platform.storage.files_dir() {
            Ok(p) => _ = Typography::small(format!("files_dir: {}", p.display())).show(ui),
            Err(e) => _ = Typography::small(format!("files_dir error: {e}")).show(ui),
        }

        snippet(
            ui,
            "// Storage: persist + load + files_dir\nuse functora_egui::storage::Storage;\n\nlet storage = Storage::new(\"my-app\");\nlet key = \"my_key\";\nlet val = \"hello world\";\nstorage.persist(key, val);\nlet loaded: Option<String> = storage.load(key);\nmatch storage.files_dir() {\n    Ok(dir) => eprintln!(\"files dir: {}\", dir.display()),\n    Err(e) => eprintln!(\"files_dir error: {e}\"),\n}",
        );
    }
}
