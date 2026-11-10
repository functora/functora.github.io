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
        crate::screens::section(ui, "Core JSON file helpers (functora_core::storage)");
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Write JSON").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                match Self::core_storage_demo() {
                    Ok(summary) => {
                        self.platform.storage_output.clone_from(&summary);
                        self.toast
                            .add(summary, ToastVariant::Success, ctx.input(|i| i.time));
                    }
                    Err(e) => self.toast.add(
                        format!("Core storage error: {e}"),
                        ToastVariant::Error,
                        ctx.input(|i| i.time),
                    ),
                }
            }
            if f.add(Button::new("Read JSON").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                match Self::core_storage_read_demo() {
                    Ok(summary) => {
                        self.platform.storage_output.clone_from(&summary);
                        self.toast
                            .add(summary, ToastVariant::Success, ctx.input(|i| i.time));
                    }
                    Err(e) => self.toast.add(
                        format!("Core storage error: {e}"),
                        ToastVariant::Error,
                        ctx.input(|i| i.time),
                    ),
                }
            }
        });
        ui.add_space(4.0);
        if !self.platform.storage_output.is_empty() {
            _ = Typography::small(&self.platform.storage_output).show(ui);
        }
        ui.add_space(12.0);
        crate::screens::section(ui, "Persistent wrapper (auto-load via `Persistent::new`)");
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
        ui.add_space(4.0);
        match functora_egui::files_dir_for("functora-egui-demo") {
            Ok(p) => _ = Typography::small(format!("files_dir_for: {}", p.display())).show(ui),
            Err(e) => _ = Typography::small(format!("files_dir_for error: {e}")).show(ui),
        }
        #[cfg(all(not(target_os = "android"), not(target_arch = "wasm32")))]
        {
            ui.add_space(4.0);
            _ = Typography::small(format!(
                "app_data_dir: {:?}",
                functora_egui::storage::app_data_dir("functora-egui-demo")
            ))
            .show(ui);
            _ = Typography::small(format!(
                "storage_file_for: {:?}",
                functora_egui::storage::storage_file_for("functora-egui-demo")
            ))
            .show(ui);
        }

        snippet(
            ui,
            "// Storage: persist + load + files_dir\nuse functora_egui::storage::Storage;\n\nlet storage = Storage::new(\"my-app\");\nlet key = \"my_key\";\nlet val = \"hello world\";\nstorage.persist(key, val);\nlet loaded: Option<String> = storage.load(key);\nmatch storage.files_dir() {\n    Ok(dir) => eprintln!(\"files dir: {}\", dir.display()),\n    Err(e) => eprintln!(\"files_dir error: {e}\"),\n}\n\n// The same directory without constructing a Storage\nuse functora_egui::files_dir_for;\n\nlet dir = files_dir_for(\"my-app\")?;\n\n// Desktop only: the data dir and the storage.json inside it\n#[cfg(all(not(target_os = \"android\"), not(target_arch = \"wasm32\")))]\n{\n    use functora_egui::storage::{app_data_dir, storage_file_for};\n\n    let data_dir = app_data_dir(\"my-app\")?;\n    let storage_file = storage_file_for(\"my-app\")?;\n    eprintln!(\"{} -> {}\", data_dir.display(), storage_file.display());\n}\n\n// Core JSON file helpers: one JSON object per file, serialized read-modify-write\nuse functora_egui::storage::{find_or_init_key, get_json_value, read_json_object, set_json_value, update_key, write_json_object};\nuse serde_json::json;\n\nlet path = std::path::Path::new(\"data/settings.json\");\n\n// Write a whole object\nwrite_json_object(path, &json!({\"theme\": \"dark\"}))?;\n\n// Read it back\nlet object = read_json_object(path)?;\nassert_eq!(object[\"theme\"], \"dark\");\n\n// Read and write single keys\nlet theme: Option<serde_json::Value> = get_json_value(path, \"theme\")?;\nset_json_value(path, \"locale\", \"en\")?;\nupdate_key(path, \"pinned\", true)?;\n\n// Read a typed value, creating it on first use\nlet pinned: bool = find_or_init_key(path, \"pinned\", || true)?;",
        );
    }
}
