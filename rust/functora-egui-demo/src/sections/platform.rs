use crate::route::AppRoute;
use functora_egui::{
    Badge, BlockingOverlay, Button, ButtonVariant, Card, Flex, Input, Label, Progress,
    ResponsiveExt, Separator, ShadcnThemeExt, Switch, Textarea, Typography, spawn_async,
};
use std::sync::mpsc;

use functora_egui::snippet;

impl crate::app::ShowcaseApp {
    pub(crate) fn poll_platform_promises(&mut self, ctx: &egui::Context) {
        if let Some(shared) = self.platform.pick_progress.clone()
            && let Ok(guard) = shared.lock()
        {
            self.platform.pick_job.clone_from(&guard);
        }
        if self.platform.clipboard_rx.is_some()
            || self.platform.clipboard_write_rx.is_some()
            || self.platform.share_rx.is_some()
            || self.platform.pick_rx.is_some()
            || self.platform.download_rx.is_some()
            || self.platform.pwa_rx.is_some()
            || self.platform.camera_rx.is_some()
            || self.platform.qr_rx.is_some()
            || self.platform.thumbnail_rx.is_some()
            || self.platform.zip_rx.is_some()
            || self.platform.worker_rx.is_some()
        {
            ctx.request_repaint();
        }
        if let Some(rx) = self.platform.clipboard_rx.take() {
            match rx.try_recv() {
                Ok(Ok(text)) => {
                    self.platform.clipboard_read = text;
                    "Read ok".clone_into(&mut self.platform.clipboard_status);
                }
                Ok(Err(e)) => self.platform.clipboard_status = format!("Read failed: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.clipboard_rx = Some(rx),
                Err(mpsc::TryRecvError::Disconnected) => {
                    "Read disconnected".clone_into(&mut self.platform.clipboard_status);
                }
            }
        }
        if let Some(rx) = self.platform.clipboard_write_rx.take() {
            match rx.try_recv() {
                Ok(Ok(())) => "Copy ok".clone_into(&mut self.platform.clipboard_status),
                Ok(Err(e)) => self.platform.clipboard_status = format!("Copy failed: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.clipboard_write_rx = Some(rx),
                Err(_) => "Copy disconnected".clone_into(&mut self.platform.clipboard_status),
            }
        }
        if let Some(rx) = self.platform.share_rx.take() {
            match rx.try_recv() {
                Ok(Ok(())) => "Shared ok".clone_into(&mut self.platform.share_status),
                Ok(Err(e)) => self.platform.share_status = format!("Share failed: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.share_rx = Some(rx),
                Err(_) => "Share disconnected".clone_into(&mut self.platform.share_status),
            }
        }
        if let Some(rx) = self.platform.pick_rx.take() {
            match rx.try_recv() {
                Ok(res) => {
                    match res {
                        Ok(files) => {
                            let incoming = files.len();
                            for (name, data) in files {
                                if let Some(pos) =
                                    self.platform.picked.iter().position(|(n, _)| n == &name)
                                {
                                    drop(self.platform.picked.remove(pos));
                                }
                                self.platform.picked.push((name, data));
                            }
                            self.platform.pick_status =
                                format!("Picked {} file(s) total", self.platform.picked.len());
                            if incoming > 1 {
                                self.platform.pick_status = format!(
                                    "Picked {} file(s) ({} new)",
                                    self.platform.picked.len(),
                                    incoming
                                );
                            }
                        }
                        Err(e) => {
                            if e == "Cancelled"
                                || e.contains("cancelled")
                                || e.contains("Cancelled")
                            {
                                self.platform.pick_status = "Pick cancelled".to_string();
                            } else {
                                self.platform.pick_status = format!("Pick failed: {e}");
                            }
                        }
                    }
                    self.platform.pick_cancel = None;
                    self.platform.pick_overlay_open = false;
                    self.platform.pick_job = None;
                    self.platform.pick_progress = None;
                }
                Err(mpsc::TryRecvError::Empty) => {
                    self.platform.pick_rx = Some(rx);
                }
                Err(_) => {
                    self.platform.pick_cancel = None;
                    self.platform.pick_overlay_open = false;
                    self.platform.pick_job = None;
                    self.platform.pick_progress = None;
                }
            }
        }
        if self.platform.pick_rx.is_none() {
            self.platform.pick_overlay_open = false;
        }
        if let Some(rx) = self.platform.download_rx.take() {
            match rx.try_recv() {
                Ok(Ok(name)) => self.platform.download_status = format!("Downloaded {name}"),
                Ok(Err(e)) => self.platform.download_status = format!("Download failed: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.download_rx = Some(rx),
                Err(_) => "Download disconnected".clone_into(&mut self.platform.download_status),
            }
        }
        if let Some(rx) = self.platform.pwa_rx.take() {
            match rx.try_recv() {
                Ok(Ok(msg)) => self.platform.pwa_status = msg,
                Ok(Err(e)) => self.platform.pwa_status = format!("PWA error: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.pwa_rx = Some(rx),
                Err(_) => "PWA disconnected".clone_into(&mut self.platform.pwa_status),
            }
        }
        if let Some(rx) = self.platform.camera_rx.take() {
            match rx.try_recv() {
                Ok(Ok(msg)) => self.platform.camera_status = msg,
                Ok(Err(e)) => self.platform.camera_status = format!("Camera error: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.camera_rx = Some(rx),
                Err(_) => "Camera disconnected".clone_into(&mut self.platform.camera_status),
            }
        }
        if let Some(rx) = self.platform.qr_rx.take() {
            match rx.try_recv() {
                Ok(Ok(msg)) => self.platform.qr_status = msg,
                Ok(Err(e)) => self.platform.qr_status = format!("QR error: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.qr_rx = Some(rx),
                Err(_) => "QR disconnected".clone_into(&mut self.platform.qr_status),
            }
        }
        if let Some(rx) = self.platform.thumbnail_rx.take() {
            match rx.try_recv() {
                Ok(Ok(msg)) => self.platform.thumbnail_status = msg,
                Ok(Err(e)) => self.platform.thumbnail_status = format!("Thumbnail error: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.thumbnail_rx = Some(rx),
                Err(_) => "Thumbnail disconnected".clone_into(&mut self.platform.thumbnail_status),
            }
        }
        if let Some(rx) = self.platform.zip_rx.take() {
            match rx.try_recv() {
                Ok(Ok(msg)) => self.platform.zip_status = msg,
                Ok(Err(e)) => self.platform.zip_status = format!("Zip error: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.zip_rx = Some(rx),
                Err(_) => "Zip disconnected".clone_into(&mut self.platform.zip_status),
            }
        }
        if let Some(rx) = self.platform.worker_rx.take() {
            match rx.try_recv() {
                Ok(Ok(msg)) => self.platform.worker_status = msg,
                Ok(Err(e)) => self.platform.worker_status = format!("Worker error: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.worker_rx = Some(rx),
                Err(_) => "Worker disconnected".clone_into(&mut self.platform.worker_status),
            }
        }
        if self.platform.clipboard_rx.is_some()
            || self.platform.clipboard_write_rx.is_some()
            || self.platform.share_rx.is_some()
            || self.platform.pick_rx.is_some()
            || self.platform.download_rx.is_some()
            || self.platform.pwa_rx.is_some()
            || self.platform.camera_rx.is_some()
            || self.platform.qr_rx.is_some()
            || self.platform.thumbnail_rx.is_some()
            || self.platform.zip_rx.is_some()
            || self.platform.worker_rx.is_some()
        {
            ctx.request_repaint();
        }
    }

    pub(crate) fn demo_storage(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Unified persistent storage: localStorage on web, storage.json via ProjectDirs on desktop, MediaStore dir on Android. Single API `load_state`/`persist_value`.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Label::new("Key").show(ui);
        _ = ui.add(Input::new(&mut self.platform.storage_key).placeholder("demo_key"));
        ui.add_space(4.0);
        _ = Label::new("Value").show(ui);
        _ = ui.add(Input::new(&mut self.platform.storage_value).placeholder("hello"));
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Save").icon(functora_egui::LucideIcon::Save))
                .inner
                .clicked()
            {
                let key = self.platform.storage_key.clone();
                let val = self.platform.storage_value.clone();
                functora_egui::storage::persist_value(&key, &val);
                self.platform.storage_status = format!("Saved {key} = {val}");
            }
            if f.add(Button::new("Load").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                let key = self.platform.storage_key.clone();
                let loaded: Option<String> = functora_egui::storage::load_state(&key);
                self.platform.storage_status = match loaded {
                    Some(v) => {
                        self.platform.storage_value.clone_from(&v);
                        format!("Loaded {key} = {v}")
                    }
                    None => format!("No value for {key}"),
                };
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                functora_egui::storage::persist_value(&self.platform.storage_key, &String::new());
                "Cleared (set to empty)".clone_into(&mut self.platform.storage_status);
            }
        });
        if !self.platform.storage_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.storage_status));
        }
        ui.add_space(12.0);
        let _ = Separator::horizontal().show(ui);
        ui.add_space(8.0);
        _ = Typography::small("Persistent wrapper (auto-load via `Persistent::new`)").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(
                Input::new(&mut self.platform.storage_persistent_text).placeholder("persistent"),
            );
            if f.add(Button::new("Persist").size(functora_egui::ComponentSize::Sm))
                .inner
                .clicked()
            {
                functora_egui::storage::persist_value(
                    "demo_persistent",
                    &self.platform.storage_persistent_text,
                );
                "Persistent saved".clone_into(&mut self.platform.storage_status);
            }
        });
        ui.add_space(4.0);
        if let Some(v) = functora_egui::storage::load_state::<String>("demo_persistent") {
            _ = Typography::small(format!("Stored persistent: {v}")).show(ui);
        }
        ui.add_space(4.0);
        match functora_egui::storage::files_dir() {
            Ok(p) => _ = Typography::small(format!("files_dir: {}", p.display())).show(ui),
            Err(e) => _ = Typography::small(format!("files_dir error: {e}")).show(ui),
        }

        snippet(
            ui,
            "// Storage: persist + load + files_dir\nuse functora_egui::storage::{persist_value, load_state, files_dir};\n\nlet key = \"my_key\";\nlet val = \"hello world\";\npersist_value(key, val);\nlet loaded: Option<String> = load_state(key);\nlet dir = files_dir()?;\neprintln!(\"files dir: {}\", dir.display());",
        );
    }

    pub(crate) fn demo_clipboard(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Clipboard read/write via arboard (desktop), navigator.clipboard (web), ClipboardManager (Android).",
        )
        .show(ui);
        ui.add_space(12.0);
        let w = ui.available_width();
        _ = Flex::column().gap(8.0).show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Label::new("Write to clipboard").show(ui2);
            });
            _ = f.add(Input::new(&mut self.platform.clipboard_write).placeholder("text to copy"));
            _ = f.ui(|ui2| {
                _ = Flex::row().gap(8.0).show(ui2, |f2| {
                    let writing = self.platform.clipboard_write_rx.is_some();
                    if f2
                        .add(
                            Button::new(if writing { "Copying..." } else { "Copy" })
                                .icon(functora_egui::LucideIcon::Copy)
                                .enabled(!writing),
                        )
                        .inner
                        .clicked()
                    {
                        let text = self.platform.clipboard_write.clone();
                        self.platform.clipboard_write_rx = Some(spawn_async(async move {
                            functora_egui::clipboard::write(text)
                                .await
                                .map_err(|e| e.to_string())
                        }));
                    }
                    let reading = self.platform.clipboard_rx.is_some();
                    if f2
                        .add(
                            Button::new(if reading { "Reading..." } else { "Paste" })
                                .variant(ButtonVariant::Outline)
                                .icon(functora_egui::LucideIcon::ClipboardPaste)
                                .enabled(!reading),
                        )
                        .inner
                        .clicked()
                    {
                        self.platform.clipboard_rx = Some(spawn_async(async move {
                            functora_egui::clipboard::read()
                                .await
                                .map_err(|e| e.to_string())
                        }));
                    }
                });
            });
            if !self.platform.clipboard_status.is_empty() {
                _ = f.ui(|ui2| {
                    _ = ui2.add(Badge::new(&self.platform.clipboard_status));
                });
            }
            _ = f.ui(|ui2| {
                _ = Label::new("Last pasted").show(ui2);
            });
            _ = f.add(
                Textarea::new(&mut self.platform.clipboard_read)
                    .placeholder("pasted text appears here")
                    .desired_width(w),
            );
        });

        snippet(
            ui,
            "// Clipboard: write + read\nuse functora_egui::clipboard::{write, read};\n\n// Write\nlet text = \"hello clipboard\";\nwrite(text).await?;\n\n// Read\nlet text = read().await?;\neprintln!(\"pasted: {text}\");",
        );
    }

    pub(crate) fn demo_share(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Social share via navigator.share (web), Intent.createChooser (Android), clipboard fallback (desktop).",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Flex::column().gap(8.0).show(ui, |f| {
            _ = f.add(Input::new(&mut self.platform.share_title).placeholder("Title"));
            _ = f.add(Input::new(&mut self.platform.share_text).placeholder("Text"));
            _ = f.add(Input::new(&mut self.platform.share_url).placeholder("https://example.com"));
            let sharing = self.platform.share_rx.is_some();
            if f.add(
                Button::new(if sharing { "Sharing..." } else { "Share" })
                    .icon(functora_egui::LucideIcon::Share2)
                    .enabled(!sharing),
            )
            .inner
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
            if !self.platform.share_status.is_empty() {
                _ = f.ui(|ui2| {
                    _ = ui2.add(Badge::new(&self.platform.share_status));
                });
            }
        });

        snippet(
            ui,
            "// Share: title + text + url\nuse functora_egui::share::{share, ShareData};\n\nlet data = ShareData {\n    title: \"My App\".to_owned(),\n    text: \"Check this out!\".to_owned(),\n    url: \"https://example.com\".to_owned(),\n};\nshare(data).await?;",
        );
    }

    pub(crate) fn demo_deep_link(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
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
            "Deep linking: `store_url`/`take_url`/`poll_deep_link` + `url_to_route`. On Android via JNI intent, on web via location href.",
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
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Store URL").icon(functora_egui::LucideIcon::Link))
                .inner
                .clicked()
            {
                functora_egui::deep_link::store_url(self.platform.deep_link_input.clone());
                "Stored".clone_into(&mut self.platform.deep_link_output);
            }
            if f.add(Button::new("Take").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                let taken = functora_egui::deep_link::take_url();
                self.platform.deep_link_output = format!("Take: {taken:?}");
            }
            if f.add(Button::new("Poll").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                let polled = functora_egui::deep_link::poll_deep_link();
                self.platform.deep_link_output = format!("Poll: {polled:?}");
            }
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("url_to_route").size(functora_egui::ComponentSize::Sm))
                .inner
                .clicked()
            {
                let route = functora_egui::deep_link::url_to_route(&self.platform.deep_link_input);
                self.platform.deep_link_output = format!("Route: {route:?}");
            }
        });
        if !self.platform.deep_link_output.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.deep_link_output));
        }
        #[cfg(target_arch = "wasm32")]
        {
            ui.add_space(8.0);
            if let Some(hash) = functora_egui::platform::web::location_hash() {
                _ = Typography::small(format!("location.hash: {hash}")).show(ui);
            }
            if let Some(href) = functora_egui::platform::web::location_href() {
                _ = Typography::small(format!("location.href: {href}")).show(ui);
            }
        }

        snippet(
            ui,
            "// Deep links: store + take + route parsing\nuse functora_egui::deep_link::{store_url, take_url, url_to_route};\n\n// Store a URL (e.g. from push notification)\nlet url = \"https://myapp.com/?page=settings&tab=notifications\";\nstore_url(url);\n\n// Later, take and parse it\nlet url = take_url();\nlet route = url_to_route(&url);\n// route = Route { path: \"/settings\", query: {\"tab\": \"notifications\"} }",
        );
    }

    pub(crate) fn demo_files(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        if let Some(cancel) = self.platform.pick_cancel.clone() {
            let mut open = self.platform.pick_overlay_open;
            BlockingOverlay::new("Uploading...")
                .description("Reading files, please wait. You can cancel if needed.")
                .show(
                    ui.ctx(),
                    &mut open,
                    self.platform.pick_job.as_ref(),
                    &cancel,
                );
            self.platform.pick_overlay_open = open;
        }
        _ = Typography::muted(
            "Files: `pick_files` via rfd (desktop) / Intent (Android) / input (web). Preview via `preview`/`preview_blob`, mime via `mime_for`.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let picking = self.platform.pick_rx.is_some();
            if f.add(
                Button::new(if picking { "Picking..." } else { "Pick files" })
                    .icon(functora_egui::LucideIcon::Files)
                    .enabled(!picking),
            )
            .inner
            .clicked()
            {
                let cancel = functora_egui::files::new_cancel_token();
                let progress = std::sync::Arc::new(std::sync::Mutex::new(None));
                self.platform.pick_cancel = Some(std::sync::Arc::clone(&cancel));
                self.platform.pick_progress = Some(std::sync::Arc::clone(&progress));
                self.platform.pick_overlay_open = true;
                self.platform.pick_job = None;
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::files::pick_files_with_shared_progress(
                        true,
                        Some(progress),
                        Some(&cancel),
                    )
                    .await
                    .map_err(|e| e.to_string())
                });
                self.platform.pick_rx = Some(rx);
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.picked.clear();
                self.platform.pick_status.clear();
            }
        });
        if !self.platform.pick_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.pick_status));
        }
        if self.platform.picked.is_empty() {
            ui.add_space(8.0);
            _ = Typography::small("No files picked yet.").show(ui);
        } else {
            ui.add_space(12.0);
            _ = Typography::small(format!("{} file(s) picked", self.platform.picked.len()))
                .show(ui);
            ui.add_space(8.0);
            for (name, data) in &self.platform.picked {
                let preview = functora_egui::files::preview(name, data);
                let mime = functora_egui::files::mime_for_name(name).unwrap_or("unknown");
                let size = functora_egui::files::format_size(data.len() as u64);
                _ = Card::new().show(ui, |ui2| {
                    _ = Flex::column().gap(4.0).show(ui2, |f| {
                        _ = f.ui(|ui3| {
                            _ = Typography::small(format!("{name} ({mime}, {size})")).show(ui3);
                        });
                        _ = f.ui(|ui3| match preview {
                            functora_egui::files::Preview::Text(ref t) => {
                                _ = Label::new(t.chars().take(200).collect::<String>()).show(ui3);
                            }
                            functora_egui::files::Preview::Markdown(ref t) => {
                                _ = Label::new(format!("MD: {}", &t[..t.len().min(200)])).show(ui3);
                            }
                            functora_egui::files::Preview::Image(_) => {
                                let source = format!("bytes://{name}");
                                _ = ui3.add(
                                    egui::Image::from_bytes(source, data.clone())
                                        .max_width(220.0)
                                        .max_height(220.0)
                                        .corner_radius(8),
                                );
                                _ = Typography::small(format!("Image: {name} ({size})")).show(ui3);
                            }
                            functora_egui::files::Preview::Video(ref url) => {
                                _ = Typography::small(format!(
                                    "Video: {}...",
                                    &url[..url.len().min(60)]
                                ))
                                .show(ui3);
                                _ = Typography::small(format!("Video file: {name} ({size})"))
                                    .show(ui3);
                            }
                            functora_egui::files::Preview::Download => {
                                _ = ui3.add(Badge::new("Download"));
                                _ = Typography::small(format!("Ready to download: {name}"))
                                    .show(ui3);
                            }
                            _ => {
                                _ = Typography::small(format!("{preview:?}")).show(ui3);
                            }
                        });
                    });
                });
                ui.add_space(8.0);
            }
        }
        ui.add_space(12.0);
        let _ = Separator::horizontal().show(ui);
        ui.add_space(8.0);
        _ = Typography::small("Blob memo cache demo").show(ui);
        ui.add_space(4.0);
        if ui
            .add(Button::new("Create revokable blob (txt)").size(functora_egui::ComponentSize::Sm))
            .clicked()
        {
            let preview = functora_egui::files::preview_blob("hello.txt", b"hello blob");
            self.platform.pick_status = format!("blob preview: {preview:?}");
        }

        snippet(
            ui,
            "// Files: pick + preview + mime\nuse functora_egui::files::{pick_files, preview, preview_blob, preview_cached, mime_for_name, format_size, CancelToken};\nuse std::sync::Arc;\n\n// Pick files (multiple = true)\nlet cancel = Arc::new(std::sync::atomic::AtomicBool::new(false));\nlet files = pick_files(true).await?;\n\nfor (name, data) in files {\n    // Get mime type\n    let mime = mime_for_name(&name).unwrap_or(\"application/octet-stream\");\n    \n    // Preview (text/image/pdf)\n    let preview = preview(&name, &data);\n    \n    // Or create a revocable blob URL (web)\n    let blob_url = preview_blob(&name, &data);\n    \n    // Or cached preview (avoids re-decoding)\n    let cached = preview_cached(&name, &data);\n    \n    let size = format_size(data.len() as u64);\n    eprintln!(\"picked: {name} ({mime}, {size})\");\n    \n    // Cancel if needed\n    // cancel.store(true, Ordering::Relaxed);\n}",
        );
    }

    pub(crate) fn demo_download(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Download via Blob+anchor (web), rfd save dialog (desktop), MediaStore Downloads (Android).",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(Input::new(&mut self.platform.download_name).placeholder("hello.txt"));
        ui.add_space(4.0);
        _ = ui.add(
            Textarea::new(&mut self.platform.download_text)
                .placeholder("file contents")
                .desired_width(ui.available_width()),
        );
        ui.add_space(8.0);
        let downloading = self.platform.download_rx.is_some();
        if ui
            .add_enabled(
                !downloading,
                Button::new(if downloading {
                    "Downloading..."
                } else {
                    "Download"
                })
                .icon(functora_egui::LucideIcon::Download),
            )
            .clicked()
        {
            let name = self.platform.download_name.clone();
            let data = self.platform.download_text.clone().into_bytes();
            self.platform.download_rx = Some(spawn_async(async move {
                functora_egui::download::download(data, &name)
                    .await
                    .map_err(|e| e.to_string())
            }));
        }
        if !self.platform.download_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.download_status));
        }

        snippet(
            ui,
            "// Download: Blob + anchor (web) / save dialog (desktop) / MediaStore (Android)\nuse functora_egui::download::download;\n\nlet data = b\"hello, world!\";\nlet filename = \"hello.txt\";\n\n// Simple one-liner\ndownload(data, filename).await?;\n\n// Or with bytes:\n// download(data.to_vec(), filename).await?;",
        );
    }

    pub(crate) fn demo_nav(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "NavHistory<R> + AppRouter<R, S>: push/go_back/go_forward, integrates with browser history."
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(format!("Current route: {}", self.router.current())).show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Go back").icon(functora_egui::LucideIcon::ArrowLeft))
                .inner
                .clicked()
            {
                _ = self.router.go_back(&mut ());
            }
            if f.add(Button::new("Go forward").icon(functora_egui::LucideIcon::ArrowRight))
                .inner
                .clicked()
            {
                _ = self.router.go_forward(&mut ());
            }
            if f.add(Button::new("Navigate to Overview").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.router.navigate(&mut (), AppRoute::Overview);
            }
        });
        ui.add_space(8.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small("Example: NavHistory + AppRouter").show(ui2);
            ui2.add_space(4.0);
            snippet(
                ui2,
                "// NavHistory: push / go_back / go_forward / sync\nuse functora_egui::nav::NavHistory;\nuse functora_egui::route::{AppRouter, Routable};\n\nlet mut history = NavHistory::new(AppRoute::Overview);\n\n// Push a route\nhistory.push(AppRoute::Component(42));\nassert_eq!(history.current(), &AppRoute::Component(42));\n\n// Go back\nhistory.go_back();\nassert_eq!(history.current(), &AppRoute::Overview);\n\n// Check state\nhistory.can_go_back(); // false\nhistory.can_go_forward(); // true\n\n// AppRouter integrates with browser history\nlet mut router = AppRouter::new(&mut (), AppRoute::Overview);\nrouter.navigate(&mut (), AppRoute::Component(42));\nrouter.go_back(&mut ());",
            );
        });
    }

    pub(crate) fn demo_progress_worker(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Progress Job + Worker::run (thread on desktop, inline on wasm) with Stage enum.",
        )
        .show(ui);
        ui.add_space(12.0);
        if let Some(job) = &self.platform.progress_job {
            _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
            ui.add_space(4.0);
            _ = Typography::small(format!(
                "Stage: {:?} {} / {} ({}%)",
                job.stage,
                job.done,
                job.total,
                job.percent()
            ))
            .show(ui);
            if let Some(name) = &job.name {
                _ = Typography::small(format!("file: {name}")).show(ui);
            }
        } else {
            _ = Typography::small("No job running").show(ui);
        }
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let running = self.platform.progress_running;
            if f.add(
                Button::new("Start fake job")
                    .icon(functora_egui::LucideIcon::Play)
                    .enabled(!running),
            )
            .inner
            .clicked()
            {
                self.platform.progress_running = true;
                self.platform.progress_job = Some(functora_egui::progress::Job {
                    stage: functora_egui::progress::Stage::Zip,
                    done: 0,
                    total: 100,
                    name: None,
                });
            }
            if f.add(
                Button::new("Tick")
                    .variant(ButtonVariant::Outline)
                    .enabled(running),
            )
            .inner
            .clicked()
                && let Some(job) = &mut self.platform.progress_job
            {
                job.done = (job.done + 10).min(job.total);
                if job.done >= job.total {
                    self.platform.progress_running = false;
                }
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                self.platform.progress_job = None;
                self.platform.progress_running = false;
            }
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Claim guard demo").size(functora_egui::ComponentSize::Sm))
                .inner
                .clicked()
            {
                let mut slot = self.platform.progress_job.clone();
                let is_claimed = functora_egui::progress::claim_job(
                    &mut slot,
                    functora_egui::progress::Stage::Download,
                )
                .is_some();
                if is_claimed {
                    self.platform.progress_job = Some(functora_egui::progress::Job {
                        stage: functora_egui::progress::Stage::Download,
                        done: 0,
                        total: 1,
                        name: None,
                    });
                }
            }
        });

        snippet(
            ui,
            "// Progress: Job<Stage> + claim_job for exclusive access\nuse functora_egui::progress::{Job, Stage, claim_job, Progress};\n\nlet mut job = Job {\n    stage: Stage::Download,\n    done: 0,\n    total: 100,\n    name: Some(\"file.zip\".to_owned()),\n};\n\n// Update progress\njob.done = 50;\n\n// Claim for exclusive access (returns Some(guard) if available)\nif let Some(_guard) = claim_job(&mut job, Stage::Zip) {\n    // Exclusive access - do zip work\n    job.done = 100;\n}\n\n// Render progress bar\n// Progress::new(f32::from(job.percent()) / 100.0).show(ui);",
        );
    }

    pub(crate) fn demo_pwa(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "PWA: pwa_init_js, pwa_sw_js, trigger_pwa_install, install_hint. Manifest/theme_color derived from Cargo.toml.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small("Generated pwa_init_js:").show(ui2);
            ui2.add_space(4.0);
            _ = Label::new(functora_egui::pwa::pwa_init_js("/sw.js", "demo-v1")).show(ui2);
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let pending = self.platform.pwa_rx.is_some();
            if f.add(
                Button::new("Install hint")
                    .variant(ButtonVariant::Outline)
                    .enabled(!pending),
            )
            .inner
            .clicked()
            {
                self.platform.pwa_rx = Some(spawn_async(async move {
                    let hint = functora_egui::camera::install_hint()
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(format!("Hint: {hint:?}"))
                }));
            }
            if f.add(
                Button::new("Trigger PWA install")
                    .icon(functora_egui::LucideIcon::Download)
                    .enabled(!pending),
            )
            .inner
            .clicked()
            {
                self.platform.pwa_rx = Some(spawn_async(async move {
                    let res = functora_egui::camera::trigger_pwa_install()
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(format!("Install: {res:?}"))
                }));
            }
        });
        if !self.platform.pwa_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.pwa_status));
        }
        ui.add_space(8.0);
        _ = Typography::small(
            "On desktop this will be NotAvailable - expected. On web with beforeinstallprompt it may be Accepted/Rejected.",
        )
        .show(ui);

        snippet(
            ui,
            "// PWA: install_hint + trigger_pwa_install\nuse functora_egui::{camera::install_hint, camera::trigger_pwa_install};\n\n// Check if install is available\nlet hint = install_hint().await?;\nmatch hint {\n    functora_egui::camera::InstallHint::Available => {\n        // Show install button\n    }\n    functora_egui::camera::InstallHint::NotAvailable => {\n        // Hide install button\n    }\n    functora_egui::camera::InstallHint::Unknown => {}\n}\n\n// Trigger install prompt\nlet res = trigger_pwa_install().await?;\n// res = Accepted | Rejected | NotAvailable | AlreadyInstalled",
        );
    }

    pub(crate) fn demo_encoding(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Encoding: encode_payload/decode_payload (base64url JSON), append/extract_query_param, generate_qr_code (svg). Crypto via functora_core.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(
            Input::new(&mut self.platform.encode_input)
                .placeholder("text to encode")
                .desired_width(ui.available_width()),
        );
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Encode").icon(functora_egui::LucideIcon::Code))
                .inner
                .clicked()
            {
                #[derive(serde::Serialize)]
                struct Payload {
                    msg: String,
                }
                let v = Payload {
                    msg: self.platform.encode_input.clone(),
                };
                match functora_egui::encoding::encode_payload(&v) {
                    Ok(s) => self.platform.encode_output = s,
                    Err(e) => self.platform.encode_output = format!("encode err: {e}"),
                }
            }
            if f.add(Button::new("Decode").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                match functora_egui::encoding::decode_payload::<serde_json::Value>(
                    &self.platform.encode_output,
                ) {
                    Ok(v) => self.platform.encode_output = format!("decoded: {v}"),
                    Err(e) => self.platform.encode_output = format!("decode err: {e}"),
                }
            }
            if f.add(Button::new("QR SVG").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                match functora_egui::encoding::generate_qr_code(&self.platform.encode_input) {
                    Ok(svg) => {
                        self.platform.encode_output =
                            svg.chars().take(300).collect::<String>() + "...";
                    }
                    Err(e) => self.platform.encode_output = format!("qr err: {e}"),
                }
            }
        });
        if !self.platform.encode_output.is_empty() {
            ui.add_space(8.0);
            _ = Card::new().show(ui, |ui2| {
                _ = Typography::small(&self.platform.encode_output).show(ui2);
            });
        }
        ui.add_space(12.0);
        _ = Typography::small(format!(
            "append_query_param: {}",
            functora_egui::encoding::append_query_param("https://example.com", "k", "v")
        ))
        .show(ui);

        snippet(
            ui,
            "// Encoding: base64url JSON + query params + QR SVG\nuse functora_egui::encoding::{encode_payload, decode_payload, generate_qr_code, append_query_param};\nuse serde::{Serialize, Deserialize};\n\n#[derive(Serialize, Deserialize)]\nstruct Payload { msg: String }\n\nlet payload = Payload { msg: \"hello\".to_owned() };\n\n// Encode to base64url JSON\nlet encoded = encode_payload(&payload)?;\n// \"eyJtc2ciOiJoZWxsbyJ9\"\n\n// Decode back\nlet decoded: Payload = decode_payload(&encoded)?;\nassert_eq!(decoded.msg, \"hello\");\n\n// Generate QR code SVG\nlet svg = generate_qr_code(\"https://example.com\")?;\n\n// Append query param\nlet url = append_query_param(\"https://example.com\", \"k\", \"v\");\n// \"https://example.com?k=v\"",
        );
    }

    pub(crate) fn demo_in_flight(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("InFlight guard: prevents concurrent async actions (share/pick), auto-releases on drop.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small(format!(
            "In flight: {}",
            self.platform.in_flight.is_in_flight()
        ))
        .show(ui);
        ui.add_space(8.0);
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Try claim").icon(functora_egui::LucideIcon::ShieldCheck))
                .inner
                .clicked()
            {
                if let Some(_guard) = self.platform.in_flight.claim() {
                    "Claimed! holding for 2s...".clone_into(&mut self.platform.in_flight_status);
                    #[cfg(target_arch = "wasm32")]
                    {
                        let flag = self.platform.in_flight.clone();
                        let ctx2 = ctx.clone();
                        wasm_bindgen_futures::spawn_local(async move {
                            gloo_timers::future::TimeoutFuture::new(2000).await;
                            drop(flag);
                            ctx2.request_repaint();
                        });
                    }
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        let flag = self.platform.in_flight.clone();
                        let ctx2 = ctx.clone();
                        drop(std::thread::spawn(move || {
                            std::thread::sleep(std::time::Duration::from_secs(2));
                            drop(flag);
                            ctx2.request_repaint();
                        }));
                    }
                } else {
                    "Already in flight - rejected".clone_into(&mut self.platform.in_flight_status);
                }
            }
            if f.add(Button::new("Reset").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.in_flight_status.clear();
            }
        });
        if !self.platform.in_flight_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.in_flight_status));
        }

        snippet(
            ui,
            "// InFlight: prevents concurrent async actions\nuse functora_egui::in_flight::InFlight;\n\nlet in_flight = InFlight::new();\n\n// Try to claim exclusive access\nif let Some(_guard) = in_flight.claim() {\n    // Exclusive access granted\n    // Do async work (share/pick/download)...\n    // Guard auto-releases on drop\n} else {\n    // Already in flight - reject or queue\n    eprintln!(\"Action already in progress\");\n}\n\n// Check status\nin_flight.is_in_flight();",
        );
    }

    pub(crate) fn demo_camera(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Camera: check_camera/start_camera/capture_frame/stop_camera + begin/stop session. Web via getUserMedia/canvas, Android via Camera2 (stub), desktop via file-picker fallback.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Check").icon(functora_egui::LucideIcon::Camera))
                .inner
                .clicked()
            {
                self.platform.camera_rx = Some(spawn_async(async move {
                    functora_egui::camera::check_camera()
                        .await
                        .map(|()| "Camera available".to_string())
                        .map_err(|e| e.to_string())
                }));
            }
            if f.add(Button::new("Start").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.camera_rx = Some(spawn_async(async move {
                    functora_egui::camera::start_camera()
                        .await
                        .map(|()| "Camera started".to_string())
                        .map_err(|e| e.to_string())
                }));
            }
            if f.add(Button::new("Capture").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.camera_rx = Some(spawn_async(async move {
                    let frame = functora_egui::camera::capture_frame()
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(format!(
                        "Frame {}x{} luma {} bytes",
                        frame.width,
                        frame.height,
                        frame.data.len()
                    ))
                }));
            }
            if f.add(Button::new("Stop").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                self.platform.camera_rx = Some(spawn_async(async move {
                    functora_egui::camera::stop_camera()
                        .await
                        .map(|()| "Camera stopped".to_string())
                        .map_err(|e| e.to_string())
                }));
            }
        });
        if !self.platform.camera_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.camera_status));
        }
        ui.add_space(8.0);
        _ = Typography::small("On desktop this will report 'not available – use file picker' (expected). On web, use QrScanner below for live preview.").show(ui);

        snippet(
            ui,
            "// Camera: check + start + capture + stop\nuse functora_egui::camera::{check_camera, start_camera, capture_frame, stop_camera};\n\n// Check if camera is available\ncheck_camera().await?;\n\n// Start camera session\nstart_camera().await?;\n\n// Capture a frame\nlet frame = capture_frame().await?;\n// frame: CameraFrame { width, height, data: Vec<u8> (RGBA) }\neprintln!(\"captured {}x{}\", frame.width, frame.height);\n\n// Stop camera\nstop_camera().await?;",
        );
    }

    pub(crate) fn demo_qr_scanner(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "QrScanner widget: stateful live preview (TextureHandle) + decode_qr_luma/rgba (rxing). Web live via canvas, Android Camera2, desktop file-picker fallback. Opt-in features `camera` + `qr`.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(Input::new(&mut self.platform.qr_input).placeholder("https://example.com"));
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Generate QR").icon(functora_egui::LucideIcon::QrCode))
                .inner
                .clicked()
            {
                let input = self.platform.qr_input.clone();
                self.platform.qr_rx = Some(spawn_async(async move {
                    if let Some((w, h, rgba)) = functora_egui::qr::qr_rgba(&input, 128) {
                        let _ = (w, h, rgba);
                        Ok(format!("QR generated {w}x{h}"))
                    } else {
                        Err("QR generation failed".to_string())
                    }
                }));
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                self.platform.qr_status.clear();
                self.platform.qr_state.clear_decoded();
                self.platform.qr_state.clear_error();
            }
        });
        if !self.platform.qr_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.qr_status));
        }
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(
                "Auto-starts and scans automatically (15 fps preview, 5 fps decode).",
            )
            .show(ui2);
            ui2.add_space(4.0);
            _ = Flex::row().gap(8.0).show(ui2, |f2| {
                _ = f2.add(Switch::new(&mut self.platform.qr_continuous).label("Continuous"));
            });
            ui2.add_space(4.0);
            if ui2
                .add(
                    Button::new("Restart scanner")
                        .variant(ButtonVariant::Outline)
                        .size(functora_egui::ComponentSize::Sm),
                )
                .clicked()
            {
                self.platform.qr_state.stop();
                self.platform.qr_state.clear_decoded();
                self.platform.qr_state.clear_error();
                let ctx = ui2.ctx().clone();
                let _ = self.platform.qr_state.start(&ctx);
            }
            ui2.add_space(8.0);
            let _ = functora_egui::QrScanner::new()
                .continuous(self.platform.qr_continuous)
                .on_scan(|text| log::info!("QR scanned: {text}"))
                .show(ui2, &mut self.platform.qr_state);
            if let Some(txt) = self.platform.qr_state.decoded() {
                ui2.add_space(8.0);
                _ = ui2.add(Badge::new(format!("Decoded: {txt}")));
            }
            if let Some(err) = self.platform.qr_state.error() {
                ui2.add_space(8.0);
                _ = ui2.label(
                    egui::RichText::new(format!("Error: {err}"))
                        .color(ui2.ctx().shadcn_theme().destructive)
                        .size(12.0),
                );
            }
        });
        ui.add_space(8.0);
        _ = Typography::small("Tip: Use Pick Image inside the scanner for file fallback (desktop) or Start Camera for live (web/android).").show(ui);

        snippet(
            ui,
            "// QrScanner: stateful live preview + auto-scan\nuse functora_egui::{QrScanner, QrScannerState};\n\n// State (persist across frames)\nlet mut qr_state = QrScannerState::new();\n\n// Start scanner (call once or on button)\nqr_state.start(&ctx)?;\n\n// Render widget (call every frame)\nQrScanner::new()\n    .continuous(true)           // keep scanning after first decode\n    .on_scan(|text| {           // callback on decode\n        log::info!(\"QR: {}\", text);\n    })\n    .show(ui, &mut qr_state);\n\n// Check decoded text\nif let Some(text) = qr_state.decoded() {\n    eprintln!(\"Decoded: {text}\");\n}\n\n// Check error\nif let Some(err) = qr_state.error() {\n    eprintln!(\"Error: {err}\");\n}\n\n// Stop when done\nqr_state.stop();",
        );
    }

    pub(crate) fn demo_thumbnail(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Thumbnail: video_thumbnail (mp4→jpeg) + jpeg_data_url + cache. Web via canvas, native via mp4+rust_h264.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(
            Input::new(&mut self.platform.thumbnail_input)
                .placeholder("data:video/mp4;base64,... or data:image/..."),
        );
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Generate thumbnail").icon(functora_egui::LucideIcon::Image))
                .inner
                .clicked()
            {
                let url = self.platform.thumbnail_input.clone();
                self.platform.thumbnail_rx = Some(spawn_async(async move {
                    Ok(format!("Thumbnail placeholder for len {}", url.len()))
                }));
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                self.platform.thumbnail_status.clear();
                self.platform.thumbnail_texture = None;
            }
        });
        if !self.platform.thumbnail_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.thumbnail_status));
        }
        if let Some(tex) = &self.platform.thumbnail_texture {
            ui.add_space(8.0);
            let _ = ui.add(egui::Image::new((tex.id(), egui::vec2(220.0, 140.0))).corner_radius(8));
        }
        ui.add_space(8.0);
        _ = Typography::small(
            "Tip: pick a video file in Files demo, then paste its data URL here.",
        )
        .show(ui);

        snippet(
            ui,
            "// Thumbnail: video_thumbnail (mp4 -> jpeg)\nuse functora_egui::thumbnail::video_thumbnail;\n\n// Input: video bytes (mp4)\nlet video_bytes: Vec<u8> = ...;\n\n// Generate thumbnail\nlet jpeg = video_thumbnail(&video_bytes)?;\n// Returns JPEG bytes\n\n// Convert to data URL for display\nlet data_url = format!(\"data:image/jpeg;base64,{}\", base64::encode(&jpeg));\n// Use with egui::Image::new(data_url)",
        );
    }

    pub(crate) fn demo_zip(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Zip: create_zip_async / unzip_async via worker::run with progress Job<Stage>. Uses picked files from Files demo.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(format!(
            "Picked files for zip: {} (from Files)",
            self.platform.picked.len()
        ))
        .show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let busy = self.platform.zip_rx.is_some();
            if f.add(Button::new(if busy { "Zipping..." } else { "Create zip" }).enabled(!busy))
                .inner
                .clicked()
            {
                let count = self.platform.picked.len();
                if count == 0 {
                    self.platform.zip_status = "No files picked (go to Files)".to_string();
                } else {
                    self.platform.zip_status =
                        format!("Zip would include {count} files (demo placeholder)");
                }
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                self.platform.zip_status.clear();
            }
        });
        if !self.platform.zip_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.zip_status));
        }

        snippet(
            ui,
            "// Zip: create_zip_async + unzip_async with progress\nuse functora_egui::zip::{create_zip_async, unzip_async};\nuse functora_egui::progress::{Job, Stage};\n\n// Create zip from picked files\nlet files: Vec<(String, Vec<u8>)> = ...;\nlet mut job = Job { stage: Stage::Zip, done: 0, total: files.len(), name: None };\n\nlet zip_bytes = create_zip_async(&files, &mut job, Stage::Zip).await?;\n// zip_bytes: Vec<u8>\n\n// Unzip\nlet mut job = Job { stage: Stage::Unzip, done: 0, total: 0, name: None };\nlet files = unzip_async(&zip_bytes, &mut job, Stage::Unzip).await?;\n// files: Vec<(String, Vec<u8>)>",
        );
    }

    pub(crate) fn demo_crypto(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Crypto: functora_core::crypto encrypt_symmetric / decrypt_symmetric (ChaCha20Poly1305/AES-GCM) + KDF.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(Input::new(&mut self.platform.crypto_input).placeholder("plain text"));
        ui.add_space(4.0);
        _ = ui.add(Input::new(&mut self.platform.crypto_password).placeholder("password"));
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Encrypt").icon(functora_egui::LucideIcon::Lock))
                .inner
                .clicked()
            {
                let input = self.platform.crypto_input.clone();
                self.platform.crypto_output = format!("Encrypted placeholder for '{input}'");
                self.platform.crypto_status =
                    "Crypto demo placeholder (no real encrypt) ".to_string();
            }
            if f.add(Button::new("Decrypt").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.crypto_output = "Decrypted placeholder".to_string();
                self.platform.crypto_status = "Decrypt placeholder".to_string();
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                self.platform.crypto_output.clear();
                self.platform.crypto_status.clear();
            }
        });
        if !self.platform.crypto_output.is_empty() {
            ui.add_space(8.0);
            _ = Card::new().show(ui, |ui2| {
                _ = Typography::small(&self.platform.crypto_output).show(ui2);
            });
        }
        if !self.platform.crypto_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.crypto_status));
        }

        snippet(
            ui,
            "// Crypto: encrypt_symmetric / decrypt_symmetric (ChaCha20Poly1305)\nuse functora_egui::crypto::{encrypt_symmetric, decrypt_symmetric};\n\nlet data = b\"secret message\";\nlet password = \"my-password\";\n\n// Encrypt\nlet encrypted = encrypt_symmetric(data, password)?;\n// Returns Vec<u8> (nonce + ciphertext + tag)\n\n// Decrypt\nlet decrypted = decrypt_symmetric(&encrypted, password)?;\nassert_eq!(decrypted, data);",
        );
    }

    pub(crate) fn demo_worker(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Worker: worker::run – runs future on thread (desktop) or inline (wasm) with Reporter<Stage> progress.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let busy = self.platform.worker_rx.is_some();
            if f.add(Button::new(if busy { "Working..." } else { "Start worker" }).enabled(!busy))
                .inner
                .clicked()
            {
                self.platform.worker_rx = Some(spawn_async(async move {
                    functora_egui::worker::run(
                        42u32,
                        |_| {},
                        |val, mut reporter| async move {
                            reporter(functora_egui::progress::Job {
                                stage: functora_egui::progress::Stage::Download,
                                done: 1,
                                total: 1,
                                name: None,
                            });
                            Ok::<String, functora_egui::error::Error>(format!("Worker done: {val}"))
                        },
                    )
                    .await
                    .map_err(|e| e.to_string())
                }));
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                self.platform.worker_status.clear();
            }
        });
        if !self.platform.worker_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.worker_status));
        }
        ui.add_space(8.0);
        _ = Typography::small("Check ProgressWorker demo for Job<Stage> progress details.")
            .show(ui);
        if let Some(rx) = self.platform.worker_rx.take() {
            match rx.try_recv() {
                Ok(Ok(msg)) => self.platform.worker_status = msg,
                Ok(Err(e)) => self.platform.worker_status = format!("Worker err: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.worker_rx = Some(rx),
                Err(_) => self.platform.worker_status = "Worker disconnected".to_string(),
            }
        }

        snippet(
            ui,
            "// Worker: run async work on thread (desktop) or inline (wasm) with progress\nuse functora_egui::worker::run;\nuse functora_egui::progress::{Job, Stage};\n\nlet input = 42u32;\n\nlet result = run(\n    input,\n    |_job| { /* setup */ },\n    |val, mut reporter| async move {\n        // Report progress\n        reporter(Job {\n            stage: Stage::Download,\n            done: 1,\n            total: 1,\n            name: Some(\"task\".to_owned()),\n        });\n        \n        // Do async work\n        let output = format!(\"Worker done: {val}\");\n        Ok(output)\n    },\n).await?;\n\n// result: String",
        );
    }

    pub(crate) fn demo_platform_info(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Platform info: is_mobile_hint (web innerWidth), location_href/hash, storage files_dir, theme, breakpoint.",
        )
        .show(ui);
        ui.add_space(12.0);
        let is_mobile = ui.ctx().on_mobile();
        let spacing = ui.ctx().responsive_spacing();
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!("on_mobile: {is_mobile}")).show(ui2);
            _ = Typography::small(format!("breakpoint: {:?}", ui2.ctx().breakpoint())).show(ui2);
            _ = Typography::small(format!(
                "spacing content_max_width: {}",
                spacing.content_max_width
            ))
            .show(ui2);
            _ = Typography::small(format!("spacing page_padding: {}", spacing.page_padding))
                .show(ui2);
            _ = Typography::small(format!(
                "current_theme: {}",
                functora_egui::current_theme(ui2.ctx())
            ))
            .show(ui2);
            #[cfg(target_arch = "wasm32")]
            {
                if let Some(hint) = functora_egui::platform::web::is_mobile_hint() {
                    _ = Typography::small(format!("is_mobile_hint: {hint}")).show(ui2);
                }
                if let Some(href) = functora_egui::platform::web::location_href() {
                    _ = Typography::small(format!("location_href: {href}")).show(ui2);
                }
                if let Some(hash) = functora_egui::platform::web::location_hash() {
                    _ = Typography::small(format!("location_hash: {hash}")).show(ui2);
                }
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                _ = Typography::small("location_href/hash only on web").show(ui2);
            }
            match functora_egui::storage::files_dir() {
                Ok(p) => _ = Typography::small(format!("files_dir: {}", p.display())).show(ui2),
                Err(e) => _ = Typography::small(format!("files_dir err: {e}")).show(ui2),
            }
            if let Some(v) = functora_egui::storage::load_state::<String>("demo_persistent") {
                _ = Typography::small(format!("demo_persistent: {v}")).show(ui2);
            }
        });
        ui.add_space(8.0);
        _ = ui.add(Input::new(&mut self.platform.platform_info).placeholder("info note"));
        ui.add_space(4.0);
        if ui
            .add(Button::new("Save to platform_info").size(functora_egui::ComponentSize::Sm))
            .clicked()
        {
            functora_egui::storage::persist_value("platform_info", &self.platform.platform_info);
            self.platform.platform_info = "Saved".to_string();
        }
    }

    pub(crate) fn demo_messages(ui: &mut egui::Ui) {
        use functora_egui::i18n::I18N;
        _ = Typography::muted(
            "Messages / I18N: functora_core::messages + i18n Language (Eng/Spa/Rus). Error::render_* etc.",
        )
        .show(ui);
        ui.add_space(12.0);
        let err = functora_egui::error::Error::JS("demo error".into());
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!("EN: {}", err.render_eng())).show(ui2);
            _ = Typography::small(format!("SPA: {}", err.render_spa())).show(ui2);
            _ = Typography::small(format!("RU: {}", err.render_rus())).show(ui2);
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            for lang in [
                functora_egui::i18n::Language::Eng,
                functora_egui::i18n::Language::Spa,
                functora_egui::i18n::Language::Rus,
            ] {
                let _ = f.add(Badge::new(lang.to_string()));
            }
        });
    }

    pub(crate) fn demo_markdown(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Markdown: functora_core::markdown::render (pulldown-cmark + ammonia) → egui rich text.",
        )
        .show(ui);
        ui.add_space(12.0);
        let md = "# Hello\n\nThis is **bold** and *italic*.\n\n- item 1\n- item 2\n\n[link](https://example.com)";
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(md).show(ui2);
        });
        ui.add_space(8.0);
        _ = Typography::small(
            "Rendered via Label::new(markdown) – see actual app content for full render.",
        )
        .show(ui);
    }

    pub(crate) fn demo_package(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Package: FUNCTORA_CORE_DATE/YEAR + Cargo.toml metadata (theme_color, title) + build info.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "FUNCTORA_CORE_DATE: {}",
                functora_egui::FUNCTORA_CORE_DATE
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "FUNCTORA_CORE_YEAR: {}",
                functora_egui::FUNCTORA_CORE_YEAR
            ))
            .show(ui2);
            _ = Typography::small(format!(
                "FUNCTORA_CORE version: {}",
                env!("CARGO_PKG_VERSION")
            ))
            .show(ui2);
            _ = Typography::small("Package metadata via include_str! for theme_color etc.")
                .show(ui2);
        });
    }

    pub(crate) fn demo_white_label(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "WhiteLabel: functora_core::white_label – branding, theme overrides, per-app config.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!("white_label available: {}", true)).show(ui2);
            _ = Typography::small(
                "Configure via Cargo.toml [package.metadata.functora-egui-*] + WhiteLabel::load",
            )
            .show(ui2);
            _ = Typography::small("WhiteLabel: default (no custom branding)").show(ui2);
            _ = Typography::small(format!(
                "white_label donate_blocks: {:?}",
                functora_egui::white_label::donate_blocks().len()
            ))
            .show(ui2);
        });
    }
}
