use functora_egui::{
    Badge, BlockingOverlay, Button, ButtonVariant, Card, Flex, Input, Label, Progress,
    ResponsiveExt, Separator, ShadcnThemeExt, Switch, Textarea, Typography,
};
use std::sync::mpsc;

#[cfg(target_arch = "wasm32")]
fn spawn_async<F, T>(future: F) -> mpsc::Receiver<T>
where
    F: std::future::Future<Output = T> + 'static,
    T: 'static,
{
    let (tx, rx) = mpsc::channel();
    wasm_bindgen_futures::spawn_local(async move {
        let res = future.await;
        drop(tx.send(res));
    });
    rx
}

#[cfg(not(target_arch = "wasm32"))]
fn spawn_async<F, T>(future: F) -> mpsc::Receiver<T>
where
    F: std::future::Future<Output = T> + Send + 'static,
    T: Send + 'static,
{
    let (tx, rx) = mpsc::channel();
    drop(std::thread::spawn(move || {
        let res = pollster::block_on(future);
        drop(tx.send(res));
    }));
    rx
}

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
            || self.platform.print_rx.is_some()
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
            let taken = rx.lock().ok().and_then(|mut guard| guard.take());
            if let Some(res) = taken {
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
                        if e == "Cancelled" || e.contains("cancelled") || e.contains("Cancelled") {
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
            } else {
                self.platform.pick_rx = Some(rx);
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
        if let Some(rx) = self.platform.print_rx.take() {
            match rx.try_recv() {
                Ok(Ok(())) => "Print triggered".clone_into(&mut self.platform.print_status),
                Ok(Err(e)) => self.platform.print_status = format!("Print failed: {e}"),
                Err(mpsc::TryRecvError::Empty) => self.platform.print_rx = Some(rx),
                Err(_) => "Print disconnected".clone_into(&mut self.platform.print_status),
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
            || self.platform.print_rx.is_some()
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
            "functora_egui::storage::persist_value(&key, &val);\nlet loaded: Option<String> = functora_egui::storage::load_state(&key);\nfunctora_egui::storage::files_dir()",
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
            "functora_egui::clipboard::write(text).await?;\nlet text = functora_egui::clipboard::read().await?;",
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
            "functora_egui::share::share(ShareData { title, text, url }).await?;",
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
            "functora_egui::deep_link::store_url(url);\nlet url = functora_egui::deep_link::take_url();\nlet route = functora_egui::deep_link::url_to_route(&url);",
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
                #[cfg(target_arch = "wasm32")]
                {
                    let rx = std::sync::Arc::new(std::sync::Mutex::new(None));
                    let rx_clone = std::sync::Arc::clone(&rx);
                    let progress_clone = std::sync::Arc::clone(&progress);
                    let cancel_clone = std::sync::Arc::clone(&cancel);
                    wasm_bindgen_futures::spawn_local(async move {
                        let res = functora_egui::files::pick_files_with_shared_progress(
                            true,
                            Some(progress_clone),
                            Some(&cancel_clone),
                        )
                        .await
                        .map_err(|e| e.to_string());
                        if let Ok(mut guard) = rx_clone.lock() {
                            *guard = Some(res);
                        }
                    });
                    self.platform.pick_rx = Some(rx);
                }
                #[cfg(not(target_arch = "wasm32"))]
                {
                    let rx = std::sync::Arc::new(std::sync::Mutex::new(None));
                    let rx_clone = std::sync::Arc::clone(&rx);
                    let progress_clone = std::sync::Arc::clone(&progress);
                    let cancel_clone = std::sync::Arc::clone(&cancel);
                    drop(std::thread::spawn(move || {
                        let res = pollster::block_on(
                            functora_egui::files::pick_files_with_shared_progress(
                                true,
                                Some(progress_clone),
                                Some(&cancel_clone),
                            ),
                        )
                        .map_err(|e| e.to_string());
                        if let Ok(mut guard) = rx_clone.lock() {
                            *guard = Some(res);
                        }
                    }));
                    self.platform.pick_rx = Some(rx);
                }
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
            "let files = functora_egui::files::pick_files(true).await?;\nlet preview = functora_egui::files::preview(name, &data);\nlet mime = functora_egui::files::mime_for_name(name);",
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
            "functora_egui::download::download(data, &filename).await?;",
        );
    }

    pub(crate) fn demo_print(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Print via `window.print()` (web), PrintManager (Android), stub on desktop.",
        )
        .show(ui);
        ui.add_space(12.0);
        let printing = self.platform.print_rx.is_some();
        if ui
            .add_enabled(
                !printing,
                Button::new(if printing {
                    "Printing..."
                } else {
                    "Print page"
                })
                .icon(functora_egui::LucideIcon::Printer),
            )
            .clicked()
        {
            self.platform.print_rx = Some(spawn_async(async move {
                functora_egui::print::print_page()
                    .await
                    .map_err(|e| e.to_string())
            }));
        }
        if !self.platform.print_status.is_empty() {
            ui.add_space(8.0);
            _ = ui.add(Badge::new(&self.platform.print_status));
        }
        ui.add_space(8.0);
        _ = Typography::small("On desktop this will show 'Print not supported' - expected.")
            .show(ui);

        snippet(ui, "false");
    }

    pub(crate) fn demo_nav(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "NavStack<R>: push/go_back/can_go_back/has_navigated, platform-agnostic (replaces dioxus Router).",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(format!(
            "Current: {} (idx {})",
            self.platform.nav.current(),
            self.platform.nav.idx()
        ))
        .show(ui);
        _ = Typography::small(format!(
            "Can go back: {}, Has navigated: {}",
            self.platform.nav.can_go_back(),
            self.platform.nav.has_navigated()
        ))
        .show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            for route in [
                crate::app::DemoRoute::Home,
                crate::app::DemoRoute::Profile,
                crate::app::DemoRoute::Settings,
                crate::app::DemoRoute::About,
            ] {
                let label = route.to_string();
                if f.add(
                    Button::new(&label)
                        .variant(ButtonVariant::Outline)
                        .size(functora_egui::ComponentSize::Sm),
                )
                .inner
                .clicked()
                {
                    self.platform.nav.push(route);
                }
            }
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Go back").icon(functora_egui::LucideIcon::ArrowLeft))
                .inner
                .clicked()
            {
                _ = self.platform.nav.go_back();
            }
            if f.add(Button::new("Reset").variant(ButtonVariant::Ghost))
                .inner
                .clicked()
            {
                self.platform.nav.reset();
            }
        });
        ui.add_space(8.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small("Go via string route").show(ui2);
            ui2.add_space(4.0);
            _ = Flex::row().gap(8.0).show(ui2, |f| {
                _ = f.add(Input::new(&mut self.platform.nav_input).placeholder("/about"));
                if f.add(Button::new("Push route").size(functora_egui::ComponentSize::Sm))
                    .inner
                    .clicked()
                {
                    self.platform
                        .nav
                        .push_route(&self.platform.nav_input.clone());
                }
            });
        });
        ui.add_space(8.0);
        _ = Typography::small(format!("Stack: {:?}", self.platform.nav.stack())).show(ui);

        snippet(
            ui,
            "nav.push(route);\nnav.go_back();\nlet route = nav.current();",
        );
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
            "functora_egui::progress::claim_job(&mut slot, Stage::Zip);",
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
            "let hint = functora_egui::camera::install_hint().await?;\nlet res = functora_egui::camera::trigger_pwa_install().await?;",
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
            "let encoded = functora_egui::encoding::encode_payload(&payload)?;\nlet decoded = functora_egui::encoding::decode_payload::<T>(&encoded)?;\nlet svg = functora_egui::encoding::generate_qr_code(&text)?;",
        );
    }

    pub(crate) fn demo_in_flight(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("InFlight guard: prevents concurrent async actions (share/print/pick), auto-releases on drop.").show(ui);
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
            "if let Some(_guard) = self.platform.in_flight.claim() {\n    // exclusive access for the duration of the guard\n}",
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
            "functora_egui::camera::check_camera().await?;\nfunctora_egui::camera::start_camera().await?;\nlet frame = functora_egui::camera::capture_frame().await?;\nfunctora_egui::camera::stop_camera().await?;",
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
            "// Stateful widget with auto-start + live preview + auto-scan.\nQrScanner::new().continuous(true).on_scan(|text| { ... })\n    .show(ui, &mut qr_state);",
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
            "let thumb = functora_egui::thumbnail::video_thumbnail(bytes);",
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
            "let zip = functora_egui::zip::create_zip_async(&files, progress, stage).await?;\nlet files = functora_egui::zip::unzip_async(bytes, progress, stage).await?;",
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
            "let enc = functora_egui::crypto::encrypt_symmetric(data, password)?;\nlet plain = functora_egui::crypto::decrypt_symmetric(&enc, password)?;",
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
            "let result = functora_egui::worker::run(input, |job| { ... }, |val, reporter| async {\n    reporter(Job { stage, done, total, name });\n    Ok(output)\n}).await?;",
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
