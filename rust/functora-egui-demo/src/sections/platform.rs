use crate::route::AppRoute;
use functora_egui::{
    Badge, BlockingOverlay, Button, ButtonVariant, Card, Flex, Input, Label, Progress,
    ResponsiveExt, Separator, ShadcnThemeExt, Switch, Textarea, ToastVariant, Typography,
    spawn_async,
};
use std::sync::mpsc;

use base64::Engine as _;
use functora_egui::ToastState;
use functora_egui::snippet;

fn poll_ok<T>(
    slot: &mut Option<mpsc::Receiver<Result<T, String>>>,
    toast: &mut ToastState,
    now: f64,
    err_prefix: &str,
    disconnected: &str,
) -> Option<T> {
    let rx = slot.take()?;
    match rx.try_recv() {
        Ok(Ok(value)) => Some(value),
        Ok(Err(error)) => {
            toast.add(format!("{err_prefix}: {error}"), ToastVariant::Error, now);
            None
        }
        Err(mpsc::TryRecvError::Empty) => {
            *slot = Some(rx);
            None
        }
        Err(mpsc::TryRecvError::Disconnected) => {
            toast.add(disconnected, ToastVariant::Error, now);
            None
        }
    }
}

impl crate::app::ShowcaseApp {
    fn has_pending_promises(&self) -> bool {
        self.platform.clipboard_rx.is_some()
            || self.platform.clipboard_write_rx.is_some()
            || self.platform.share_rx.is_some()
            || self.platform.pick_rx.is_some()
            || self.platform.download_rx.is_some()
            || self.platform.pwa_rx.is_some()
            || self.platform.camera_rx.is_some()
            || self.platform.qr_rx.is_some()
            || self.platform.thumbnail_rx.is_some()
            || self.platform.zip_rx.is_some()
            || self.platform.crypto_rx.is_some()
            || self.platform.worker_rx.is_some()
    }

    /// Builds a `(uri, jpeg bytes)` thumbnail pair from a data URL via the
    /// real `files::video_thumbnail` (mp4 decode + cache). Pure and sync so
    /// tests can exercise it; the demo runs it inside `spawn_async`. The uri
    /// keeps a `.jpg` extension so the image loader routes correctly.
    pub fn make_thumbnail(url: &str) -> Result<(String, Vec<u8>), String> {
        let data_url = functora_egui::files::video_thumbnail(url).ok_or_else(|| {
            "No thumbnail available: input is not a supported mp4 data URL".to_owned()
        })?;
        let payload = data_url.split_once(',').map_or("", |(_, rest)| rest);
        let jpeg = base64::engine::general_purpose::STANDARD
            .decode(payload)
            .map_err(|_| "Thumbnail decode failed".to_owned())?;
        if jpeg.is_empty() {
            return Err("Thumbnail decode failed".to_owned());
        }
        Ok(("bytes://thumb.jpg".to_owned(), jpeg))
    }

    /// Checks an unzipped listing against the original files by name and
    /// bytes. Pure so tests can exercise it without running the worker.
    pub fn verify_zip_roundtrip(
        original: &[(String, Vec<u8>)],
        unzipped: &[(String, Vec<u8>)],
    ) -> Result<String, String> {
        if original.len() != unzipped.len() {
            return Err(format!(
                "Zip verify failed: expected {} files, got {}",
                original.len(),
                unzipped.len()
            ));
        }
        for (name, data) in original {
            match unzipped.iter().find(|(back_name, _)| back_name == name) {
                Some((_, back_data)) if back_data == data => {}
                Some(_) => {
                    return Err(format!("Zip verify failed: content mismatch for {name}"));
                }
                None => return Err(format!("Zip verify failed: missing {name}")),
            }
        }
        let total: usize = original.iter().map(|(_, data)| data.len()).sum();
        Ok(format!(
            "Zip ok: {} files, {total} bytes, round-trip verified",
            original.len()
        ))
    }

    /// Encrypts `input` with `password` (`ChaCha20Poly1305` + Argon2id) and
    /// returns the note as JSON for the output card. Runs inside
    /// `spawn_async` in the demo because key derivation blocks.
    pub fn encrypt_output(input: &str, password: &str) -> Result<String, String> {
        functora_egui::crypto::encrypt_symmetric(
            input.as_bytes(),
            password,
            functora_egui::crypto::CipherType::ChaCha20Poly1305,
            &[],
        )
        .map_err(|e| e.to_string())
        .and_then(|note| serde_json::to_string(&note).map_err(|e| e.to_string()))
    }

    /// Parses an `encrypt_output` JSON note and decrypts it with `password`.
    pub fn decrypt_output(json: &str, password: &str) -> Result<String, String> {
        serde_json::from_str::<functora_egui::crypto::EncryptedNote>(json)
            .map_err(|e| format!("Not an encrypted note: {e}"))
            .and_then(|note| {
                functora_egui::crypto::decrypt_symmetric(&note, password, &[])
                    .map_err(|e| e.to_string())
            })
            .and_then(|bytes| {
                String::from_utf8(bytes).map_err(|e| format!("Decrypted bytes are not text: {e}"))
            })
    }

    async fn zip_roundtrip_async(files: Vec<(String, Vec<u8>)>) -> Result<String, String> {
        let attachments = files
            .iter()
            .map(|(name, data)| functora_egui::files::Attachment {
                name: name.clone(),
                data: std::sync::Arc::from(data.clone()),
            })
            .collect::<Vec<_>>();
        let zipped = functora_egui::zip::create_zip_async(
            &attachments,
            |_| {},
            functora_egui::progress::Stage::Zip,
        )
        .await
        .map_err(|e| e.to_string())?;
        let unzipped =
            functora_egui::zip::unzip_async(zipped, |_| {}, functora_egui::progress::Stage::Unzip)
                .await
                .map_err(|e| e.to_string())?;
        Self::verify_zip_roundtrip(&files, &unzipped)
    }

    pub fn poll_platform_promises(&mut self, ctx: &egui::Context) {
        if let Some(shared) = self.platform.pick_progress.clone()
            && let Ok(guard) = shared.lock()
        {
            self.platform.pick_job.clone_from(&guard);
        }
        let now = ctx.input(|i| i.time);
        if self.has_pending_promises() {
            ctx.request_repaint();
        }
        if let Some(text) = poll_ok(
            &mut self.platform.clipboard_rx,
            &mut self.toast,
            now,
            "Read failed",
            "Read disconnected",
        ) {
            self.platform.clipboard_read = text;
            self.toast.add("Read ok", ToastVariant::Success, now);
        }
        if let Some(()) = poll_ok(
            &mut self.platform.clipboard_write_rx,
            &mut self.toast,
            now,
            "Copy failed",
            "Copy disconnected",
        ) {
            self.toast.add("Copy ok", ToastVariant::Success, now);
        }
        if let Some(()) = poll_ok(
            &mut self.platform.share_rx,
            &mut self.toast,
            now,
            "Share failed",
            "Share disconnected",
        ) {
            self.toast.add("Shared ok", ToastVariant::Success, now);
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
                            let total = self.platform.picked.len();
                            self.toast.add(
                                if incoming > 1 {
                                    format!("Picked {total} file(s) ({incoming} new)")
                                } else {
                                    format!("Picked {total} file(s) total")
                                },
                                ToastVariant::Success,
                                now,
                            );
                        }
                        Err(e) => {
                            if e == "Cancelled"
                                || e.contains("cancelled")
                                || e.contains("Cancelled")
                            {
                                self.toast.add("Pick cancelled", ToastVariant::Default, now);
                            } else {
                                self.toast.add(
                                    format!("Pick failed: {e}"),
                                    ToastVariant::Error,
                                    now,
                                );
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
        if let Some(name) = poll_ok(
            &mut self.platform.download_rx,
            &mut self.toast,
            now,
            "Download failed",
            "Download disconnected",
        ) {
            self.toast
                .add(format!("Downloaded {name}"), ToastVariant::Success, now);
        }
        if let Some(msg) = poll_ok(
            &mut self.platform.pwa_rx,
            &mut self.toast,
            now,
            "PWA error",
            "PWA disconnected",
        ) {
            self.toast.add(msg, ToastVariant::Success, now);
        }
        if let Some(msg) = poll_ok(
            &mut self.platform.camera_rx,
            &mut self.toast,
            now,
            "Camera error",
            "Camera disconnected",
        ) {
            self.toast.add(msg, ToastVariant::Success, now);
        }
        if let Some(msg) = poll_ok(
            &mut self.platform.qr_rx,
            &mut self.toast,
            now,
            "QR error",
            "QR disconnected",
        ) {
            self.toast.add(msg, ToastVariant::Success, now);
        }
        if let Some(rx) = self.platform.thumbnail_rx.take() {
            match rx.try_recv() {
                Ok(Ok((uri, jpeg))) => {
                    let len = jpeg.len();
                    self.platform.thumbnail_image = Some((uri, jpeg));
                    self.toast.add(
                        format!("Thumbnail ready ({len} bytes)"),
                        ToastVariant::Success,
                        now,
                    );
                }
                Ok(Err(error)) => {
                    self.toast.add(
                        format!("Thumbnail error: {error}"),
                        ToastVariant::Error,
                        now,
                    );
                }
                Err(mpsc::TryRecvError::Empty) => {
                    self.platform.thumbnail_rx = Some(rx);
                }
                Err(mpsc::TryRecvError::Disconnected) => {
                    self.toast
                        .add("Thumbnail disconnected", ToastVariant::Error, now);
                }
            }
        }
        if let Some(summary) = poll_ok(
            &mut self.platform.zip_rx,
            &mut self.toast,
            now,
            "Zip error",
            "Zip disconnected",
        ) {
            self.toast.add(summary, ToastVariant::Success, now);
        }
        if let Some(rx) = self.platform.crypto_rx.take() {
            match rx.try_recv() {
                Ok(Ok(text)) => {
                    let label = match self.platform.crypto_op.take() {
                        Some(crate::app::CryptoOp::Decrypt) => "Decrypted text",
                        _ => "Encrypted note",
                    };
                    let len = text.len();
                    self.platform.crypto_output = text;
                    self.toast.add(
                        format!("{label} ready ({len} bytes)"),
                        ToastVariant::Success,
                        now,
                    );
                }
                Ok(Err(error)) => {
                    self.platform.crypto_op = None;
                    self.toast
                        .add(format!("Crypto error: {error}"), ToastVariant::Error, now);
                }
                Err(mpsc::TryRecvError::Empty) => {
                    self.platform.crypto_rx = Some(rx);
                }
                Err(mpsc::TryRecvError::Disconnected) => {
                    self.platform.crypto_op = None;
                    self.toast
                        .add("Crypto disconnected", ToastVariant::Error, now);
                }
            }
        }
        if let Some(msg) = poll_ok(
            &mut self.platform.worker_rx,
            &mut self.toast,
            now,
            "Worker error",
            "Worker disconnected",
        ) {
            self.toast.add(msg, ToastVariant::Success, now);
        }
        if self.has_pending_promises() {
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
                functora_egui::storage::persist_value(&key, &val);
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
                let loaded: Option<String> = functora_egui::storage::load_state(&key);
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
                functora_egui::storage::persist_value(&self.platform.storage_key, &String::new());
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
            functora_egui::storage::persist_value(
                "demo_persistent",
                &self.platform.storage_persistent_text,
            );
            self.toast.add(
                "Persistent saved",
                ToastVariant::Success,
                ui.ctx().input(|i| i.time),
            );
        }
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

    pub fn demo_clipboard(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Clipboard read/write via arboard (desktop), navigator.clipboard (web), ClipboardManager (Android).",
        )
        .show(ui);
        ui.add_space(12.0);
        let w = ui.available_width();
        _ = Label::new("Write to clipboard").show(ui);
        ui.add_space(8.0);
        _ = Input::new(&mut self.platform.clipboard_write)
            .placeholder("text to copy")
            .show(ui);
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f2| {
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
        ui.add_space(8.0);
        _ = Label::new("Last pasted").show(ui);
        ui.add_space(8.0);
        _ = Textarea::new(&mut self.platform.clipboard_read)
            .placeholder("pasted text appears here")
            .desired_width(w)
            .show(ui);

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
        }

        snippet(
            ui,
            "// Deep links: store + take + route parsing\nuse functora_egui::deep_link::{store_url, take_url, url_to_route};\n\n// Store a URL (e.g. from push notification)\nlet url = \"https://myapp.com/?page=settings&tab=notifications\";\nstore_url(url);\n\n// Later, take and parse it\nlet url = take_url();\nlet route = url_to_route(&url);\n// route = Route { path: \"/settings\", query: {\"tab\": \"notifications\"} }",
        );
    }

    pub fn demo_files(&mut self, ui: &mut egui::Ui) {
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
            }
        });
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
                    _ = Flex::column().gap(4.0).align_start().show(ui2, |f| {
                        _ = f.ui(|ui3| {
                            _ = Typography::small(format!("{name} ({mime}, {size})")).show(ui3);
                        });
                        _ = f.ui(|ui3| match preview {
                            functora_egui::files::Preview::Text(ref t) => {
                                _ = Label::new(t.chars().take(200).collect::<String>()).show(ui3);
                            }
                            functora_egui::files::Preview::Markdown(ref t) => {
                                _ = functora_egui::markdown_view::show(
                                    ui3,
                                    &mut self.platform.md_cache,
                                    t,
                                );
                            }
                            functora_egui::files::Preview::Image(_) => {
                                let theme = ui3.ctx().shadcn_theme();
                                let clicked = ui3
                                    .add(
                                        egui::Image::from_bytes(
                                            format!("bytes://{name}"),
                                            data.clone(),
                                        )
                                        .bg_fill(theme.secondary)
                                        .sense(egui::Sense::click())
                                        .max_width(220.0)
                                        .max_height(220.0),
                                    )
                                    .on_hover_text(if name.is_empty() {
                                        "Image preview".to_owned()
                                    } else {
                                        format!("Click to {name}")
                                    })
                                    .clicked();
                                if clicked {
                                    self.toast.add(
                                        format!("Image: {name}"),
                                        functora_egui::ToastVariant::Default,
                                        ui3.ctx().input(|i| i.time),
                                    );
                                }
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
        if ui.add(Button::new("Create revokable blob (txt)")).clicked() {
            let preview = functora_egui::files::preview_blob("hello.txt", b"hello blob");
            self.toast.add(
                format!("blob preview: {preview:?}"),
                ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
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
            if f.add(Button::new("Clear").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.progress_job = None;
                self.platform.progress_running = false;
            }
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Claim guard demo")).inner.clicked() {
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
        });
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
            if f.add(Button::new("QR SVG").variant(ButtonVariant::Outline))
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
                    self.toast.add(
                        "Claimed! holding for 2s...",
                        ToastVariant::Success,
                        ctx.input(|i| i.time),
                    );
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
                    self.toast.add(
                        "Already in flight - rejected",
                        ToastVariant::Error,
                        ctx.input(|i| i.time),
                    );
                }
            }
        });

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
            let busy = self.platform.camera_rx.is_some();
            if f.add(
                Button::new("Check")
                    .icon(functora_egui::LucideIcon::Camera)
                    .enabled(!busy),
            )
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
            if f.add(
                Button::new("Start")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
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
            if f.add(
                Button::new("Capture")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
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
            if f.add(
                Button::new("Stop")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
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
            if f.add(
                Button::new(if self.platform.qr_rx.is_some() {
                    "Generating..."
                } else {
                    "Generate QR"
                })
                .icon(functora_egui::LucideIcon::QrCode)
                .enabled(self.platform.qr_rx.is_none()),
            )
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
            if f.add(Button::new("Clear").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.qr_state.clear_decoded();
                self.platform.qr_state.clear_error();
            }
        });
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
                .add(Button::new("Restart scanner").variant(ButtonVariant::Outline))
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

    pub fn demo_thumbnail(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Thumbnail: files::video_thumbnail (mp4 data URL -> jpeg data URL) + cache. Native decodes via mp4+rust_h264; web reports unavailable.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(
            Input::new(&mut self.platform.thumbnail_input)
                .placeholder("data:video/mp4;base64,... or data:image/..."),
        );
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let busy = self.platform.thumbnail_rx.is_some();
            if f.add(
                Button::new(if busy {
                    "Generating..."
                } else {
                    "Generate thumbnail"
                })
                .icon(functora_egui::LucideIcon::Image)
                .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let url = self.platform.thumbnail_input.clone();
                self.platform.thumbnail_rx =
                    Some(spawn_async(async move { Self::make_thumbnail(&url) }));
            }
        });
        ui.add_space(8.0);
        if let Some((uri, jpeg)) = self.platform.thumbnail_image.clone() {
            _ = Typography::small(format!("Thumbnail: {} bytes", jpeg.len())).show(ui);
            ui.add_space(4.0);
            _ = ui.add(
                egui::Image::from_bytes(uri, jpeg)
                    .maintain_aspect_ratio(true)
                    .max_height(240.0),
            );
            ui.add_space(4.0);
        }
        _ = Typography::small(
            "Tip: pick a video file in Files demo, then paste its data URL here. Non-video input reports an honest error.",
        )
        .show(ui);

        snippet(
            ui,
            "// Thumbnail: files::video_thumbnail (mp4 data URL -> jpeg) + from_bytes display\nuse functora_egui::{spawn_async, files::video_thumbnail};\n\n// Pure helper (runs inside spawn_async so mp4 decode never blocks paint)\nfn make_thumbnail(url: &str) -> Result<(String, Vec<u8>), String> {\n    let data_url = video_thumbnail(url)\n        .ok_or_else(|| \"No thumbnail available\".to_owned())?;\n    let payload = data_url.split_once(',').map(|(_, rest)| rest).unwrap_or(\"\");\n    let jpeg = base64_decode(payload)?;\n    Ok((\"bytes://thumb.jpg\".to_owned(), jpeg))\n}\n\nlet url = thumbnail_input.clone();\nthumbnail_rx = Some(spawn_async(async move { make_thumbnail(&url) }));\n\n// Render the stored bytes (bytes:// keeps the extension for loader routing)\nif let Some((uri, jpeg)) = &thumbnail_image {\n    ui.add(\n        egui::Image::from_bytes(uri.clone(), jpeg.clone())\n            .maintain_aspect_ratio(true)\n            .max_height(240.0),\n    );\n}",
        );
    }

    pub(crate) fn demo_zip(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Zip: zip::create_zip_async / unzip_async over the picked files from Files demo, then verify_zip_roundtrip compares names and bytes.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(format!(
            "Picked files for zip: {} (from Files)",
            self.platform.picked.len()
        ))
        .show(ui);
        ui.add_space(8.0);
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let busy = self.platform.zip_rx.is_some();
            if f.add(
                Button::new(if busy {
                    "Zipping..."
                } else {
                    "Create zip + verify"
                })
                .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let files = self.platform.picked.clone();
                if files.is_empty() {
                    self.toast.add(
                        "No files picked (go to Files)",
                        ToastVariant::Error,
                        ctx.input(|i| i.time),
                    );
                } else {
                    self.platform.zip_rx = Some(spawn_async(async move {
                        Self::zip_roundtrip_async(files).await
                    }));
                }
            }
        });

        snippet(
            ui,
            "// Zip: create_zip_async + unzip_async + verify_zip_roundtrip\nuse functora_egui::zip::{create_zip_async, unzip_async};\nuse functora_egui::progress::Stage;\nuse functora_egui::files::Attachment;\n\nlet attachments = picked\n    .iter()\n    .map(|(name, data)| Attachment { name: name.clone(), data: data.clone().into() })\n    .collect::<Vec<_>>();\n\nlet zipped = create_zip_async(&attachments, |_| {}, Stage::Zip).await?;\nlet unzipped = unzip_async(zipped, |_| {}, Stage::Unzip).await?;\nlet summary = Self::verify_zip_roundtrip(&picked, unzipped)?;\n// \"Zip ok: 2 files, 42 bytes, round-trip verified\"",
        );
    }

    pub(crate) fn demo_crypto(&mut self, ui: &mut egui::Ui) {
        self.poll_platform_promises(ui.ctx());
        _ = Typography::muted(
            "Crypto: encrypt_output / decrypt_output (ChaCha20Poly1305 + Argon2id via crypto::encrypt_symmetric). Key derivation runs in spawn_async so paint never blocks.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(Input::new(&mut self.platform.crypto_input).placeholder("plain text"));
        ui.add_space(4.0);
        _ = ui.add(Input::new(&mut self.platform.crypto_password).placeholder("password"));
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let busy = self.platform.crypto_rx.is_some();
            if f.add(
                Button::new(if busy { "Working..." } else { "Encrypt" })
                    .icon(functora_egui::LucideIcon::Lock)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let input = self.platform.crypto_input.clone();
                let password = self.platform.crypto_password.clone();
                self.platform.crypto_op = Some(crate::app::CryptoOp::Encrypt);
                self.platform.crypto_rx = Some(spawn_async(async move {
                    Self::encrypt_output(&input, &password)
                }));
            }
            if f.add(
                Button::new("Decrypt")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let json = self.platform.crypto_output.clone();
                let password = self.platform.crypto_password.clone();
                self.platform.crypto_op = Some(crate::app::CryptoOp::Decrypt);
                self.platform.crypto_rx = Some(spawn_async(async move {
                    Self::decrypt_output(&json, &password)
                }));
            }
            if f.add(
                Button::new("Clear")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                self.platform.crypto_output.clear();
            }
        });
        if !self.platform.crypto_output.is_empty() {
            ui.add_space(8.0);
            _ = Card::new().show(ui, |ui2| {
                _ = Typography::small(&self.platform.crypto_output).show(ui2);
            });
        }

        snippet(
            ui,
            "// Crypto: encrypt_output / decrypt_output (ChaCha20Poly1305 + Argon2id)\nuse functora_egui::crypto::{CipherType, EncryptedNote, encrypt_symmetric, decrypt_symmetric};\n\n// Pure helpers (run inside spawn_async: Argon2id blocks)\nfn encrypt_output(input: &str, password: &str) -> Result<String, String> {\n    let note = encrypt_symmetric(input.as_bytes(), password, CipherType::ChaCha20Poly1305, &[])?;\n    Ok(serde_json::to_string(&note)?)\n}\nfn decrypt_output(json: &str, password: &str) -> Result<String, String> {\n    let note: EncryptedNote = serde_json::from_str(json)?;\n    let bytes = decrypt_symmetric(&note, password, &[])?;\n    Ok(String::from_utf8(bytes)?)\n}\n\ncrypto_op = Some(CryptoOp::Encrypt);\ncrypto_rx = Some(spawn_async(async move { encrypt_output(&input, &password) }));\n// poll arm stores the output and toasts \"Encrypted note ready (N bytes)\"",
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
        });
        ui.add_space(8.0);
        _ = Typography::small("Check ProgressWorker demo for Job<Stage> progress details.")
            .show(ui);

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
        if ui.add(Button::new("Save to platform_info")).clicked() {
            functora_egui::storage::persist_value("platform_info", &self.platform.platform_info);
            self.toast
                .add("Saved", ToastVariant::Success, ui.ctx().input(|i| i.time));
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

    pub fn demo_markdown(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Markdown: CommonMarkViewer + CommonMarkCache (egui_commonmark) renders raw source to native widgets. Opt-in feature `markdown`.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Label::new("Source").show(ui);
        ui.add_space(8.0);
        _ = Textarea::new(&mut self.platform.md_source)
            .placeholder("# Hello")
            .desired_width(ui.available_width())
            .show(ui);
        ui.add_space(8.0);
        let preview_width = ui.available_width();
        _ = Card::new().show(ui, |ui2| {
            ui2.set_min_width((preview_width - 32.0).max(0.0));
            _ = functora_egui::markdown_view::show(
                ui2,
                &mut self.platform.md_cache,
                &self.platform.md_source,
            );
        });

        snippet(
            ui,
            "// Markdown: theme-aware rendered CommonMark\nuse functora_egui::CommonMarkCache;\nuse functora_egui::markdown_view;\n\n// Cache (persist across frames)\nlet mut cache = CommonMarkCache::default();\n\n// Render every frame (maps shadcn theme onto egui visuals)\nmarkdown_view::show(ui, &mut cache, \"# Hello\\n\\nThis is **bold**.\");",
        );
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
