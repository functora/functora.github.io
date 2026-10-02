use base64::Engine as _;
use functora_egui::ToastState;
use functora_egui::{ToastVariant, spawn_async};
use std::sync::mpsc;

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

impl crate::state::ShowcaseApp {
    fn has_pending_promises(&self) -> bool {
        self.platform.clipboard_rx.is_some()
            || self.platform.clipboard_write_rx.is_some()
            || self.platform.share_rx.is_some()
            || self.platform.pick_rx.is_some()
            || self.platform.download_rx.is_some()
            || self.platform.pwa_rx.is_some()
            || self.platform.camera_rx.is_some()
            || self.platform.thumbnail_rx.is_some()
            || self.platform.zip_rx.is_some()
            || self.platform.crypto_rx.is_some()
            || self.platform.worker_rx.is_some()
    }

    /// Cuts `url` down to at most `max_bytes` on a char boundary, so preview
    /// labels can never panic on multi-byte data URLs. Pure so tests can
    /// exercise every boundary.
    #[must_use]
    pub fn truncate_preview_url(url: &str, max_bytes: usize) -> &str {
        if url.len() <= max_bytes {
            return url;
        }
        let end = url
            .char_indices()
            .map(|(index, _)| index)
            .take_while(|&index| index <= max_bytes)
            .last()
            .unwrap_or_default();
        &url[..end]
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

    pub(crate) async fn zip_roundtrip_async(
        files: Vec<(String, Vec<u8>)>,
    ) -> Result<String, String> {
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
                        Err(functora_egui::error::Error::Cancelled) => {
                            self.toast.add("Pick cancelled", ToastVariant::Default, now);
                        }
                        Err(e) => {
                            self.toast
                                .add(format!("Pick failed: {e}"), ToastVariant::Error, now);
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
                Err(mpsc::TryRecvError::Disconnected) => {
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
                        Some(crate::state::CryptoOp::Decrypt) => "Decrypted text",
                        Some(crate::state::CryptoOp::Encrypt) | None => "Encrypted note",
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

    /// Claims the demo's `InFlight` guard and moves it into a `spawn_async`
    /// task that holds it for the simulated 2s operation, so the claim stays
    /// held for the whole task and releases only when the task drops the
    /// guard. Returns `false` when a claim is already held.
    pub fn try_claim_in_flight(&mut self, ctx: &egui::Context) -> bool {
        let Some(guard) = self.platform.in_flight.claim() else {
            return false;
        };
        let repaint = ctx.clone();
        drop(spawn_async(async move {
            #[cfg(target_arch = "wasm32")]
            gloo_timers::future::TimeoutFuture::new(2000).await;
            #[cfg(not(target_arch = "wasm32"))]
            std::thread::sleep(std::time::Duration::from_secs(2));
            drop(guard);
            repaint.request_repaint();
        }));
        true
    }
}
