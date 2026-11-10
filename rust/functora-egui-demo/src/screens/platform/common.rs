use base64::Engine as _;
use functora_egui::ToastState;
use functora_egui::{PollOutcome, ToastVariant, poll_channel, spawn_async};
use std::sync::mpsc;

const STREAM_AAD: &[u8] = b"functora-egui-demo/stream";

fn poll_ok<T>(
    slot: &mut Option<mpsc::Receiver<Result<T, String>>>,
    toast: &mut ToastState,
    now: f64,
    err_prefix: &str,
    disconnected: &str,
) -> Option<T> {
    match poll_channel(slot) {
        None => None,
        Some(PollOutcome::Disconnected) => {
            toast.add(disconnected, ToastVariant::Error, now);
            None
        }
        Some(PollOutcome::Ready(Ok(value))) => Some(value),
        Some(PollOutcome::Ready(Err(error))) => {
            toast.add(format!("{err_prefix}: {error}"), ToastVariant::Error, now);
            None
        }
    }
}

impl crate::state::ShowcaseApp {
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

    /// Encrypts `input` with the chunked streaming API and returns the note
    /// as JSON. The streaming variant derives one key and seals every chunk
    /// with a per-chunk nonce, so large payloads never need one big buffer.
    pub fn stream_encrypt_output(input: &str, password: &str) -> Result<String, String> {
        functora_egui::crypto::stream_encrypt_symmetric(
            input.as_bytes(),
            password,
            functora_egui::crypto::CipherType::ChaCha20Poly1305,
            STREAM_AAD,
        )
        .map_err(|e| e.to_string())
        .and_then(|note| serde_json::to_string(&note).map_err(|e| e.to_string()))
    }

    /// Parses a streaming note and decrypts it with `password`.
    pub fn stream_decrypt_output(json: &str, password: &str) -> Result<String, String> {
        serde_json::from_str::<functora_egui::crypto::EncryptedNote>(json)
            .map_err(|e| format!("Not an encrypted note: {e}"))
            .and_then(|note| {
                functora_egui::crypto::stream_decrypt_symmetric(&note, password, STREAM_AAD)
                    .map_err(|e| e.to_string())
            })
            .and_then(|bytes| {
                String::from_utf8(bytes).map_err(|e| format!("Decrypted bytes are not text: {e}"))
            })
    }

    /// Derives stream parts, seals one chunk and recovers it with a second
    /// `StreamParts` built from the same salt and nonce.
    pub fn stream_parts_roundtrip(password: &str, plaintext: &str) -> Result<String, String> {
        use functora_egui::crypto::{ChunkKind, CipherType, StreamParts};
        let parts = StreamParts::derive(password, CipherType::ChaCha20Poly1305, STREAM_AAD)
            .map_err(|e| e.to_string())?;
        let sealed = parts
            .encrypt_chunk(0, ChunkKind::Last, plaintext.as_bytes())
            .map_err(|e| e.to_string())?;
        let recovered = StreamParts::recover(
            password,
            CipherType::ChaCha20Poly1305,
            parts.salt(),
            parts.nonce(),
            STREAM_AAD,
        )
        .map_err(|e| e.to_string())?;
        let opened = recovered
            .decrypt_chunk(0, ChunkKind::Last, &sealed)
            .map_err(|e| e.to_string())?;
        String::from_utf8(opened).map_err(|e| format!("Decrypted bytes are not text: {e}"))
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
            std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
        )
        .await
        .map_err(|e| e.to_string())?;
        let unzipped = functora_egui::zip::unzip_async(
            zipped,
            |_| {},
            functora_egui::progress::Stage::Unzip,
            std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
        )
        .await
        .map_err(|e| e.to_string())?;
        Self::verify_zip_roundtrip(&files, &unzipped)
    }

    /// Every message the library can produce, so the demo renders the full
    /// catalog instead of a hand-picked subset.
    #[must_use]
    pub fn all_messages() -> Vec<functora_egui::messages::Msg> {
        use functora_egui::messages::Msg;
        use functora_egui::progress::Stage;
        [
            Msg::Copied,
            Msg::Pasted,
            Msg::About,
            Msg::License,
            Msg::Select,
            Msg::Search,
            Msg::Password,
            Msg::PasswordPlaceholder,
            Msg::Paste,
            Msg::Copy,
            Msg::Loading,
            Msg::ErrorTitle("boom".to_owned()),
            Msg::EnvError(std::env::VarError::NotPresent),
            Msg::ChannelError(std::sync::mpsc::RecvError),
            Msg::NotJsonObject("[]".to_owned()),
            Msg::ErrorTitleLabel,
            Msg::PasswordRequired,
            Msg::ClipboardWriteError("denied".to_owned()),
            Msg::ClipboardReadError("empty".to_owned()),
            Msg::CameraNotAvailable("desktop".to_owned()),
            Msg::CameraPermissionDenied("denied".to_owned()),
            Msg::Back,
            Msg::Home,
            Msg::DonateGreeting,
            Msg::DonateIntro,
            Msg::Stage(Stage::Attach),
            Msg::Stage(Stage::Zip),
            Msg::Stage(Stage::Encrypt),
            Msg::Stage(Stage::Decrypt),
            Msg::Stage(Stage::Unzip),
            Msg::Stage(Stage::Download),
            Msg::Stage(Stage::Preview),
            Msg::Copyright,
            Msg::AllRightsReserved,
            Msg::ByContinuing,
            Msg::YouAgree,
            Msg::TermsOfService,
            Msg::TermsOfServiceTitle,
            Msg::PrivacyPolicyAnd,
            Msg::PrivacyPolicyTitle,
            Msg::VersionLabel,
            Msg::Application,
            Msg::Theme,
            Msg::Donate,
            Msg::DonateLink,
            Msg::And,
            Msg::FooterShareWord,
            Msg::FooterAppWord,
            Msg::LanguageFlag(functora_egui::i18n::Language::Eng),
            Msg::LanguageName(functora_egui::i18n::Language::Eng),
            Msg::LicenseText,
            Msg::PrivacyText,
            Msg::CopyAppLink,
            Msg::ShareAppLink,
            Msg::Sent,
            Msg::SourceCodeButton,
            Msg::AuthorButton,
            Msg::JoinTestingButton,
            Msg::GooglePlayButton,
            Msg::DownloadApkButton,
            Msg::AboutAndroidBeta1,
            Msg::AboutAndroidBetaLink1,
            Msg::AboutAndroidBeta2,
            Msg::AboutAndroidBetaLink2,
            Msg::AboutAndroidBeta3,
            Msg::AboutAndroidBetaLink3,
            Msg::AboutAndroidBeta4,
            Msg::PwaInstallPrompt,
            Msg::PwaInstallSuccess,
            Msg::PwaInstallRejected,
            Msg::PwaInstallUnavailable,
            Msg::PwaInstallIos,
            Msg::PwaInstallMac,
            Msg::CipherAesLabel,
            Msg::CipherChaChaLabel,
            Msg::PickingFiles,
            Msg::FilesAttached(3),
            Msg::Cancel,
            Msg::Start,
            Msg::Stop,
            Msg::PickImage,
            Msg::Clear,
            Msg::Preparing,
            Msg::Cancelling,
            Msg::CameraStarting,
            Msg::CameraOff,
            Msg::Decoded,
        ]
        .into_iter()
        .collect()
    }

    /// Converts an RGBA buffer to 8-bit luma, the input format the core QR
    /// decoders expect.
    #[must_use]
    pub fn rgba_to_luma(rgba: &[u8]) -> Vec<u8> {
        rgba.as_chunks::<4>()
            .0
            .iter()
            .map(|px| {
                u8::try_from(
                    (306 * u64::from(px[0])
                        + 601 * u64::from(px[1])
                        + 117 * u64::from(px[2])
                        + 0x200)
                        >> 10,
                )
                .unwrap_or(u8::MAX)
            })
            .collect()
    }

    /// One error per public `Error` variant family, so the demo shows the
    /// typed payloads instead of a single hand-picked case.
    #[must_use]
    pub fn demo_errors() -> Vec<functora_egui::error::Error> {
        use functora_egui::error::{
            Capability, ClipboardError, Error, IoError, JsonError, Missing, WorkerStopped,
        };
        std::iter::once(
            serde_json::from_str::<serde_json::Value>("{")
                .err()
                .map(JsonError::from)
                .map(Error::Json),
        )
        .flatten()
        .chain([
            Error::IO(IoError::from(std::io::Error::other("demo io failure"))),
            Error::Missing(Missing::Window),
            Error::Missing(Missing::Document),
            Error::Missing(Missing::Body),
            Error::Missing(Missing::VideoElement),
            Error::Missing(Missing::Canvas),
            Error::Missing(Missing::Context2d),
            Error::Missing(Missing::LocalStorage),
            Error::Missing(Missing::Anchor),
            Error::Missing(Missing::Input),
            Error::Missing(Missing::FileEntry),
            Error::Missing(Missing::ClipboardText),
            Error::Missing(Missing::DesktopSession),
            Error::Missing(Missing::AndroidApp),
            Error::Missing(Missing::DataDir),
            Error::CapabilityDisabled(Capability::Clipboard),
            Error::CapabilityDisabled(Capability::Share),
            Error::CapabilityDisabled(Capability::Save),
            Error::Clipboard(ClipboardError::Unavailable),
            Error::Clipboard(ClipboardError::Busy),
            Error::Clipboard(ClipboardError::ContentUnavailable),
            Error::Clipboard(ClipboardError::ConversionFailed),
            Error::Clipboard(ClipboardError::Unknown("unknown".to_owned())),
            Error::Worker(WorkerStopped),
            Error::QrNotFound,
            Error::EvalFinished,
            Error::CameraStalled,
            Error::Cancelled,
            Error::FilePickerTimedOut,
            Error::FilePickerFailed,
            Error::SaveCancelled,
            Error::OpenUrlFailed {
                url: "https://example.com".to_owned(),
            },
            Error::OpenUrlTimedOut {
                url: "https://example.com".to_owned(),
            },
        ])
        .collect()
    }

    /// Writes a small JSON document with the core storage helpers and reads
    /// it back, so the demo exercises the file-backed API. The file lives in
    /// the system temp directory, never in the repository.
    pub fn core_storage_demo() -> Result<String, String> {
        use functora_egui::storage::{
            find_or_init_key, get_json_value, read_json_object, set_json_value, update_key,
            write_json_object,
        };
        let path = std::env::temp_dir().join("functora-egui-demo-storage.json");
        write_json_object(&path, &serde_json::json!({ "theme": "dark" }))
            .map_err(|e| e.to_string())?;
        let object = read_json_object(&path).map_err(|e| e.to_string())?;
        set_json_value(&path, "locale", "en").map_err(|e| e.to_string())?;
        update_key(&path, "pinned", true).map_err(|e| e.to_string())?;
        let pinned: bool =
            find_or_init_key(&path, "pinned", || false).map_err(|e| e.to_string())?;
        let theme: Option<serde_json::Value> =
            get_json_value(&path, "theme").map_err(|e| e.to_string())?;
        Ok(format!("object {object}, pinned {pinned}, theme {theme:?}"))
    }

    /// Reads the JSON document written by `core_storage_demo` back.
    pub fn core_storage_read_demo() -> Result<String, String> {
        use functora_egui::storage::{get_json_value, read_json_object};
        let path = std::env::temp_dir().join("functora-egui-demo-storage.json");
        let object = read_json_object(&path).map_err(|e| e.to_string())?;
        let locale: Option<serde_json::Value> =
            get_json_value(&path, "locale").map_err(|e| e.to_string())?;
        Ok(format!("object {object}, locale {locale:?}"))
    }

    /// Packs the picked files into a sealed archive, reads the archive
    /// metadata back and unseals it again. Runs inside `spawn_async` in the
    /// demo because the archive work and the Argon2id key derivation block.
    pub(crate) async fn package_roundtrip(
        files: Vec<(String, Vec<u8>)>,
        password: &str,
    ) -> Result<String, String> {
        use functora_egui::crypto::CipherType;
        use functora_egui::package::{ArchiveSource, PackageStages};
        use functora_egui::progress::Stage;

        const PREFIX: &[u8] = b"functora-egui-demo";
        let entries = files
            .iter()
            .map(|(name, data)| (name.clone(), std::sync::Arc::from(data.clone())))
            .collect::<Vec<_>>();
        let stages = PackageStages::new(Stage::Zip, Stage::Encrypt, Stage::Decrypt);
        let token = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let archive = functora_egui::package::package_async(
            entries,
            password,
            Some(CipherType::ChaCha20Poly1305),
            PREFIX,
            stages,
            |_| {},
            std::sync::Arc::clone(&token),
        )
        .await
        .map_err(|e| e.to_string())?;
        let meta = functora_egui::package::read_metadata(&ArchiveSource::Bytes(archive.clone()))
            .map_err(|e| e.to_string())?;
        let unsealed = functora_egui::package::extract_package_async(
            ArchiveSource::Bytes(archive),
            password,
            PREFIX,
            stages,
            |_| {},
            token,
        )
        .await
        .map_err(|e| e.to_string())?;
        Ok(format!(
            "Package ok: {} bytes sealed, cipher {:?}, kdf {:?}, salt {} bytes, unsealed {} bytes",
            meta.nonce.len() + meta.salt.len(),
            meta.cipher,
            meta.kdf,
            meta.salt.len(),
            unsealed.len()
        ))
    }

    pub fn poll_platform_promises(&mut self, ctx: &egui::Context) {
        if let Some(shared) = self.platform.pick_progress.clone()
            && let Ok(guard) = shared.lock()
        {
            self.platform.pick_job.clone_from(&guard);
        }
        let now = ctx.input(|i| i.time);
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
        match poll_channel(&mut self.platform.pick_rx) {
            None => {}
            Some(PollOutcome::Disconnected) => {
                self.platform.pick_cancel = None;
                self.platform.pick_overlay_open = false;
                self.platform.pick_job = None;
                self.platform.pick_progress = None;
            }
            Some(PollOutcome::Ready(res)) => {
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
        }
        if self.platform.pick_rx.is_none() {
            self.platform.pick_overlay_open = false;
        }
        if let Some(name) = poll_ok(
            &mut self.platform.download_rx,
            &mut self.toast,
            now,
            "Save failed",
            "Save disconnected",
        ) {
            self.toast
                .add(format!("Saved {name}"), ToastVariant::Success, now);
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
        match poll_channel(&mut self.platform.thumbnail_rx) {
            None => {}
            Some(PollOutcome::Disconnected) => {
                self.toast
                    .add("Thumbnail disconnected", ToastVariant::Error, now);
            }
            Some(PollOutcome::Ready(Ok((uri, jpeg)))) => {
                let len = jpeg.len();
                self.platform.thumbnail_image = Some((uri, jpeg));
                self.toast.add(
                    format!("Thumbnail ready ({len} bytes)"),
                    ToastVariant::Success,
                    now,
                );
            }
            Some(PollOutcome::Ready(Err(error))) => {
                self.toast.add(
                    format!("Thumbnail error: {error}"),
                    ToastVariant::Error,
                    now,
                );
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
        match poll_channel(&mut self.platform.crypto_rx) {
            None => {}
            Some(PollOutcome::Disconnected) => {
                self.platform.crypto_op = None;
                self.toast
                    .add("Crypto disconnected", ToastVariant::Error, now);
            }
            Some(PollOutcome::Ready(Ok(text))) => {
                let label = match self.platform.crypto_op.take() {
                    Some(crate::state::CryptoOp::Decrypt) => "Decrypted text",
                    Some(crate::state::CryptoOp::StreamDecrypt) => "Decrypted stream",
                    Some(crate::state::CryptoOp::StreamEncrypt) => "Encrypted stream",
                    Some(crate::state::CryptoOp::StreamParts) => "Stream parts round-trip",
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
            Some(PollOutcome::Ready(Err(error))) => {
                self.platform.crypto_op = None;
                self.toast
                    .add(format!("Crypto error: {error}"), ToastVariant::Error, now);
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
        if let Some(summary) = poll_ok(
            &mut self.platform.package_rx,
            &mut self.toast,
            now,
            "Package error",
            "Package disconnected",
        ) {
            self.platform.package_output.clone_from(&summary);
            self.toast.add(summary, ToastVariant::Success, now);
        }
        if self.platform.has_pending_promises() {
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
