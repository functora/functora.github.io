use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Card, Flex, Input, Typography, spawn_async};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_crypto(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Crypto: encrypt_output / decrypt_output (ChaCha20Poly1305 + Argon2id via crypto::encrypt_symmetric) plus the chunked streaming API (stream_encrypt_symmetric / stream_decrypt_symmetric / StreamParts / derive_key). Key derivation runs in spawn_async so paint never blocks.",
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
                self.platform.crypto_op = Some(crate::state::CryptoOp::Encrypt);
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
                self.platform.crypto_op = Some(crate::state::CryptoOp::Decrypt);
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
        ui.add_space(12.0);
        crate::screens::section(ui, "Streaming (chunked) encryption");
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            let busy = self.platform.crypto_rx.is_some();
            if f.add(
                Button::new("Stream encrypt")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let input = self.platform.crypto_input.clone();
                let password = self.platform.crypto_password.clone();
                self.platform.crypto_op = Some(crate::state::CryptoOp::StreamEncrypt);
                self.platform.crypto_rx = Some(spawn_async(async move {
                    Self::stream_encrypt_output(&input, &password)
                }));
            }
            if f.add(
                Button::new("Stream decrypt")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let json = self.platform.crypto_output.clone();
                let password = self.platform.crypto_password.clone();
                self.platform.crypto_op = Some(crate::state::CryptoOp::StreamDecrypt);
                self.platform.crypto_rx = Some(spawn_async(async move {
                    Self::stream_decrypt_output(&json, &password)
                }));
            }
            if f.add(
                Button::new("StreamParts round-trip")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let input = self.platform.crypto_input.clone();
                let password = self.platform.crypto_password.clone();
                self.platform.crypto_op = Some(crate::state::CryptoOp::StreamParts);
                self.platform.crypto_rx = Some(spawn_async(async move {
                    Self::stream_parts_roundtrip(&password, &input)
                }));
            }
        });
        ui.add_space(8.0);
        _ = Typography::small(format!(
            "KEY_SIZE: {}, STREAM_CHUNK: {}, STREAM_TAG: {}",
            functora_egui::crypto::KEY_SIZE,
            functora_egui::crypto::STREAM_CHUNK,
            functora_egui::crypto::STREAM_TAG
        ))
        .show(ui);

        snippet(
            ui,
            "// Crypto: encrypt_output / decrypt_output (ChaCha20Poly1305 + Argon2id)\nuse functora_egui::crypto::{CipherType, EncryptedNote, encrypt_symmetric, decrypt_symmetric};\n\n// Pure helpers (run inside spawn_async: Argon2id blocks)\nfn encrypt_output(input: &str, password: &str) -> Result<String, String> {\n    let note = encrypt_symmetric(input.as_bytes(), password, CipherType::ChaCha20Poly1305, &[])?;\n    Ok(serde_json::to_string(&note)?)\n}\nfn decrypt_output(json: &str, password: &str) -> Result<String, String> {\n    let note: EncryptedNote = serde_json::from_str(json)?;\n    let bytes = decrypt_symmetric(&note, password, &[])?;\n    Ok(String::from_utf8(bytes)?)\n}\n\ncrypto_op = Some(CryptoOp::Encrypt);\ncrypto_rx = Some(spawn_async(async move { encrypt_output(&input, &password) }));\n// poll arm stores the output and toasts \"Encrypted note ready (N bytes)\"\n\n// Streaming variant: one derived key, per-chunk nonces, no big buffer\nuse functora_egui::crypto::{ChunkKind, StreamParts, stream_encrypt_symmetric, stream_decrypt_symmetric, derive_key, Kdf, KEY_SIZE, STREAM_CHUNK, STREAM_TAG};\n\nconst AAD: &[u8] = b\"functora-egui-demo/stream\";\n\nlet note = stream_encrypt_symmetric(input.as_bytes(), password, CipherType::ChaCha20Poly1305, AAD)?;\nlet plain = stream_decrypt_symmetric(&note, password, AAD)?;\n\n// Seal a single chunk by hand and recover it from salt + nonce\nlet parts = StreamParts::derive(password, CipherType::ChaCha20Poly1305, AAD)?;\nlet sealed = parts.encrypt_chunk(0, ChunkKind::Last, input.as_bytes())?;\nlet recovered = StreamParts::recover(password, CipherType::ChaCha20Poly1305, parts.salt(), parts.nonce(), AAD)?;\nlet opened = recovered.decrypt_chunk(0, ChunkKind::Last, &sealed)?;\n\n// Raw key derivation (Argon2id) - KEY_SIZE bytes, STREAM_CHUNK bytes per chunk\nlet key = derive_key(password, parts.salt(), Kdf::Argon2id)?;\nassert_eq!(key.len(), KEY_SIZE);",
        );
    }
}
