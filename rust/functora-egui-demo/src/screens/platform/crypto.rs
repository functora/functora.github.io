use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Card, Flex, Input, Typography, spawn_async};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_crypto(&mut self, ui: &mut egui::Ui) {
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

        snippet(
            ui,
            "// Crypto: encrypt_output / decrypt_output (ChaCha20Poly1305 + Argon2id)\nuse functora_egui::crypto::{CipherType, EncryptedNote, encrypt_symmetric, decrypt_symmetric};\n\n// Pure helpers (run inside spawn_async: Argon2id blocks)\nfn encrypt_output(input: &str, password: &str) -> Result<String, String> {\n    let note = encrypt_symmetric(input.as_bytes(), password, CipherType::ChaCha20Poly1305, &[])?;\n    Ok(serde_json::to_string(&note)?)\n}\nfn decrypt_output(json: &str, password: &str) -> Result<String, String> {\n    let note: EncryptedNote = serde_json::from_str(json)?;\n    let bytes = decrypt_symmetric(&note, password, &[])?;\n    Ok(String::from_utf8(bytes)?)\n}\n\ncrypto_op = Some(CryptoOp::Encrypt);\ncrypto_rx = Some(spawn_async(async move { encrypt_output(&input, &password) }));\n// poll arm stores the output and toasts \"Encrypted note ready (N bytes)\"",
        );
    }
}
