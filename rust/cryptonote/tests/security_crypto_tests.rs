//! The shared URL and archive formats must never carry the note body or the
//! password. An encrypted note leaks ciphertext on purpose; it must not leak
//! anything that turns the ciphertext back into plaintext without the
//! password.

use cryptonote::crypto::CipherType;
use cryptonote::encoding::{NoteData, build_url, decode_note, extract_note_param};
use functora_tagged::InfallibleInto;

const BASE_URL: &str = "https://functora.github.io/apps/cryptonote/0.2.0/?screen=open";
const PLAINTEXT: &str = "the quick brown fox jumps over the lazy dog";
const PASSWORD: &str = "correct horse battery staple";

fn encrypted_url(cipher: CipherType) -> String {
    let note = cryptonote::crypto::encrypt_symmetric(PLAINTEXT.as_bytes(), PASSWORD, cipher)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    build_url(BASE_URL, &NoteData::CipherText(note)).unwrap_or_else(|e| panic!("build_url: {e:?}"))
}

#[test]
fn encrypted_url_carries_neither_plaintext_nor_password() {
    for cipher in [CipherType::Aes256Gcm, CipherType::ChaCha20Poly1305] {
        let url = encrypted_url(cipher);
        assert!(!url.contains(PLAINTEXT), "the note body must never reach the URL");
        assert!(!url.contains(PASSWORD), "the password must never reach the URL");
        assert!(
            !url.contains(&base64_of(PLAINTEXT)),
            "the encoded note body must never reach the URL"
        );
    }
}

#[test]
fn encrypted_url_roundtrips_to_the_ciphertext() {
    let url = encrypted_url(CipherType::Aes256Gcm);
    let encoded = extract_note_param(&url).unwrap_or_else(|e| panic!("extract: {e:?}"));
    match decode_note(&encoded).unwrap_or_else(|e| panic!("decode: {e:?}")) {
        NoteData::CipherText(note) => {
            assert_eq!(note.cipher, CipherType::Aes256Gcm);
            assert_ne!(note.ciphertext.len(), 0);
            assert_eq!(note.salt.len(), 32);
            assert_eq!(note.nonce.len(), 12);
        }
        NoteData::PlainText(_) => panic!("an encrypted note must not decode to plaintext"),
    }
}

#[test]
fn tampered_url_payload_fails_to_decrypt() {
    let url = encrypted_url(CipherType::Aes256Gcm);
    let encoded = extract_note_param(&url).unwrap_or_else(|e| panic!("extract: {e:?}"));
    let NoteData::CipherText(note) = decode_note(&encoded).unwrap_or_else(|e| panic!("decode: {e:?}")) else {
        panic!("an encrypted note must not decode to plaintext")
    };

    let mut tampered_ciphertext = note.clone();
    tampered_ciphertext.ciphertext[0] ^= 0xFF;
    assert!(
        cryptonote::crypto::decrypt_symmetric(&tampered_ciphertext, PASSWORD).is_err(),
        "a tampered ciphertext must not decrypt"
    );

    let mut tampered_salt = note.clone();
    tampered_salt.salt[0] ^= 0xFF;
    assert!(
        cryptonote::crypto::decrypt_symmetric(&tampered_salt, PASSWORD).is_err(),
        "a tampered salt must not decrypt"
    );

    let mut tampered_nonce = note.clone();
    tampered_nonce.nonce[0] ^= 0xFF;
    assert!(
        cryptonote::crypto::decrypt_symmetric(&tampered_nonce, PASSWORD).is_err(),
        "a tampered nonce must not decrypt"
    );

    let mut truncated = note;
    let _ = truncated.ciphertext.pop();
    assert!(
        cryptonote::crypto::decrypt_symmetric(&truncated, PASSWORD).is_err(),
        "a truncated ciphertext must not decrypt"
    );
}

#[test]
fn wrong_password_fails_to_decrypt() {
    let note = cryptonote::crypto::encrypt_symmetric(PLAINTEXT.as_bytes(), PASSWORD, CipherType::Aes256Gcm)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    assert!(
        cryptonote::crypto::decrypt_symmetric(&note, "wrong password").is_err(),
        "a wrong password must not decrypt"
    );
}

#[test]
fn empty_password_fails_to_decrypt() {
    let note = cryptonote::crypto::encrypt_symmetric(PLAINTEXT.as_bytes(), PASSWORD, CipherType::Aes256Gcm)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    assert!(
        cryptonote::crypto::decrypt_symmetric(&note, "").is_err(),
        "an empty password must not decrypt"
    );
}

#[test]
fn plain_mode_url_carries_the_note_body_in_the_clear() {
    let url =
        build_url(BASE_URL, &NoteData::PlainText(PLAINTEXT.to_owned())).unwrap_or_else(|e| panic!("build_url: {e:?}"));
    assert!(!url.contains(PLAINTEXT), "the raw body is only base64url encoded");
    let encoded = extract_note_param(&url).unwrap_or_else(|e| panic!("extract: {e:?}"));
    match decode_note(&encoded).unwrap_or_else(|e| panic!("decode: {e:?}")) {
        NoteData::PlainText(text) => assert_eq!(
            text, PLAINTEXT,
            "plain mode is documented as carrying the body in the clear: the URL alone is enough to read the note"
        ),
        NoteData::CipherText(_) => panic!("a plain note must decode to plaintext"),
    }
}

fn base64_of(text: &str) -> String {
    use base64::Engine;
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(text.as_bytes())
}

#[test]
fn the_debug_output_never_carries_a_secret() {
    let state = cryptonote::state::TemporaryState {
        note: "the private note body".to_owned(),
        password: "sup3rs3cr3t".to_owned().into(),
        attachments: vec![functora_egui::files::Attachment {
            name: "passport.pdf".to_owned(),
            data: std::sync::Arc::from(b"the attachment bytes".to_vec()),
        }],
        ..Default::default()
    };

    let rendered = format!("{state:?}");

    assert!(
        !rendered.contains("sup3rs3cr3t"),
        "the password must never reach the Debug output: {rendered}"
    );
    assert!(
        !rendered.contains("the private note body"),
        "the note body must never reach the Debug output: {rendered}"
    );

    let attachment_bytes = state.attachments[0]
        .data
        .iter()
        .map(u8::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    assert!(
        !rendered.contains(&attachment_bytes),
        "the attachment body must never reach the Debug output: {rendered}"
    );
    assert!(
        rendered.contains("passport.pdf"),
        "the attachment name is not a secret and stays visible: {rendered}"
    );
}

#[test]
fn the_debug_output_never_carries_a_plain_external_note() {
    let url =
        build_url(BASE_URL, &NoteData::PlainText(PLAINTEXT.to_owned())).unwrap_or_else(|e| panic!("build_url: {e:?}"));
    let state = cryptonote::state::TemporaryState {
        external: cryptonote::External::Note(cryptonote::ExternalNote {
            data: NoteData::PlainText(PLAINTEXT.to_owned()),
            url,
        }),
        ..Default::default()
    };

    let rendered = format!("{state:?}");

    assert!(
        !rendered.contains(PLAINTEXT),
        "the note body must never reach the Debug output: {rendered}"
    );
    assert!(
        !rendered.contains(&base64_of(PLAINTEXT)),
        "the encoded note body must never reach the Debug output: {rendered}"
    );
}

#[test]
fn the_debug_output_never_carries_the_archive_bytes() {
    let state = cryptonote::state::TemporaryState {
        external: cryptonote::External::Archive(
            cryptonote::crypto::ExternalArchive::new(PLAINTEXT.as_bytes().to_vec()).infallible(),
        ),
        ..Default::default()
    };

    let rendered = format!("{state:?}");

    let archive_bytes = PLAINTEXT
        .as_bytes()
        .iter()
        .map(u8::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    assert!(
        !rendered.contains(&archive_bytes),
        "the archive bytes must never reach the Debug output: {rendered}"
    );
}
