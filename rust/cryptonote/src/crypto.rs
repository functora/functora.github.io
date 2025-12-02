use aes_gcm::{
    Aes256Gcm, Nonce,
    aead::{Aead, KeyInit, OsRng},
};
use chacha20poly1305::{ChaCha20Poly1305, Key};
use hkdf::Hkdf;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use x25519_dalek::{
    EphemeralSecret, PublicKey, StaticSecret,
};

const NONCE_SIZE: usize = 12;
const SALT_SIZE: usize = 32;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
)]
pub enum CipherType {
    ChaCha20Poly1305,
    Aes256Gcm,
}

#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize,
)]
pub enum EncryptionMode {
    None,
    Symmetric { cipher: CipherType },
    Asymmetric { cipher: CipherType },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EncryptedData {
    pub mode: EncryptionMode,
    pub nonce: Vec<u8>,
    pub ciphertext: Vec<u8>,
    pub salt: Option<Vec<u8>>,
    pub public_key: Option<Vec<u8>>,
}

pub fn derive_key(
    password: &str,
    salt: &[u8],
    cipher: CipherType,
) -> Vec<u8> {
    let key_len = match cipher {
        CipherType::ChaCha20Poly1305 => 32,
        CipherType::Aes256Gcm => 32,
    };
    let hk = Hkdf::<Sha256>::new(
        Some(salt),
        password.as_bytes(),
    );
    (0..key_len)
        .scan(vec![0u8; key_len], |key, _| {
            hk.expand(b"cryptonote-key", key).ok()?;
            Some(key.clone())
        })
        .last()
        .unwrap_or_else(|| vec![0u8; key_len])
}

pub fn encrypt_symmetric(
    plaintext: &[u8],
    password: &str,
    cipher: CipherType,
) -> Result<EncryptedData, String> {
    let mut salt = vec![0u8; SALT_SIZE];
    getrandom::getrandom(&mut salt).map_err(|e| {
        format!("Random generation failed: {}", e)
    })?;
    let key = derive_key(password, &salt, cipher);
    let mut nonce = vec![0u8; NONCE_SIZE];
    getrandom::getrandom(&mut nonce).map_err(|e| {
        format!("Random generation failed: {}", e)
    })?;

    let ciphertext = match cipher {
        CipherType::ChaCha20Poly1305 => {
            let key_array = Key::from_slice(&key);
            let cipher = ChaCha20Poly1305::new(key_array);
            let nonce_array =
                chacha20poly1305::Nonce::from_slice(&nonce);
            cipher.encrypt(nonce_array, plaintext).map_err(
                |e| {
                    format!(
                        "ChaCha20 encryption failed: {}",
                        e
                    )
                },
            )?
        }
        CipherType::Aes256Gcm => {
            let cipher = Aes256Gcm::new_from_slice(&key)
                .map_err(|e| {
                    format!(
                        "AES key creation failed: {}",
                        e
                    )
                })?;
            let nonce_array = Nonce::from_slice(&nonce);
            cipher.encrypt(nonce_array, plaintext).map_err(
                |e| format!("AES encryption failed: {}", e),
            )?
        }
    };

    Ok(EncryptedData {
        mode: EncryptionMode::Symmetric { cipher },
        nonce,
        ciphertext,
        salt: Some(salt),
        public_key: None,
    })
}

pub fn decrypt_symmetric(
    data: &EncryptedData,
    password: &str,
) -> Result<Vec<u8>, String> {
    let cipher = match data.mode {
        EncryptionMode::Symmetric { cipher } => cipher,
        _ => {
            return Err(
                "Invalid encryption mode".to_string()
            );
        }
    };

    let salt = data
        .salt
        .as_ref()
        .ok_or("Missing salt for symmetric decryption")?;
    let key = derive_key(password, salt, cipher);

    match cipher {
        CipherType::ChaCha20Poly1305 => {
            let key_array = Key::from_slice(&key);
            let cipher = ChaCha20Poly1305::new(key_array);
            let nonce_array =
                chacha20poly1305::Nonce::from_slice(
                    &data.nonce,
                );
            cipher
                .decrypt(
                    nonce_array,
                    data.ciphertext.as_ref(),
                )
                .map_err(|e| {
                    format!(
                        "ChaCha20 decryption failed: {}",
                        e
                    )
                })
        }
        CipherType::Aes256Gcm => {
            let cipher = Aes256Gcm::new_from_slice(&key)
                .map_err(|e| {
                    format!(
                        "AES key creation failed: {}",
                        e
                    )
                })?;
            let nonce_array =
                Nonce::from_slice(&data.nonce);
            cipher
                .decrypt(
                    nonce_array,
                    data.ciphertext.as_ref(),
                )
                .map_err(|e| {
                    format!("AES decryption failed: {}", e)
                })
        }
    }
}

pub fn encrypt_asymmetric(
    plaintext: &[u8],
    recipient_public_key: &[u8],
    cipher: CipherType,
) -> Result<EncryptedData, String> {
    let recipient_pk = PublicKey::from(
        <[u8; 32]>::try_from(recipient_public_key)
            .map_err(|_| "Invalid public key length")?,
    );
    let ephemeral_secret =
        EphemeralSecret::random_from_rng(OsRng);
    let ephemeral_public =
        PublicKey::from(&ephemeral_secret);
    let shared_secret =
        ephemeral_secret.diffie_hellman(&recipient_pk);

    let mut nonce = vec![0u8; NONCE_SIZE];
    getrandom::getrandom(&mut nonce).map_err(|e| {
        format!("Random generation failed: {}", e)
    })?;

    let ciphertext = match cipher {
        CipherType::ChaCha20Poly1305 => {
            let key =
                Key::from_slice(shared_secret.as_bytes());
            let cipher = ChaCha20Poly1305::new(key);
            let nonce_array =
                chacha20poly1305::Nonce::from_slice(&nonce);
            cipher.encrypt(nonce_array, plaintext).map_err(
                |e| {
                    format!(
                        "ChaCha20 encryption failed: {}",
                        e
                    )
                },
            )?
        }
        CipherType::Aes256Gcm => {
            let cipher = Aes256Gcm::new_from_slice(
                shared_secret.as_bytes(),
            )
            .map_err(|e| {
                format!("AES key creation failed: {}", e)
            })?;
            let nonce_array = Nonce::from_slice(&nonce);
            cipher.encrypt(nonce_array, plaintext).map_err(
                |e| format!("AES encryption failed: {}", e),
            )?
        }
    };

    Ok(EncryptedData {
        mode: EncryptionMode::Asymmetric { cipher },
        nonce,
        ciphertext,
        salt: None,
        public_key: Some(
            ephemeral_public.as_bytes().to_vec(),
        ),
    })
}

pub fn decrypt_asymmetric(
    data: &EncryptedData,
    private_key: &[u8],
) -> Result<Vec<u8>, String> {
    let cipher = match data.mode {
        EncryptionMode::Asymmetric { cipher } => cipher,
        _ => {
            return Err(
                "Invalid encryption mode".to_string()
            );
        }
    };

    let ephemeral_public = data.public_key.as_ref().ok_or(
        "Missing public key for asymmetric decryption",
    )?;
    let ephemeral_pk = PublicKey::from(
        <[u8; 32]>::try_from(ephemeral_public.as_slice())
            .map_err(
            |_| "Invalid ephemeral public key length",
        )?,
    );

    let secret = StaticSecret::from(
        <[u8; 32]>::try_from(private_key)
            .map_err(|_| "Invalid private key length")?,
    );
    let shared_secret =
        secret.diffie_hellman(&ephemeral_pk);

    match cipher {
        CipherType::ChaCha20Poly1305 => {
            let key =
                Key::from_slice(shared_secret.as_bytes());
            let cipher = ChaCha20Poly1305::new(key);
            let nonce_array =
                chacha20poly1305::Nonce::from_slice(
                    &data.nonce,
                );
            cipher
                .decrypt(
                    nonce_array,
                    data.ciphertext.as_ref(),
                )
                .map_err(|e| {
                    format!(
                        "ChaCha20 decryption failed: {}",
                        e
                    )
                })
        }
        CipherType::Aes256Gcm => {
            let cipher = Aes256Gcm::new_from_slice(
                shared_secret.as_bytes(),
            )
            .map_err(|e| {
                format!("AES key creation failed: {}", e)
            })?;
            let nonce_array =
                Nonce::from_slice(&data.nonce);
            cipher
                .decrypt(
                    nonce_array,
                    data.ciphertext.as_ref(),
                )
                .map_err(|e| {
                    format!("AES decryption failed: {}", e)
                })
        }
    }
}

pub fn generate_keypair() -> (Vec<u8>, Vec<u8>) {
    let secret = StaticSecret::random_from_rng(OsRng);
    let public = PublicKey::from(&secret);
    (secret.to_bytes().to_vec(), public.as_bytes().to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symmetric_chacha20_roundtrip() {
        let plaintext = b"Hello, Cryptonote!";
        let password = "test_password_123";
        let encrypted = encrypt_symmetric(
            plaintext,
            password,
            CipherType::ChaCha20Poly1305,
        )
        .expect("Encryption failed");
        let decrypted =
            decrypt_symmetric(&encrypted, password)
                .expect("Decryption failed");
        assert_eq!(plaintext.to_vec(), decrypted);
    }

    #[test]
    fn test_symmetric_aes_roundtrip() {
        let plaintext = b"Secret message with AES";
        let password = "strong_password";
        let encrypted = encrypt_symmetric(
            plaintext,
            password,
            CipherType::Aes256Gcm,
        )
        .expect("Encryption failed");
        let decrypted =
            decrypt_symmetric(&encrypted, password)
                .expect("Decryption failed");
        assert_eq!(plaintext.to_vec(), decrypted);
    }

    #[test]
    fn test_symmetric_wrong_password() {
        let plaintext = b"Test data";
        let encrypted = encrypt_symmetric(
            plaintext,
            "correct",
            CipherType::ChaCha20Poly1305,
        )
        .expect("Encryption failed");
        let result = decrypt_symmetric(&encrypted, "wrong");
        assert!(result.is_err());
    }

    #[test]
    fn test_asymmetric_chacha20_roundtrip() {
        let plaintext = b"Asymmetric encryption test";
        let (private_key, public_key) = generate_keypair();
        let encrypted = encrypt_asymmetric(
            plaintext,
            &public_key,
            CipherType::ChaCha20Poly1305,
        )
        .expect("Encryption failed");
        let decrypted =
            decrypt_asymmetric(&encrypted, &private_key)
                .expect("Decryption failed");
        assert_eq!(plaintext.to_vec(), decrypted);
    }

    #[test]
    fn test_asymmetric_aes_roundtrip() {
        let plaintext = b"X25519 + AES test";
        let (private_key, public_key) = generate_keypair();
        let encrypted = encrypt_asymmetric(
            plaintext,
            &public_key,
            CipherType::Aes256Gcm,
        )
        .expect("Encryption failed");
        let decrypted =
            decrypt_asymmetric(&encrypted, &private_key)
                .expect("Decryption failed");
        assert_eq!(plaintext.to_vec(), decrypted);
    }

    #[test]
    fn test_key_derivation_consistency() {
        let password = "test";
        let salt = vec![1u8; SALT_SIZE];
        let key1 = derive_key(
            password,
            &salt,
            CipherType::ChaCha20Poly1305,
        );
        let key2 = derive_key(
            password,
            &salt,
            CipherType::ChaCha20Poly1305,
        );
        assert_eq!(key1, key2);
    }
}
