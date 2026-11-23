use base64::Engine;
use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use chacha20poly1305::aead::{Aead, KeyInit, Payload};
use chacha20poly1305::{ChaCha20Poly1305, Key, Nonce};
use functora_egui::storage::Storage;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use zeroize::Zeroizing;

use crate::error::{Error, Result};

const VAULT_DATA_KEY: &str = "cryptogram_vault_data";
const VAULT_KEY_KEY: &str = "cryptogram_vault_key";
const VAULT_MANIFEST_KEY: &str = "cryptogram_vault_manifest";
const VAULT_INDEX_KEY: &str = "cryptogram_vault_chunk_index";
const VAULT_CHUNK_PREFIX: &str = "cryptogram_vault_chunk.";
const VAULT_AAD: &[u8] = b"cryptogram encrypted vault v1";
const NONCE_BYTES: usize = 12;
const TAG_BYTES: usize = 16;
const KEY_BYTES: usize = 32;
const KEY_ENCODED_BYTES: usize = 43;
const GENERATION_BYTES: usize = 16;
const GENERATION_ENCODED_BYTES: usize = 22;
const CHECKSUM_ENCODED_BYTES: usize = 43;
const MAX_VAULT_PLAINTEXT_BYTES: usize = 512 * 1024;
const MAX_VAULT_ENCODED_BYTES: usize = 1024 * 1024;
const MAX_VAULT_CHUNK_BYTES: usize = 2048;
const MAX_VAULT_CHUNKS: usize = 512;
const MAX_VAULT_GENERATIONS: usize = 64;

#[derive(Debug, Clone, Copy)]
pub struct VaultStore {
    storage: Storage,
}

#[derive(Clone, Copy)]
enum KeyLocation {
    Secure,
    Local,
}

#[derive(Clone)]
enum SecureState {
    Available(Option<String>),
    Unsupported,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ChunkManifest {
    generation: String,
    chunks: usize,
    checksum: String,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ChunkGeneration {
    id: String,
    chunks: usize,
}

#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ChunkIndex {
    generations: Vec<ChunkGeneration>,
}

impl VaultStore {
    #[must_use]
    pub fn new() -> Self {
        Self {
            storage: Storage::new("cryptogram"),
        }
    }

    #[must_use]
    pub fn with_storage(storage: Storage) -> Self {
        Self { storage }
    }

    pub async fn load(&self) -> Result<Option<Vec<u8>>> {
        let secure_key_state = secure_state(VAULT_KEY_KEY).await?;
        let local_key = self.local_string(VAULT_KEY_KEY)?;
        let secure_key = match &secure_key_state {
            SecureState::Available(Some(key)) => Some(key.clone()),
            SecureState::Available(None) | SecureState::Unsupported => None,
        };
        let key = secure_key.clone().or_else(|| local_key.clone());
        let manifest_value = self.read_manifest()?;
        let legacy_secure_data = self.secure_legacy_data().await?;
        let legacy_local_data = self.local_string(VAULT_DATA_KEY)?;
        let encrypted_candidate = match &manifest_value {
            Some(manifest_data) => Some(self.read_chunk_data(manifest_data)?),
            None => legacy_secure_data
                .clone()
                .or_else(|| legacy_local_data.clone()),
        };
        let Some(encrypted_value) = encrypted_candidate else {
            self.migrate_key_if_possible(secure_key.is_some(), local_key.as_deref())
                .await?;
            return Ok(None);
        };
        let Some(key_value) = key else {
            return Err(Error::Vault("encrypted vault has no key".to_owned()));
        };
        let plaintext = decrypt_blob(&key_value, &encrypted_value)?;
        let legacy_present = legacy_secure_data.is_some() || legacy_local_data.is_some();
        if manifest_value.is_none() || legacy_present {
            self.write_chunk_data(&encrypted_value)?;
            self.remove_legacy_data().await?;
        }
        self.migrate_key_if_possible(secure_key.is_some(), local_key.as_deref())
            .await?;
        Ok(Some(plaintext))
    }

    pub async fn save(&self, plaintext: &[u8]) -> Result<()> {
        if plaintext.len() > MAX_VAULT_PLAINTEXT_BYTES {
            return Err(Error::Vault("vault payload is too large".to_owned()));
        }
        let (key, location) = self.ensure_key().await?;
        let encrypted = encrypt_blob(&key, plaintext)?;
        self.write_chunk_data(&encrypted)?;
        self.remove_legacy_data().await?;
        match location {
            KeyLocation::Secure => Ok(self.storage.try_remove(VAULT_KEY_KEY)?),
            KeyLocation::Local => Ok(self.storage.try_persist(VAULT_KEY_KEY, &key)?),
        }
    }

    pub async fn clear(&self) -> Result<()> {
        let manifest = self.read_manifest()?;
        let index = self.read_index()?;
        manifest
            .iter()
            .map(|entry| self.remove_generation(&entry.generation, entry.chunks))
            .collect::<Result<Vec<_>>>()
            .map(|_| ())?;
        index
            .generations
            .iter()
            .map(|entry| self.remove_generation(&entry.id, entry.chunks))
            .collect::<Result<Vec<_>>>()
            .map(|_| ())?;
        self.storage.try_remove(VAULT_MANIFEST_KEY)?;
        self.storage.try_remove(VAULT_INDEX_KEY)?;
        remove_secure_item(VAULT_DATA_KEY).await?;
        self.storage.try_remove(VAULT_DATA_KEY)?;
        remove_secure_item(VAULT_KEY_KEY).await?;
        self.storage.try_remove(VAULT_KEY_KEY)?;
        Ok(())
    }

    async fn ensure_key(&self) -> Result<(String, KeyLocation)> {
        let secure_state = secure_state(VAULT_KEY_KEY).await?;
        let local_key = self.local_string(VAULT_KEY_KEY)?;
        match secure_state {
            SecureState::Available(Some(key)) => Ok((key, KeyLocation::Secure)),
            SecureState::Available(None) => {
                if let Some(key) = local_key {
                    return self
                        .migrate_local_key(&key)
                        .await
                        .map(|location| (key, location));
                }
                let key = URL_SAFE_NO_PAD.encode(random_key()?);
                match secure_set(VAULT_KEY_KEY, &key).await {
                    Ok(()) => Ok((key, KeyLocation::Secure)),
                    Err(Error::Unsupported(_)) => {
                        self.storage.try_persist(VAULT_KEY_KEY, &key)?;
                        Ok((key, KeyLocation::Local))
                    }
                    Err(error) => Err(error),
                }
            }
            SecureState::Unsupported => {
                if let Some(key) = local_key {
                    return Ok((key, KeyLocation::Local));
                }
                let key = URL_SAFE_NO_PAD.encode(random_key()?);
                self.storage.try_persist(VAULT_KEY_KEY, &key)?;
                Ok((key, KeyLocation::Local))
            }
        }
    }

    async fn migrate_key_if_possible(
        &self,
        secure_key_present: bool,
        local_key: Option<&str>,
    ) -> Result<()> {
        if secure_key_present {
            if local_key.is_some() {
                self.storage.try_remove(VAULT_KEY_KEY)?;
            }
            return Ok(());
        }
        let Some(key) = local_key else {
            return Ok(());
        };
        match secure_state(VAULT_KEY_KEY).await? {
            SecureState::Available(Some(_)) | SecureState::Unsupported => Ok(()),
            SecureState::Available(None) => match secure_set(VAULT_KEY_KEY, key).await {
                Ok(()) => Ok(self.storage.try_remove(VAULT_KEY_KEY)?),
                Err(Error::Unsupported(_)) => Ok(()),
                Err(error) => Err(error),
            },
        }
    }

    async fn migrate_local_key(&self, key: &str) -> Result<KeyLocation> {
        match secure_state(VAULT_KEY_KEY).await? {
            SecureState::Available(Some(_)) => Ok(KeyLocation::Secure),
            SecureState::Available(None) => match secure_set(VAULT_KEY_KEY, key).await {
                Ok(()) => {
                    self.storage.try_remove(VAULT_KEY_KEY)?;
                    Ok(KeyLocation::Secure)
                }
                Err(Error::Unsupported(_)) => Ok(KeyLocation::Local),
                Err(error) => Err(error),
            },
            SecureState::Unsupported => Ok(KeyLocation::Local),
        }
    }

    async fn secure_legacy_data(&self) -> Result<Option<String>> {
        match secure_state(VAULT_DATA_KEY).await? {
            SecureState::Available(Some(value)) if !value.is_empty() => Ok(Some(value)),
            SecureState::Available(Some(_)) => {
                remove_secure_item(VAULT_DATA_KEY).await?;
                Ok(None)
            }
            SecureState::Available(None) | SecureState::Unsupported => Ok(None),
        }
    }

    async fn remove_legacy_data(&self) -> Result<()> {
        remove_secure_item(VAULT_DATA_KEY).await?;
        Ok(self.storage.try_remove(VAULT_DATA_KEY)?)
    }

    fn local_string(&self, key: &str) -> Result<Option<String>> {
        match self.storage.try_load::<String>(key)? {
            Some(value) if value.is_empty() => {
                self.storage.try_remove(key)?;
                Ok(None)
            }
            Some(value) => Ok(Some(value)),
            None => Ok(None),
        }
    }

    fn read_manifest(&self) -> Result<Option<ChunkManifest>> {
        let Some(manifest) = self.storage.try_load::<ChunkManifest>(VAULT_MANIFEST_KEY)? else {
            return Ok(None);
        };
        validate_manifest(&manifest)?;
        Ok(Some(manifest))
    }

    fn read_index(&self) -> Result<ChunkIndex> {
        let Some(index) = self.storage.try_load::<ChunkIndex>(VAULT_INDEX_KEY)? else {
            return Ok(ChunkIndex::default());
        };
        validate_index(&index)?;
        Ok(index)
    }

    fn read_chunk_data(&self, manifest: &ChunkManifest) -> Result<String> {
        let chunks = (0..manifest.chunks)
            .map(|index| {
                let chunk = self
                    .storage
                    .try_load::<String>(&chunk_key(&manifest.generation, index))?
                    .ok_or_else(|| Error::Vault("vault chunk is missing".to_owned()))?;
                if chunk.is_empty() || chunk.len() > MAX_VAULT_CHUNK_BYTES {
                    return Err(Error::Vault("vault chunk has an invalid length".to_owned()));
                }
                Ok(chunk)
            })
            .collect::<Result<Vec<_>>>()?;
        let encoded = chunks.into_iter().collect::<String>();
        if encoded.len() > MAX_VAULT_ENCODED_BYTES {
            return Err(Error::Vault("vault payload is too large".to_owned()));
        }
        let checksum = URL_SAFE_NO_PAD.encode(Sha256::digest(encoded.as_bytes()));
        if checksum != manifest.checksum {
            return Err(Error::Vault("vault payload checksum mismatch".to_owned()));
        }
        Ok(encoded)
    }

    fn write_chunk_data(&self, encoded: &str) -> Result<()> {
        if encoded.is_empty() || encoded.len() > MAX_VAULT_ENCODED_BYTES {
            return Err(Error::Vault(
                "vault payload has an invalid length".to_owned(),
            ));
        }
        let decoded = URL_SAFE_NO_PAD
            .decode(encoded)
            .map_err(|_| Error::Vault("vault payload is not base64url".to_owned()))?;
        if decoded.len() > MAX_VAULT_PLAINTEXT_BYTES + NONCE_BYTES + TAG_BYTES
            || URL_SAFE_NO_PAD.encode(&decoded) != encoded
        {
            return Err(Error::Vault("vault payload is not canonical".to_owned()));
        }
        let chunks = encoded
            .as_bytes()
            .chunks(MAX_VAULT_CHUNK_BYTES)
            .map(|chunk| {
                std::str::from_utf8(chunk)
                    .map(str::to_owned)
                    .map_err(|_| Error::Vault("vault payload is not text".to_owned()))
            })
            .collect::<Result<Vec<_>>>()?;
        if chunks.is_empty() || chunks.len() > MAX_VAULT_CHUNKS {
            return Err(Error::Vault("vault chunk count is invalid".to_owned()));
        }
        let generation = random_generation()?;
        let old_manifest = self.read_manifest()?;
        let mut index = self.read_index()?;
        if let Some(manifest) = &old_manifest
            && !index
                .generations
                .iter()
                .any(|entry| entry.id == manifest.generation)
        {
            index.generations.push(ChunkGeneration {
                id: manifest.generation.clone(),
                chunks: manifest.chunks,
            });
        }
        let old_generation = old_manifest
            .as_ref()
            .map(|manifest| manifest.generation.as_str());
        let stale = index
            .generations
            .iter()
            .filter(|entry| Some(entry.id.as_str()) != old_generation)
            .cloned()
            .collect::<Vec<_>>();
        stale
            .iter()
            .map(|entry| self.remove_generation(&entry.id, entry.chunks))
            .collect::<Result<Vec<_>>>()
            .map(|_| ())?;
        index
            .generations
            .retain(|entry| Some(entry.id.as_str()) == old_generation);
        index.generations.push(ChunkGeneration {
            id: generation.clone(),
            chunks: chunks.len(),
        });
        if index.generations.len() > MAX_VAULT_GENERATIONS {
            return Err(Error::Vault(
                "vault generation index is too large".to_owned(),
            ));
        }
        self.storage
            .try_persist(VAULT_INDEX_KEY, &index)
            .map_err(|error| Error::Vault(format!("cannot persist vault index: {error}")))?;
        let write_result = chunks
            .iter()
            .enumerate()
            .try_for_each(|(chunk_index, chunk)| {
                self.storage
                    .try_persist(&chunk_key(&generation, chunk_index), chunk)
            });
        if let Err(error) = write_result {
            let cleanup = self.remove_generation(&generation, chunks.len());
            return Err(match cleanup {
                Ok(()) => Error::Vault(format!("cannot persist vault chunks: {error}")),
                Err(cleanup_error) => Error::Vault(format!(
                    "cannot persist vault chunks: {error}; cleanup failed: {cleanup_error}"
                )),
            });
        }
        let manifest = ChunkManifest {
            generation: generation.clone(),
            chunks: chunks.len(),
            checksum: URL_SAFE_NO_PAD.encode(Sha256::digest(encoded.as_bytes())),
        };
        self.storage
            .try_persist(VAULT_MANIFEST_KEY, &manifest)
            .map_err(|error| Error::Vault(format!("cannot persist vault manifest: {error}")))?;
        let cleanup = index
            .generations
            .iter()
            .filter(|entry| entry.id != generation)
            .map(|entry| self.remove_generation(&entry.id, entry.chunks))
            .collect::<Result<Vec<_>>>()
            .map(|_| ());
        let mut final_index = index;
        final_index
            .generations
            .retain(|entry| entry.id == generation);
        let index_result = self
            .storage
            .try_persist(VAULT_INDEX_KEY, &final_index)
            .map_err(Error::from);
        match (cleanup, index_result) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(error), _) | (_, Err(error)) => Err(Error::Vault(format!(
                "cannot clean vault generations: {error}"
            ))),
        }
    }

    fn remove_generation(&self, generation: &str, chunks: usize) -> Result<()> {
        Ok((0..chunks)
            .try_for_each(|index| self.storage.try_remove(&chunk_key(generation, index)))?)
    }
}

impl Default for VaultStore {
    fn default() -> Self {
        Self::new()
    }
}

fn encrypt_blob(key: &str, plaintext: &[u8]) -> Result<String> {
    if plaintext.len() > MAX_VAULT_PLAINTEXT_BYTES {
        return Err(Error::Vault("vault payload is too large".to_owned()));
    }
    let key_bytes = decode_key(key)?;
    let mut nonce = [0_u8; NONCE_BYTES];
    getrandom::fill(&mut nonce).map_err(Error::Random)?;
    let cipher = ChaCha20Poly1305::new(Key::from_slice(key_bytes.as_slice()));
    let ciphertext = cipher
        .encrypt(
            Nonce::from_slice(&nonce),
            Payload {
                msg: plaintext,
                aad: VAULT_AAD,
            },
        )
        .map_err(Error::OuterCrypto)?;
    let mut envelope = nonce.to_vec();
    envelope.extend_from_slice(&ciphertext);
    Ok(URL_SAFE_NO_PAD.encode(envelope))
}

fn decrypt_blob(key: &str, encoded: &str) -> Result<Vec<u8>> {
    if encoded.is_empty() || encoded.len() > MAX_VAULT_ENCODED_BYTES {
        return Err(Error::Vault(
            "vault payload has an invalid length".to_owned(),
        ));
    }
    let key_bytes = decode_key(key)?;
    let envelope = URL_SAFE_NO_PAD
        .decode(encoded)
        .map_err(|error| Error::Vault(format!("vault payload is not base64url: {error}")))?;
    if URL_SAFE_NO_PAD.encode(&envelope) != encoded {
        return Err(Error::Vault("vault payload is not canonical".to_owned()));
    }
    if envelope.len() < NONCE_BYTES + TAG_BYTES {
        return Err(Error::Vault("vault payload is too short".to_owned()));
    }
    if envelope.len() > MAX_VAULT_PLAINTEXT_BYTES + NONCE_BYTES + TAG_BYTES {
        return Err(Error::Vault("vault payload is too large".to_owned()));
    }
    let (nonce, ciphertext) = envelope.split_at(NONCE_BYTES);
    let cipher = ChaCha20Poly1305::new(Key::from_slice(key_bytes.as_slice()));
    let plaintext = cipher
        .decrypt(
            Nonce::from_slice(nonce),
            Payload {
                msg: ciphertext,
                aad: VAULT_AAD,
            },
        )
        .map_err(Error::OuterCrypto)?;
    if plaintext.len() > MAX_VAULT_PLAINTEXT_BYTES {
        return Err(Error::Vault("vault payload is too large".to_owned()));
    }
    Ok(plaintext)
}

fn decode_key(encoded: &str) -> Result<Zeroizing<Vec<u8>>> {
    if encoded.len() != KEY_ENCODED_BYTES {
        return Err(Error::Vault("vault key has an invalid length".to_owned()));
    }
    let key = URL_SAFE_NO_PAD
        .decode(encoded)
        .map_err(|error| Error::Vault(format!("vault key is not base64url: {error}")))?;
    if key.len() != KEY_BYTES || URL_SAFE_NO_PAD.encode(&key) != encoded {
        return Err(Error::Vault("vault key is not canonical".to_owned()));
    }
    Ok(Zeroizing::new(key))
}

fn random_key() -> Result<Zeroizing<Vec<u8>>> {
    let mut key = Zeroizing::new(vec![0_u8; KEY_BYTES]);
    getrandom::fill(key.as_mut_slice()).map_err(Error::Random)?;
    Ok(key)
}

fn random_generation() -> Result<String> {
    let mut generation = [0_u8; GENERATION_BYTES];
    getrandom::fill(&mut generation).map_err(Error::Random)?;
    Ok(URL_SAFE_NO_PAD.encode(generation))
}

fn chunk_key(generation: &str, index: usize) -> String {
    format!("{VAULT_CHUNK_PREFIX}{generation}.{index}")
}

fn validate_manifest(manifest: &ChunkManifest) -> Result<()> {
    validate_generation(&manifest.generation)?;
    if manifest.chunks == 0 || manifest.chunks > MAX_VAULT_CHUNKS {
        return Err(Error::Vault(
            "vault manifest has an invalid chunk count".to_owned(),
        ));
    }
    if manifest.checksum.len() != CHECKSUM_ENCODED_BYTES {
        return Err(Error::Vault(
            "vault manifest checksum is invalid".to_owned(),
        ));
    }
    let checksum = URL_SAFE_NO_PAD
        .decode(&manifest.checksum)
        .map_err(|_| Error::Vault("vault manifest checksum is invalid".to_owned()))?;
    if checksum.len() != 32 || URL_SAFE_NO_PAD.encode(checksum) != manifest.checksum {
        return Err(Error::Vault(
            "vault manifest checksum is invalid".to_owned(),
        ));
    }
    Ok(())
}

fn validate_generation(generation: &str) -> Result<()> {
    if generation.len() != GENERATION_ENCODED_BYTES {
        return Err(Error::Vault("vault generation is invalid".to_owned()));
    }
    let decoded = URL_SAFE_NO_PAD
        .decode(generation)
        .map_err(|_| Error::Vault("vault generation is invalid".to_owned()))?;
    if decoded.len() != GENERATION_BYTES || URL_SAFE_NO_PAD.encode(decoded) != generation {
        return Err(Error::Vault("vault generation is invalid".to_owned()));
    }
    Ok(())
}

fn validate_index(index: &ChunkIndex) -> Result<()> {
    if index.generations.len() > MAX_VAULT_GENERATIONS {
        return Err(Error::Vault(
            "vault generation index is too large".to_owned(),
        ));
    }
    index.generations.iter().try_for_each(|entry| {
        validate_generation(&entry.id)?;
        if entry.chunks == 0 || entry.chunks > MAX_VAULT_CHUNKS {
            return Err(Error::Vault(
                "vault generation has an invalid chunk count".to_owned(),
            ));
        }
        Ok(())
    })
}

async fn secure_state(key: &str) -> Result<SecureState> {
    match secure_get(key).await {
        Ok(Some(value)) if !value.is_empty() => Ok(SecureState::Available(Some(value))),
        Ok(Some(_) | None) => Ok(SecureState::Available(None)),
        Err(Error::Unsupported(_)) => Ok(SecureState::Unsupported),
        Err(error) => Err(error),
    }
}

async fn remove_secure_item(key: &str) -> Result<()> {
    match secure_remove(key).await {
        Ok(()) | Err(Error::Unsupported(_)) => Ok(()),
        Err(error) => Err(error),
    }
}

#[cfg(target_arch = "wasm32")]
async fn secure_get(key: &str) -> Result<Option<String>> {
    crate::web::secure_get(key).await
}

#[cfg(not(target_arch = "wasm32"))]
async fn secure_get(_key: &str) -> Result<Option<String>> {
    std::future::ready(Err(Error::Unsupported(
        "Telegram SecureStorage is available only in the web build",
    )))
    .await
}

#[cfg(target_arch = "wasm32")]
async fn secure_set(key: &str, value: &str) -> Result<()> {
    crate::web::secure_set(key, value).await
}

#[cfg(not(target_arch = "wasm32"))]
async fn secure_set(_key: &str, _value: &str) -> Result<()> {
    std::future::ready(Err(Error::Unsupported(
        "Telegram SecureStorage is available only in the web build",
    )))
    .await
}

#[cfg(target_arch = "wasm32")]
async fn secure_remove(key: &str) -> Result<()> {
    crate::web::secure_remove(key).await
}

#[cfg(not(target_arch = "wasm32"))]
async fn secure_remove(_key: &str) -> Result<()> {
    std::future::ready(Err(Error::Unsupported(
        "Telegram SecureStorage is available only in the web build",
    )))
    .await
}
