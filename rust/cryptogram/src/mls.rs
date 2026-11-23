use std::collections::BTreeMap;
use std::fmt;

use base64::Engine;
use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use openmls::credentials::{BasicCredential, CredentialWithKey};
use openmls::framing::{MlsMessageBodyIn, MlsMessageIn, MlsMessageOut, ProcessedMessageContent};
use openmls::group::{GroupId, MlsGroup, MlsGroupCreateConfig, MlsGroupJoinConfig, StagedWelcome};
use openmls::key_packages::KeyPackage;
use openmls::prelude::{Ciphersuite, ProtocolVersion};
use openmls::treesync::RatchetTreeIn;
use openmls_basic_credential::SignatureKeyPair;
use openmls_memory_storage::MemoryStorage;
use openmls_rust_crypto::RustCrypto;
use openmls_traits::OpenMlsProvider as _;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use tls_codec::{
    Deserialize as TlsDeserialize, DeserializeBytes as TlsDeserializeBytes,
    Serialize as TlsSerialize,
};
use zeroize::Zeroize;

use crate::error::{Error, Result};
use crate::protocol::{ChatMessage, FrameKind, JoinBundle, JoinId, encode_frame};

pub const CIPHERSUITE: Ciphersuite = Ciphersuite::MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519;

#[derive(Default)]
pub struct LocalProvider {
    crypto: RustCrypto,
    storage: MemoryStorage,
}

impl fmt::Debug for LocalProvider {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("LocalProvider")
            .field("crypto", &"[redacted]")
            .field("storage", &"[redacted]")
            .finish()
    }
}

impl openmls_traits::OpenMlsProvider for LocalProvider {
    type CryptoProvider = RustCrypto;
    type RandProvider = RustCrypto;
    type StorageProvider = MemoryStorage;

    fn storage(&self) -> &Self::StorageProvider {
        &self.storage
    }

    fn crypto(&self) -> &Self::CryptoProvider {
        &self.crypto
    }

    fn rand(&self) -> &Self::RandProvider {
        &self.crypto
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct StorageEntry {
    pub key: Vec<u8>,
    pub value: Vec<u8>,
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct StorageSnapshot {
    pub entries: Vec<StorageEntry>,
}

impl LocalProvider {
    pub fn snapshot(&self) -> Result<StorageSnapshot> {
        let values = self
            .storage
            .values
            .read()
            .map_err(|_| Error::Vault("MLS storage lock is poisoned".to_owned()))?;
        let mut entries = values
            .iter()
            .map(|(key, value)| StorageEntry {
                key: key.clone(),
                value: value.clone(),
            })
            .collect::<Vec<_>>();
        entries.sort_by(|left, right| left.key.cmp(&right.key));
        Ok(StorageSnapshot { entries })
    }

    pub fn restore(snapshot: StorageSnapshot) -> Result<Self> {
        let provider = Self::default();
        {
            let mut values = provider
                .storage
                .values
                .write()
                .map_err(|_| Error::Vault("MLS storage lock is poisoned".to_owned()))?;
            values.clear();
            values.extend(
                snapshot
                    .entries
                    .into_iter()
                    .map(|entry| (entry.key, entry.value)),
            );
        }
        Ok(provider)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroupSnapshot {
    pub id: Vec<u8>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ClientSnapshot {
    pub identity: Vec<u8>,
    pub credential: CredentialWithKey,
    pub signer: SignatureKeyPair,
    pub storage: StorageSnapshot,
    pub groups: Vec<GroupSnapshot>,
}

pub struct Client {
    provider: LocalProvider,
    signer: SignatureKeyPair,
    credential: CredentialWithKey,
    identity: Vec<u8>,
    groups: BTreeMap<Vec<u8>, MlsGroup>,
}

impl fmt::Debug for Client {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Client")
            .field("provider", &self.provider)
            .field("signer", &"[redacted]")
            .field("credential", &"[redacted]")
            .field("identity", &"[redacted]")
            .field("groups", &self.groups.len())
            .finish()
    }
}

impl Client {
    pub fn new(identity: Vec<u8>) -> Result<Self> {
        if identity.is_empty() {
            return Err(Error::Mls("MLS identity must not be empty".to_owned()));
        }
        let provider = LocalProvider::default();
        let signer = SignatureKeyPair::new(CIPHERSUITE.signature_algorithm())
            .map_err(|error| Error::Mls(format!("cannot create signer: {error}")))?;
        signer
            .store(provider.storage())
            .map_err(|error| Error::Mls(format!("cannot store signer: {error}")))?;
        let credential = CredentialWithKey {
            credential: BasicCredential::new(identity.clone()).into(),
            signature_key: signer.public().into(),
        };
        Ok(Self {
            provider,
            signer,
            credential,
            identity,
            groups: BTreeMap::new(),
        })
    }

    pub fn random(identity: Vec<u8>) -> Result<Self> {
        Self::new(identity)
    }

    pub fn identity(&self) -> &[u8] {
        &self.identity
    }

    pub fn group_ids(&self) -> Vec<Vec<u8>> {
        self.groups.keys().cloned().collect()
    }

    pub fn group_count(&self) -> usize {
        self.groups.len()
    }

    pub fn create_group(&mut self) -> Result<Vec<u8>> {
        let group = MlsGroup::new(
            &self.provider,
            &self.signer,
            &MlsGroupCreateConfig::default(),
            self.credential.clone(),
        )
        .map_err(|error| Error::Mls(format!("cannot create MLS group: {error}")))?;
        let id = group.group_id().as_slice().to_vec();
        let _previous = self.groups.insert(id.clone(), group);
        Ok(id)
    }

    pub fn key_package(&mut self) -> Result<Vec<u8>> {
        let bundle = KeyPackage::builder()
            .build(
                CIPHERSUITE,
                &self.provider,
                &self.signer,
                self.credential.clone(),
            )
            .map_err(|error| Error::Mls(format!("cannot create MLS key package: {error}")))?;
        MlsMessageOut::from(bundle.key_package().clone())
            .to_bytes()
            .map_err(|error| Error::Mls(format!("cannot serialize MLS key package: {error}")))
    }

    pub fn validate_key_package(&self, bytes: &[u8]) -> Result<KeyPackage> {
        let message = decode_message(bytes)?;
        match message.extract() {
            MlsMessageBodyIn::KeyPackage(key_package) => key_package
                .validate(self.provider.crypto(), ProtocolVersion::default())
                .map_err(|error| Error::Mls(format!("invalid MLS key package: {error}"))),
            MlsMessageBodyIn::PublicMessage(_)
            | MlsMessageBodyIn::PrivateMessage(_)
            | MlsMessageBodyIn::Welcome(_)
            | MlsMessageBodyIn::GroupInfo(_) => {
                Err(Error::Mls("MLS payload is not a key package".to_owned()))
            }
        }
    }

    pub fn add_member(
        &mut self,
        group_id: &[u8],
        key_package_bytes: &[u8],
    ) -> Result<MemberCommit> {
        let key_package = self.validate_key_package(key_package_bytes)?;
        let Self {
            provider,
            signer,
            groups,
            ..
        } = self;
        let group = groups
            .get_mut(group_id)
            .ok_or_else(|| Error::Mls("unknown MLS group".to_owned()))?;
        let (commit, welcome, _) = group
            .add_members(provider, signer, std::slice::from_ref(&key_package))
            .map_err(|error| Error::Mls(format!("cannot add MLS member: {error}")))?;
        group
            .merge_pending_commit(provider)
            .map_err(|error| Error::Mls(format!("cannot merge MLS commit: {error}")))?;
        let tree = RatchetTreeIn::from(group.export_ratchet_tree());
        Ok(MemberCommit {
            commit: commit
                .to_bytes()
                .map_err(|error| Error::Mls(format!("cannot serialize MLS commit: {error}")))?,
            welcome: welcome
                .to_bytes()
                .map_err(|error| Error::Mls(format!("cannot serialize MLS welcome: {error}")))?,
            tree: tree
                .tls_serialize_detached()
                .map_err(|error| Error::Mls(format!("cannot serialize MLS tree: {error}")))?,
        })
    }

    pub fn join_group(&mut self, welcome_bytes: &[u8], tree_bytes: &[u8]) -> Result<Vec<u8>> {
        let welcome = match decode_message(welcome_bytes)?.extract() {
            MlsMessageBodyIn::Welcome(welcome) => welcome,
            MlsMessageBodyIn::PublicMessage(_)
            | MlsMessageBodyIn::PrivateMessage(_)
            | MlsMessageBodyIn::GroupInfo(_)
            | MlsMessageBodyIn::KeyPackage(_) => {
                return Err(Error::Mls("MLS payload is not a welcome".to_owned()));
            }
        };
        let tree = RatchetTreeIn::tls_deserialize_exact_bytes(tree_bytes)
            .map_err(|error| Error::Mls(format!("invalid MLS ratchet tree: {error}")))?;
        let staged = StagedWelcome::new_from_welcome(
            &self.provider,
            &MlsGroupJoinConfig::default(),
            welcome,
            Some(tree),
        )
        .map_err(|error| Error::Mls(format!("cannot stage MLS welcome: {error}")))?;
        let group = staged
            .into_group(&self.provider)
            .map_err(|error| Error::Mls(format!("cannot join MLS group: {error}")))?;
        let id = group.group_id().as_slice().to_vec();
        let _previous = self.groups.insert(id.clone(), group);
        Ok(id)
    }

    pub fn encrypt_application(&mut self, group_id: &[u8], text: &str) -> Result<Vec<u8>> {
        let chat_message = ChatMessage::new(text.to_owned())?;
        let bytes = serde_json::to_vec(&chat_message)?;
        let Self {
            provider,
            signer,
            groups,
            ..
        } = self;
        let group = groups
            .get_mut(group_id)
            .ok_or_else(|| Error::Mls("unknown MLS group".to_owned()))?;
        let mls_message = group
            .create_message(provider, signer, &bytes)
            .map_err(|error| Error::Mls(format!("cannot encrypt MLS message: {error}")))?;
        Ok(encode_frame(
            FrameKind::Application,
            &mls_message
                .to_bytes()
                .map_err(|error| Error::Mls(format!("cannot serialize MLS message: {error}")))?,
        ))
    }

    pub fn process_application(
        &mut self,
        group_id: &[u8],
        message_bytes: &[u8],
    ) -> Result<ChatMessage> {
        self.process_application_optional(group_id, message_bytes)?
            .ok_or_else(|| Error::Mls("MLS private message echo was received".to_owned()))
    }

    pub fn process_application_optional(
        &mut self,
        group_id: &[u8],
        message_bytes: &[u8],
    ) -> Result<Option<ChatMessage>> {
        let protocol_message = decode_protocol_message(message_bytes)?;
        let Self {
            provider, groups, ..
        } = self;
        let group = groups
            .get_mut(group_id)
            .ok_or_else(|| Error::Mls("unknown MLS group".to_owned()))?;
        let processed = group
            .process_message(provider, protocol_message)
            .map_err(|error| Error::Mls(format!("cannot process MLS message: {error}")))?;
        match processed.into_content() {
            ProcessedMessageContent::ApplicationMessage(application_message) => {
                let bytes = application_message.into_bytes();
                serde_json::from_slice(&bytes).map(Some).map_err(|error| {
                    Error::Mls(format!("invalid MLS application payload: {error}"))
                })
            }
            ProcessedMessageContent::OwnPrivateMessage => Ok(None),
            ProcessedMessageContent::StagedCommitMessage(_)
            | ProcessedMessageContent::ProposalMessage(_)
            | ProcessedMessageContent::ExternalJoinProposalMessage(_)
            | ProcessedMessageContent::OwnPendingCommit => Err(Error::Mls(
                "MLS control frame was received in an application frame".to_owned(),
            )),
        }
    }

    pub fn process_commit(&mut self, group_id: &[u8], message_bytes: &[u8]) -> Result<()> {
        let message = decode_protocol_message(message_bytes)?;
        let message_epoch = message.epoch().as_u64();
        let Self {
            provider, groups, ..
        } = self;
        let group = groups
            .get_mut(group_id)
            .ok_or_else(|| Error::Mls("unknown MLS group".to_owned()))?;
        if message_epoch < group.epoch().as_u64() {
            return Ok(());
        }
        let processed = group
            .process_message(provider, message)
            .map_err(|error| Error::Mls(format!("cannot process MLS commit: {error}")))?;
        match processed.into_content() {
            ProcessedMessageContent::StagedCommitMessage(commit) => group
                .merge_staged_commit(provider, *commit)
                .map_err(|error| Error::Mls(format!("cannot merge MLS commit: {error}"))),
            ProcessedMessageContent::OwnPendingCommit => Ok(()),
            ProcessedMessageContent::ApplicationMessage(_) => Err(Error::Mls(
                "MLS application message was received in a commit frame".to_owned(),
            )),
            ProcessedMessageContent::ProposalMessage(_)
            | ProcessedMessageContent::ExternalJoinProposalMessage(_) => Err(Error::Mls(
                "MLS proposal was received in a commit frame".to_owned(),
            )),
            ProcessedMessageContent::OwnPrivateMessage => Err(Error::Mls(
                "MLS private message echo was received".to_owned(),
            )),
        }
    }

    pub fn export_tree(&self, group_id: &[u8]) -> Result<Vec<u8>> {
        let group = self
            .groups
            .get(group_id)
            .ok_or_else(|| Error::Mls("unknown MLS group".to_owned()))?;
        RatchetTreeIn::from(group.export_ratchet_tree())
            .tls_serialize_detached()
            .map_err(|error| Error::Mls(format!("cannot serialize MLS tree: {error}")))
    }

    pub fn snapshot(&self) -> Result<ClientSnapshot> {
        Ok(ClientSnapshot {
            identity: self.identity.clone(),
            credential: self.credential.clone(),
            signer: self.signer.clone(),
            storage: self.provider.snapshot()?,
            groups: self
                .groups
                .keys()
                .map(|id| GroupSnapshot { id: id.clone() })
                .collect(),
        })
    }

    pub fn restore(snapshot: ClientSnapshot) -> Result<Self> {
        let provider = LocalProvider::restore(snapshot.storage)?;
        let mut groups = BTreeMap::new();
        for group in snapshot.groups {
            let group_id = GroupId::from_slice(&group.id);
            let loaded = MlsGroup::load(&provider.storage, &group_id)
                .map_err(|error| Error::Mls(format!("cannot load MLS group: {error}")))?
                .ok_or_else(|| Error::Mls("MLS group is missing from storage".to_owned()))?;
            let _previous = groups.insert(group.id, loaded);
        }
        Ok(Self {
            provider,
            signer: snapshot.signer,
            credential: snapshot.credential,
            identity: snapshot.identity,
            groups,
        })
    }

    pub fn group_mut(&mut self, group_id: &[u8]) -> Result<&mut MlsGroup> {
        self.groups
            .get_mut(group_id)
            .ok_or_else(|| Error::Mls("unknown MLS group".to_owned()))
    }

    pub fn group(&self, group_id: &[u8]) -> Result<&MlsGroup> {
        self.groups
            .get(group_id)
            .ok_or_else(|| Error::Mls("unknown MLS group".to_owned()))
    }

    pub fn member_count(&self, group_id: &[u8]) -> Result<usize> {
        Ok(self.group(group_id)?.members().count())
    }

    pub fn identity_digest(&self) -> String {
        let digest = Sha256::digest(&self.identity);
        URL_SAFE_NO_PAD.encode(digest)
    }
}

pub struct MemberCommit {
    pub commit: Vec<u8>,
    pub welcome: Vec<u8>,
    pub tree: Vec<u8>,
}

impl MemberCommit {
    pub fn bundle(&self, join_id: JoinId) -> Result<JoinBundle> {
        JoinBundle::new(
            join_id,
            self.welcome.clone(),
            self.tree.clone(),
            self.commit.clone(),
        )
    }
}

fn decode_message(bytes: &[u8]) -> Result<MlsMessageIn> {
    let mut input = bytes;
    let message = MlsMessageIn::tls_deserialize(&mut input)
        .map_err(|error| Error::Mls(format!("invalid MLS message: {error}")))?;
    if !input.is_empty() {
        return Err(Error::Mls("MLS message has trailing bytes".to_owned()));
    }
    Ok(message)
}

fn decode_protocol_message(bytes: &[u8]) -> Result<openmls::framing::ProtocolMessage> {
    decode_message(bytes)?
        .try_into_protocol_message()
        .map_err(|error| Error::Mls(format!("invalid MLS protocol message: {error}")))
}

impl fmt::Debug for MemberCommit {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("MemberCommit")
            .field("commit", &"[redacted]")
            .field("welcome", &"[redacted]")
            .field("tree", &"[redacted]")
            .finish()
    }
}

#[must_use]
pub fn secure_bytes(bytes: &[u8]) -> String {
    URL_SAFE_NO_PAD.encode(bytes)
}

pub fn wipe(bytes: &mut [u8]) {
    bytes.zeroize();
}
