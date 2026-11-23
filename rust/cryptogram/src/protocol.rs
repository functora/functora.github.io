use std::fmt;

use base64::Engine;
use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use chacha20poly1305::aead::{Aead, KeyInit, Payload};
use chacha20poly1305::{ChaCha20Poly1305, Key, Nonce};
use functora_tagged::{Refine, Tagged};
use hkdf::Hkdf;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use zeroize::Zeroize;

use crate::error::{Error, Result};

const CHANNEL_BYTES: usize = 16;
const INVITE_SECRET_BYTES: usize = 32;
const NONCE_BYTES: usize = 12;
const MAX_RELAY_PAYLOAD_BYTES: usize = 96 * 1024;
const MAX_RELAY_PAYLOAD_ENCODED_BYTES: usize = 128 * 1024;
const MAX_FRAME_BYTES: usize = MAX_RELAY_PAYLOAD_BYTES - NONCE_BYTES - 16;
const MAX_MESSAGE_BYTES: usize = 16 * 1024;
const MAX_INVITATION_FRAGMENT_BYTES: usize = 256;
const MAX_TELEGRAM_FRAGMENT_BYTES: usize = 16 * 1024;
const JOIN_ID_BYTES: usize = 32;
pub const MAX_RELAY_SEQUENCE: u64 = 9_007_199_254_740_991;
const OUTER_AAD: &[u8] = b"cryptogram outer transport v1";
const OUTER_INFO: &[u8] = b"cryptogram outer transport key v1";

#[derive(Debug, Clone, Copy)]
pub struct ChannelDimension;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChannelIdError {
    Empty,
    InvalidLength,
    InvalidCharacter,
}

impl fmt::Display for ChannelIdError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => formatter.write_str("channel id is empty"),
            Self::InvalidLength => formatter.write_str("channel id has an invalid length"),
            Self::InvalidCharacter => {
                formatter.write_str("channel id contains an invalid character")
            }
        }
    }
}

impl std::error::Error for ChannelIdError {}

#[derive(Debug)]
pub enum FChannelId {}

impl Refine<String> for FChannelId {
    type RefineError = ChannelIdError;

    fn refine(representation: String) -> std::result::Result<String, Self::RefineError> {
        if representation.is_empty() {
            return Err(ChannelIdError::Empty);
        }
        if representation.len() != 22
            || !representation
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-' || byte == b'_')
        {
            return Err(ChannelIdError::InvalidLength);
        }
        let decoded = URL_SAFE_NO_PAD
            .decode(&representation)
            .map_err(|_| ChannelIdError::InvalidCharacter)?;
        if decoded.len() != CHANNEL_BYTES || URL_SAFE_NO_PAD.encode(decoded) != representation {
            return Err(ChannelIdError::InvalidCharacter);
        }
        Ok(representation)
    }
}

pub type ChannelId = Tagged<String, ChannelDimension, FChannelId>;

#[derive(Debug, Clone, Copy)]
pub struct SequenceDimension;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SequenceError {
    NotPositive,
    TooLarge,
}

impl fmt::Display for SequenceError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotPositive => formatter.write_str("sequence must be positive"),
            Self::TooLarge => {
                formatter.write_str("sequence exceeds the JavaScript safe integer range")
            }
        }
    }
}

impl std::error::Error for SequenceError {}

#[derive(Debug)]
pub enum FSequence {}

impl Refine<u64> for FSequence {
    type RefineError = SequenceError;

    fn refine(representation: u64) -> std::result::Result<u64, Self::RefineError> {
        if representation == 0 {
            Err(SequenceError::NotPositive)
        } else if representation > MAX_RELAY_SEQUENCE {
            Err(SequenceError::TooLarge)
        } else {
            Ok(representation)
        }
    }
}

pub type Sequence = Tagged<u64, SequenceDimension, FSequence>;

#[derive(Debug, Clone, Copy)]
pub struct InviteSecretDimension;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InviteSecretError {
    InvalidLength,
}

impl fmt::Display for InviteSecretError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("invite secret must contain exactly 32 bytes")
    }
}

impl std::error::Error for InviteSecretError {}

#[derive(Debug)]
pub enum FInviteSecret {}

impl Refine<Vec<u8>> for FInviteSecret {
    type RefineError = InviteSecretError;

    fn refine(representation: Vec<u8>) -> std::result::Result<Vec<u8>, Self::RefineError> {
        if representation.len() == INVITE_SECRET_BYTES {
            Ok(representation)
        } else {
            Err(InviteSecretError::InvalidLength)
        }
    }
}

pub type InviteSecret = Tagged<Vec<u8>, InviteSecretDimension, FInviteSecret>;

#[derive(Debug, Clone, Copy)]
pub struct JoinIdDimension;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JoinIdError {
    InvalidLength,
}

impl fmt::Display for JoinIdError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("join id must contain exactly 32 bytes")
    }
}

impl std::error::Error for JoinIdError {}

#[derive(Debug)]
pub enum FJoinId {}

impl Refine<Vec<u8>> for FJoinId {
    type RefineError = JoinIdError;

    fn refine(representation: Vec<u8>) -> std::result::Result<Vec<u8>, Self::RefineError> {
        if representation.len() == JOIN_ID_BYTES {
            Ok(representation)
        } else {
            Err(JoinIdError::InvalidLength)
        }
    }
}

pub type JoinId = Tagged<Vec<u8>, JoinIdDimension, FJoinId>;

pub trait ChannelIdExt {
    fn random() -> Result<Self>
    where
        Self: Sized;
    fn as_str(&self) -> &str;
}

impl ChannelIdExt for Tagged<String, ChannelDimension, FChannelId> {
    fn random() -> Result<Self> {
        let bytes = random_bytes::<CHANNEL_BYTES>()?;
        Tagged::<String, ChannelDimension, FChannelId>::new(URL_SAFE_NO_PAD.encode(bytes))
            .map_err(|error| Error::InvalidRelayInput(error.to_string()))
    }

    fn as_str(&self) -> &str {
        self.rep().as_str()
    }
}

pub trait SequenceExt {
    fn value(&self) -> u64;
}

impl SequenceExt for Tagged<u64, SequenceDimension, FSequence> {
    fn value(&self) -> u64 {
        *self.rep()
    }
}

pub trait InviteSecretExt {
    fn random() -> Result<Self>
    where
        Self: Sized;
    fn bytes(&self) -> &[u8];
}

impl InviteSecretExt for Tagged<Vec<u8>, InviteSecretDimension, FInviteSecret> {
    fn random() -> Result<Self> {
        Tagged::<Vec<u8>, InviteSecretDimension, FInviteSecret>::new(
            random_bytes::<INVITE_SECRET_BYTES>()?.to_vec(),
        )
        .map_err(|error| Error::InvalidInvitation(error.to_string()))
    }

    fn bytes(&self) -> &[u8] {
        self.rep().as_slice()
    }
}

pub trait JoinIdExt {
    fn random() -> Result<Self>
    where
        Self: Sized;
    fn bytes(&self) -> &[u8];
}

impl JoinIdExt for Tagged<Vec<u8>, JoinIdDimension, FJoinId> {
    fn random() -> Result<Self> {
        Tagged::<Vec<u8>, JoinIdDimension, FJoinId>::new(random_bytes::<JOIN_ID_BYTES>()?.to_vec())
            .map_err(|error| Error::InvalidFrame(error.to_string()))
    }

    fn bytes(&self) -> &[u8] {
        self.rep().as_slice()
    }
}

#[derive(Clone)]
pub struct Invitation {
    channel_id: ChannelId,
    invite_secret: InviteSecret,
}

impl fmt::Debug for Invitation {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Invitation")
            .field("channel_id", &self.channel_id)
            .field("invite_secret", &"[redacted]")
            .finish()
    }
}

impl Invitation {
    #[must_use]
    pub fn new(channel_id: ChannelId, invite_secret: InviteSecret) -> Self {
        Self {
            channel_id,
            invite_secret,
        }
    }

    pub fn random() -> Result<Self> {
        Ok(Self::new(ChannelId::random()?, InviteSecret::random()?))
    }

    #[must_use]
    pub fn channel_id(&self) -> &ChannelId {
        &self.channel_id
    }

    #[must_use]
    pub fn invite_secret(&self) -> &InviteSecret {
        &self.invite_secret
    }

    #[must_use]
    pub fn to_fragment(&self) -> String {
        format!(
            "channel_id={}&invite_secret={}",
            self.channel_id.as_str(),
            URL_SAFE_NO_PAD.encode(self.invite_secret.bytes())
        )
    }

    #[must_use]
    pub fn invite_url(&self, base_url: &str) -> String {
        let base = base_url.split('#').next().unwrap_or(base_url);
        format!("{base}#{}", self.to_fragment())
    }

    pub fn from_fragment(fragment: &str) -> Result<Self> {
        if fragment.len() > MAX_INVITATION_FRAGMENT_BYTES {
            return Err(Error::InvalidInvitation(
                "invitation fragment is too large".to_owned(),
            ));
        }
        let fragment_value = fragment.strip_prefix('#').unwrap_or(fragment);
        let mut channel_option = None;
        let mut secret_option = None;
        for part in fragment_value.split('&') {
            if part.is_empty() {
                return Err(Error::InvalidInvitation(
                    "invalid invitation fragment".to_owned(),
                ));
            }
            let Some((key, value)) = part.split_once('=') else {
                return Err(Error::InvalidInvitation(
                    "invalid invitation fragment".to_owned(),
                ));
            };
            match key {
                "channel_id" if channel_option.is_none() => channel_option = Some(value),
                "invite_secret" if secret_option.is_none() => secret_option = Some(value),
                _ => {
                    return Err(Error::InvalidInvitation(
                        "invalid invitation fragment".to_owned(),
                    ));
                }
            }
        }
        let channel_value = channel_option
            .ok_or_else(|| Error::InvalidInvitation("missing channel_id".to_owned()))?;
        let secret_value = secret_option
            .ok_or_else(|| Error::InvalidInvitation("missing invite_secret".to_owned()))?;
        let channel_id =
            Tagged::<String, ChannelDimension, FChannelId>::new(channel_value.to_owned())
                .map_err(|error| Error::InvalidInvitation(error.to_string()))?;
        let secret_bytes = URL_SAFE_NO_PAD
            .decode(secret_value)
            .map_err(|error| Error::InvalidInvitation(error.to_string()))?;
        if secret_bytes.len() != INVITE_SECRET_BYTES
            || URL_SAFE_NO_PAD.encode(&secret_bytes) != secret_value
        {
            return Err(Error::InvalidInvitation("invalid invite secret".to_owned()));
        }
        let invite_secret =
            Tagged::<Vec<u8>, InviteSecretDimension, FInviteSecret>::new(secret_bytes)
                .map_err(|error| Error::InvalidInvitation(error.to_string()))?;
        Ok(Self::new(channel_id, invite_secret))
    }

    pub fn from_telegram_fragment(fragment: &str) -> Result<Self> {
        if fragment.len() > MAX_TELEGRAM_FRAGMENT_BYTES {
            return Err(Error::InvalidInvitation(
                "Telegram launch fragment is too large".to_owned(),
            ));
        }
        let fragment_value = fragment.strip_prefix('#').unwrap_or(fragment);
        let invitation_parts = fragment_value
            .split('&')
            .map(|part| {
                let (key, _) = part.split_once('=').ok_or_else(|| {
                    Error::InvalidInvitation("invalid invitation fragment".to_owned())
                })?;
                Ok((key, part))
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .filter(|(key, _)| !is_telegram_parameter(key))
            .map(|(_, part)| part)
            .collect::<Vec<_>>();
        if invitation_parts.len() != 2 {
            return Err(Error::InvalidInvitation(
                "invitation fields are missing or duplicated".to_owned(),
            ));
        }
        Self::from_fragment(&invitation_parts.join("&"))
    }

    pub fn from_url(url: &str) -> Result<Self> {
        let fragment = url.split_once('#').map_or(url, |(_, value)| value);
        Self::from_telegram_fragment(fragment)
    }
}

fn is_telegram_parameter(key: &str) -> bool {
    matches!(
        key,
        "tgWebAppData"
            | "tgWebAppVersion"
            | "tgWebAppPlatform"
            | "tgWebAppTheme"
            | "tgWebAppColorScheme"
            | "tgWebAppStartParam"
            | "tgWebAppStartAttach"
            | "tgWebAppShowScore"
            | "tgWebAppIsInline"
            | "tgWebAppBotUsername"
            | "tgWebAppThemeParams"
            | "_path"
    )
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct OpaquePayload(String);

impl fmt::Debug for OpaquePayload {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("OpaquePayload")
            .field("length", &self.0.len())
            .finish()
    }
}

impl TryFrom<String> for OpaquePayload {
    type Error = Error;

    fn try_from(value: String) -> Result<Self> {
        Self::new(value)
    }
}

impl OpaquePayload {
    pub fn new(value: String) -> Result<Self> {
        if value.is_empty() || value.len() > MAX_RELAY_PAYLOAD_ENCODED_BYTES {
            return Err(Error::InvalidRelayInput(
                "payload length is outside the allowed range".to_owned(),
            ));
        }
        let decoded = URL_SAFE_NO_PAD
            .decode(&value)
            .map_err(|_| Error::InvalidRelayInput("payload is not base64url".to_owned()))?;
        if decoded.is_empty() || decoded.len() > MAX_RELAY_PAYLOAD_BYTES {
            return Err(Error::InvalidRelayInput(
                "payload length is outside the allowed range".to_owned(),
            ));
        }
        if URL_SAFE_NO_PAD.encode(decoded) != value {
            return Err(Error::InvalidRelayInput(
                "payload is not canonical base64url".to_owned(),
            ));
        }
        Ok(Self(value))
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[must_use]
    pub fn into_inner(self) -> String {
        self.0
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RelayPutRequest {
    pub channel: ChannelId,
    pub payload: OpaquePayload,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RelayPutResponse {
    pub sequence: Sequence,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RelayGetRequest {
    pub channel: ChannelId,
    pub sequence: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RelayGetResponse {
    pub messages: Vec<RelayMessage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RelayMessage {
    pub sequence: Sequence,
    pub payload: OpaquePayload,
}

#[derive(Clone)]
pub struct OuterCipher {
    key: [u8; 32],
    aad: Vec<u8>,
}

impl fmt::Debug for OuterCipher {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("OuterCipher")
            .field("key", &"[redacted]")
            .field("aad", &self.aad)
            .finish()
    }
}

impl Drop for OuterCipher {
    fn drop(&mut self) {
        self.key.zeroize();
    }
}

impl OuterCipher {
    pub fn from_invitation(invitation: &Invitation) -> Result<Self> {
        let hkdf = Hkdf::<Sha256>::new(
            Some(invitation.channel_id().as_str().as_bytes()),
            invitation.invite_secret().bytes(),
        );
        let mut key = [0_u8; 32];
        hkdf.expand(OUTER_INFO, &mut key)
            .map_err(Error::KeyDerivation)?;
        let mut aad = OUTER_AAD.to_vec();
        aad.extend_from_slice(b"\0");
        aad.extend_from_slice(invitation.channel_id().as_str().as_bytes());
        Ok(Self { key, aad })
    }

    pub fn seal(&self, plaintext: &[u8]) -> Result<String> {
        if plaintext.len() > MAX_FRAME_BYTES {
            return Err(Error::InvalidFrame("frame is too large".to_owned()));
        }
        let mut nonce_bytes = [0_u8; NONCE_BYTES];
        getrandom::fill(&mut nonce_bytes).map_err(Error::Random)?;
        let cipher = ChaCha20Poly1305::new(Key::from_slice(&self.key));
        let ciphertext = cipher
            .encrypt(
                Nonce::from_slice(&nonce_bytes),
                Payload {
                    msg: plaintext,
                    aad: &self.aad,
                },
            )
            .map_err(Error::OuterCrypto)?;
        let mut envelope = nonce_bytes.to_vec();
        envelope.extend_from_slice(&ciphertext);
        Ok(URL_SAFE_NO_PAD.encode(envelope))
    }

    pub fn open(&self, payload: &str) -> Result<Vec<u8>> {
        if payload.len() > MAX_RELAY_PAYLOAD_ENCODED_BYTES {
            return Err(Error::InvalidFrame("outer payload is too large".to_owned()));
        }
        let envelope = URL_SAFE_NO_PAD.decode(payload)?;
        if URL_SAFE_NO_PAD.encode(&envelope) != payload {
            return Err(Error::InvalidFrame(
                "outer payload is not canonical base64url".to_owned(),
            ));
        }
        if envelope.len() <= NONCE_BYTES {
            return Err(Error::InvalidFrame("outer payload is too short".to_owned()));
        }
        if envelope.len() > MAX_RELAY_PAYLOAD_BYTES {
            return Err(Error::InvalidFrame("outer payload is too large".to_owned()));
        }
        let (nonce_bytes, ciphertext) = envelope.split_at(NONCE_BYTES);
        let cipher = ChaCha20Poly1305::new(Key::from_slice(&self.key));
        cipher
            .decrypt(
                Nonce::from_slice(nonce_bytes),
                Payload {
                    msg: ciphertext,
                    aad: &self.aad,
                },
            )
            .map_err(Error::OuterCrypto)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrameKind {
    JoinRequest,
    JoinBundle,
    Application,
}

impl FrameKind {
    const fn tag(self) -> u8 {
        match self {
            Self::JoinRequest => 1,
            Self::JoinBundle => 2,
            Self::Application => 3,
        }
    }

    const fn from_tag(tag: u8) -> Option<Self> {
        match tag {
            1 => Some(Self::JoinRequest),
            2 => Some(Self::JoinBundle),
            3 => Some(Self::Application),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct JoinRequest {
    join_id: JoinId,
    key_package: Vec<u8>,
}

impl fmt::Debug for JoinRequest {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("JoinRequest")
            .field("join_id", &"[redacted]")
            .field("key_package_length", &self.key_package.len())
            .finish()
    }
}

impl JoinRequest {
    pub fn new(join_id: JoinId, key_package: Vec<u8>) -> Result<Self> {
        if key_package.is_empty() || key_package.len() > MAX_FRAME_BYTES {
            return Err(Error::InvalidFrame(
                "MLS key package length is outside the allowed range".to_owned(),
            ));
        }
        Ok(Self {
            join_id,
            key_package,
        })
    }

    #[must_use]
    pub fn join_id(&self) -> &JoinId {
        &self.join_id
    }

    #[must_use]
    pub fn key_package(&self) -> &[u8] {
        &self.key_package
    }

    #[must_use]
    pub fn encode(&self) -> Vec<u8> {
        let mut encoded = Vec::with_capacity(self.join_id.bytes().len() + self.key_package.len());
        encoded.extend_from_slice(self.join_id.bytes());
        encoded.extend_from_slice(&self.key_package);
        encoded
    }

    pub fn decode(encoded: &[u8]) -> Result<Self> {
        if encoded.len() <= JOIN_ID_BYTES || encoded.len() > MAX_FRAME_BYTES {
            return Err(Error::InvalidFrame(
                "MLS join request length is outside the allowed range".to_owned(),
            ));
        }
        let (join_id_bytes, key_package) = encoded.split_at(JOIN_ID_BYTES);
        let join_id = Tagged::<Vec<u8>, JoinIdDimension, FJoinId>::new(join_id_bytes.to_vec())
            .map_err(|error| Error::InvalidFrame(error.to_string()))?;
        Self::new(join_id, key_package.to_vec())
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct JoinBundle {
    join_id: JoinId,
    welcome: Vec<u8>,
    tree: Vec<u8>,
    commit: Vec<u8>,
}

impl fmt::Debug for JoinBundle {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("JoinBundle")
            .field("join_id", &"[redacted]")
            .field("welcome_length", &self.welcome.len())
            .field("tree_length", &self.tree.len())
            .field("commit_length", &self.commit.len())
            .finish()
    }
}

impl JoinBundle {
    pub fn new(join_id: JoinId, welcome: Vec<u8>, tree: Vec<u8>, commit: Vec<u8>) -> Result<Self> {
        if welcome.is_empty()
            || tree.is_empty()
            || commit.is_empty()
            || welcome.len() > MAX_FRAME_BYTES
            || tree.len() > MAX_FRAME_BYTES
            || commit.len() > MAX_FRAME_BYTES
        {
            return Err(Error::InvalidFrame(
                "MLS join bundle length is outside the allowed range".to_owned(),
            ));
        }
        Ok(Self {
            join_id,
            welcome,
            tree,
            commit,
        })
    }

    #[must_use]
    pub fn join_id(&self) -> &JoinId {
        &self.join_id
    }

    #[must_use]
    pub fn welcome(&self) -> &[u8] {
        &self.welcome
    }

    #[must_use]
    pub fn tree(&self) -> &[u8] {
        &self.tree
    }

    #[must_use]
    pub fn commit(&self) -> &[u8] {
        &self.commit
    }

    #[must_use]
    pub fn encode(&self) -> Vec<u8> {
        let capacity = self
            .join_id
            .bytes()
            .len()
            .saturating_add(self.welcome.len())
            .saturating_add(self.tree.len())
            .saturating_add(self.commit.len())
            .saturating_add(12);
        let mut encoded = Vec::with_capacity(capacity);
        encoded.extend_from_slice(self.join_id.bytes());
        encoded.extend_from_slice(
            &u32::try_from(self.welcome.len())
                .unwrap_or(u32::MAX)
                .to_be_bytes(),
        );
        encoded.extend_from_slice(
            &u32::try_from(self.tree.len())
                .unwrap_or(u32::MAX)
                .to_be_bytes(),
        );
        encoded.extend_from_slice(
            &u32::try_from(self.commit.len())
                .unwrap_or(u32::MAX)
                .to_be_bytes(),
        );
        encoded.extend_from_slice(&self.welcome);
        encoded.extend_from_slice(&self.tree);
        encoded.extend_from_slice(&self.commit);
        encoded
    }

    pub fn decode(encoded: &[u8]) -> Result<Self> {
        let header_len = JOIN_ID_BYTES + 12;
        if encoded.len() <= header_len || encoded.len() > MAX_FRAME_BYTES {
            return Err(Error::InvalidFrame(
                "MLS join bundle length is outside the allowed range".to_owned(),
            ));
        }
        let (join_id_bytes, lengths) = encoded.split_at(JOIN_ID_BYTES);
        let join_id = Tagged::<Vec<u8>, JoinIdDimension, FJoinId>::new(join_id_bytes.to_vec())
            .map_err(|error| Error::InvalidFrame(error.to_string()))?;
        let welcome_len = u32::from_be_bytes(lengths[0..4].try_into().map_err(|_| {
            Error::InvalidFrame("MLS join bundle has an invalid welcome length".to_owned())
        })?) as usize;
        let tree_len = u32::from_be_bytes(lengths[4..8].try_into().map_err(|_| {
            Error::InvalidFrame("MLS join bundle has an invalid tree length".to_owned())
        })?) as usize;
        let commit_len = u32::from_be_bytes(lengths[8..12].try_into().map_err(|_| {
            Error::InvalidFrame("MLS join bundle has an invalid commit length".to_owned())
        })?) as usize;
        let payload_len = welcome_len
            .checked_add(tree_len)
            .and_then(|length| length.checked_add(commit_len))
            .ok_or_else(|| Error::InvalidFrame("MLS join bundle length overflowed".to_owned()))?;
        if payload_len == 0 || payload_len != encoded.len().saturating_sub(header_len) {
            return Err(Error::InvalidFrame(
                "MLS join bundle lengths are inconsistent".to_owned(),
            ));
        }
        let (welcome, remainder) = encoded[header_len..].split_at(welcome_len);
        let (tree, commit) = remainder.split_at(tree_len);
        Self::new(join_id, welcome.to_vec(), tree.to_vec(), commit.to_vec())
    }
}

#[must_use]
pub fn encode_frame(kind: FrameKind, payload: &[u8]) -> Vec<u8> {
    let mut frame = Vec::with_capacity(payload.len() + 1);
    frame.push(kind.tag());
    frame.extend_from_slice(payload);
    frame
}

pub fn decode_frame(frame: &[u8]) -> Result<(FrameKind, Vec<u8>)> {
    if frame.len() > MAX_FRAME_BYTES {
        return Err(Error::InvalidFrame("frame is too large".to_owned()));
    }
    let (tag, payload) = frame
        .split_first()
        .ok_or_else(|| Error::InvalidFrame("frame is empty".to_owned()))?;
    let kind = FrameKind::from_tag(*tag)
        .ok_or_else(|| Error::InvalidFrame("unknown frame kind".to_owned()))?;
    Ok((kind, payload.to_vec()))
}

#[derive(Clone, Serialize)]
pub struct ChatMessage {
    pub text: String,
}

#[derive(Deserialize)]
struct ChatMessageWire {
    text: String,
}

impl<'de> Deserialize<'de> for ChatMessage {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = ChatMessageWire::deserialize(deserializer)?;
        Self::new(wire.text).map_err(serde::de::Error::custom)
    }
}

impl fmt::Debug for ChatMessage {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("ChatMessage")
            .field("text", &"[redacted]")
            .finish()
    }
}

impl ChatMessage {
    pub fn new(text: String) -> Result<Self> {
        if text.is_empty() || text.len() > MAX_MESSAGE_BYTES {
            return Err(Error::InvalidFrame(
                "message length is outside the allowed range".to_owned(),
            ));
        }
        Ok(Self { text })
    }
}

fn random_bytes<const N: usize>() -> Result<[u8; N]> {
    let mut bytes = [0_u8; N];
    getrandom::fill(&mut bytes).map_err(Error::Random)?;
    Ok(bytes)
}
