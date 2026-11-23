use std::collections::HashMap;
use std::fmt;
use std::sync::{Arc, Mutex};

use functora_tagged::Tagged;
use serde::{Deserialize, Serialize};

use crate::error::{Error, Result};
use crate::protocol::{
    ChannelId, ChannelIdExt, Invitation, MAX_RELAY_SEQUENCE, OpaquePayload, OuterCipher,
    RelayGetRequest, RelayMessage, RelayPutRequest, RelayPutResponse, Sequence, SequenceExt,
};

const MAX_RELAY_RESULTS: usize = 64;
const MAX_RELAY_RESULT_BYTES: usize = 128 * 1024;
const MAX_MEMORY_CHANNEL_MESSAGES: usize = 10_000;

#[derive(Clone, Serialize, Deserialize)]
struct StoredMessage {
    sequence: u64,
    payload: String,
}

impl fmt::Debug for StoredMessage {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("StoredMessage")
            .field("sequence", &self.sequence)
            .field("payload_length", &self.payload.len())
            .finish()
    }
}

#[derive(Clone, Default)]
pub struct MemoryRelay {
    channels: Arc<Mutex<HashMap<String, Vec<StoredMessage>>>>,
}

impl fmt::Debug for MemoryRelay {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("MemoryRelay")
            .field("channels", &"[redacted]")
            .finish()
    }
}

impl MemoryRelay {
    pub fn put(&self, channel: &ChannelId, payload: &OpaquePayload) -> Result<u64> {
        let mut channels = self
            .channels
            .lock()
            .map_err(|_| Error::Relay("memory relay lock is poisoned".to_owned()))?;
        let messages = channels.entry(channel.as_str().to_owned()).or_default();
        if messages.len() >= MAX_MEMORY_CHANNEL_MESSAGES {
            return Err(Error::Relay("memory relay channel is full".to_owned()));
        }
        let sequence = messages.last().map_or(Ok(1), |message| {
            message
                .sequence
                .checked_add(1)
                .ok_or_else(|| Error::Relay("memory relay sequence is exhausted".to_owned()))
        })?;
        messages.push(StoredMessage {
            sequence,
            payload: payload.as_str().to_owned(),
        });
        Ok(sequence)
    }

    pub fn get(&self, channel: &ChannelId, after: u64) -> Result<Vec<RelayMessage>> {
        let channels = self
            .channels
            .lock()
            .map_err(|_| Error::Relay("memory relay lock is poisoned".to_owned()))?;
        let Some(messages) = channels.get(channel.as_str()) else {
            return Ok(Vec::new());
        };
        let mut result = Vec::new();
        let mut total_bytes = 0_usize;
        for message in messages.iter().filter(|message| message.sequence > after) {
            let payload = OpaquePayload::new(message.payload.clone())?;
            total_bytes = total_bytes.saturating_add(payload.as_str().len());
            if result.len() >= MAX_RELAY_RESULTS || total_bytes > MAX_RELAY_RESULT_BYTES {
                break;
            }
            let sequence = Tagged::new(message.sequence)
                .map_err(|error| Error::Relay(format!("invalid relay sequence: {error}")))?;
            result.push(RelayMessage { sequence, payload });
        }
        Ok(result)
    }
}

#[derive(Clone, Debug)]
pub enum RelayBackend {
    Telegram,
    Memory(MemoryRelay),
}

#[derive(Clone, Debug)]
pub struct RelayClient {
    channel: ChannelId,
    cipher: OuterCipher,
    backend: RelayBackend,
}

#[derive(Clone)]
pub struct DecodedRelayMessage {
    pub sequence: Sequence,
    pub frame: Option<Vec<u8>>,
}

impl fmt::Debug for DecodedRelayMessage {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("DecodedRelayMessage")
            .field("sequence", &self.sequence)
            .field("frame", &"[redacted]")
            .finish()
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawRelayMessage {
    sequence: u64,
    payload: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawRelayGetResponse {
    messages: Vec<RawRelayMessage>,
}

impl RelayClient {
    pub fn new(invitation: &Invitation, backend: RelayBackend) -> Result<Self> {
        Ok(Self {
            channel: invitation.channel_id().clone(),
            cipher: OuterCipher::from_invitation(invitation)?,
            backend,
        })
    }

    #[must_use]
    pub fn channel(&self) -> &ChannelId {
        &self.channel
    }

    pub async fn put_frame(&self, frame: &[u8]) -> Result<Sequence> {
        let payload = OpaquePayload::new(self.cipher.seal(frame)?)?;
        match &self.backend {
            RelayBackend::Memory(memory) => {
                let sequence = memory.put(&self.channel, &payload)?;
                Tagged::new(sequence)
                    .map_err(|error| Error::Relay(format!("invalid relay sequence: {error}")))
            }
            RelayBackend::Telegram => {
                let request = RelayPutRequest {
                    channel: self.channel.clone(),
                    payload,
                };
                let response: RelayPutResponse = endpoint_call("relayPut", &request).await?;
                Ok(response.sequence)
            }
        }
    }

    pub async fn get_frames(&self, after: u64) -> Result<Vec<DecodedRelayMessage>> {
        if after > MAX_RELAY_SEQUENCE {
            return Err(Error::Relay(
                "relay cursor exceeds the JavaScript safe integer range".to_owned(),
            ));
        }
        let raw_messages = match &self.backend {
            RelayBackend::Memory(memory) => memory
                .get(&self.channel, after)?
                .into_iter()
                .map(|message| RawRelayMessage {
                    sequence: message.sequence.value(),
                    payload: message.payload.into_inner(),
                })
                .collect(),
            RelayBackend::Telegram => {
                let request = RelayGetRequest {
                    channel: self.channel.clone(),
                    sequence: after,
                };
                let response: RawRelayGetResponse = endpoint_call("relayGet", &request).await?;
                response.messages
            }
        };
        validate_relay_messages(raw_messages, after)?
            .into_iter()
            .map(|message| {
                let sequence = Tagged::new(message.sequence)
                    .map_err(|error| Error::Relay(format!("invalid relay sequence: {error}")))?;
                let frame = OpaquePayload::new(message.payload)
                    .ok()
                    .and_then(|payload| self.cipher.open(payload.as_str()).ok());
                Ok(DecodedRelayMessage { sequence, frame })
            })
            .collect()
    }
}

fn validate_relay_messages(
    messages: Vec<RawRelayMessage>,
    after: u64,
) -> Result<Vec<RawRelayMessage>> {
    if messages.len() > MAX_RELAY_RESULTS {
        return Err(Error::Relay("relay returned too many messages".to_owned()));
    }
    let mut total_bytes = 0_usize;
    let mut previous = after;
    messages
        .into_iter()
        .try_fold(Vec::new(), |mut result, message| {
            if message.sequence > MAX_RELAY_SEQUENCE {
                return Err(Error::Relay(
                    "relay sequence exceeds the JavaScript safe integer range".to_owned(),
                ));
            }
            if message.sequence <= previous {
                return Err(Error::Relay(
                    "relay returned a non-increasing sequence".to_owned(),
                ));
            }
            total_bytes = total_bytes
                .checked_add(message.payload.len())
                .ok_or_else(|| Error::Relay("relay response size overflowed".to_owned()))?;
            if total_bytes > MAX_RELAY_RESULT_BYTES {
                return Err(Error::Relay("relay response is too large".to_owned()));
            }
            previous = message.sequence;
            result.push(message);
            Ok(result)
        })
}

#[cfg(target_arch = "wasm32")]
async fn endpoint_call<T>(endpoint: &str, request: &impl Serialize) -> Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    let value = serde_wasm_bindgen::to_value(request)?;
    let response = crate::web::serverless_call(endpoint, value).await?;
    Ok(serde_wasm_bindgen::from_value(response)?)
}

#[cfg(not(target_arch = "wasm32"))]
async fn endpoint_call<T>(_endpoint: &str, _request: &impl Serialize) -> Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    std::future::ready(Err(Error::Unsupported(
        "Telegram Serverless is available only in the web build",
    )))
    .await
}
