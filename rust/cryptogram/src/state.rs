use std::fmt;

use base64::Engine;
use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::error::{Error, Result};
use crate::mls::{Client, ClientSnapshot};
use crate::protocol::{
    ChannelId, FrameKind, Invitation, InviteSecret, JoinId, JoinIdExt, SequenceExt, decode_frame,
    encode_frame,
};
use crate::relay::{RelayBackend, RelayClient};
use crate::vault::VaultStore;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RoomRole {
    Creator,
    Joiner,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MessageDirection {
    Incoming,
    Outgoing,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct StoredMessage {
    pub text: String,
    pub direction: MessageDirection,
    pub sequence: u64,
}

impl fmt::Debug for StoredMessage {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("StoredMessage")
            .field("text", &"[redacted]")
            .field("direction", &self.direction)
            .field("sequence", &self.sequence)
            .finish()
    }
}

#[derive(Clone, Serialize)]
pub struct PendingFrame {
    pub frame: Vec<u8>,
    pub message_index: Option<usize>,
}

impl fmt::Debug for PendingFrame {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("PendingFrame")
            .field("frame", &"[redacted]")
            .field("message_index", &self.message_index)
            .finish()
    }
}

impl<'de> Deserialize<'de> for PendingFrame {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum PendingFrameWire {
            Legacy(Vec<u8>),
            Current {
                frame: Vec<u8>,
                #[serde(default)]
                message_index: Option<usize>,
            },
        }

        match PendingFrameWire::deserialize(deserializer)? {
            PendingFrameWire::Legacy(frame) => Ok(Self {
                frame,
                message_index: None,
            }),
            PendingFrameWire::Current {
                frame,
                message_index,
            } => Ok(Self {
                frame,
                message_index,
            }),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct RoomState {
    pub channel: ChannelId,
    pub invite_secret: InviteSecret,
    pub group_id: Vec<u8>,
    pub role: RoomRole,
    pub name: String,
    pub last_sequence: u64,
    pub messages: Vec<StoredMessage>,
    #[serde(default)]
    pub join_id: Option<JoinId>,
    #[serde(default)]
    pub processed_join_requests: Vec<String>,
    #[serde(default)]
    pub processed_application_frames: Vec<String>,
    #[serde(default)]
    pub processed_commit_frames: Vec<String>,
    #[serde(default)]
    pub ignored_sequences: Vec<u64>,
    pub pending_frames: Vec<PendingFrame>,
}

impl fmt::Debug for RoomState {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("RoomState")
            .field("channel", &self.channel)
            .field("invite_secret", &"[redacted]")
            .field("group_id_length", &self.group_id.len())
            .field("join_id_present", &self.join_id.is_some())
            .field("role", &self.role)
            .field("name", &self.name)
            .field("last_sequence", &self.last_sequence)
            .field("message_count", &self.messages.len())
            .field(
                "processed_join_request_count",
                &self.processed_join_requests.len(),
            )
            .field(
                "processed_application_frame_count",
                &self.processed_application_frames.len(),
            )
            .field(
                "processed_commit_frame_count",
                &self.processed_commit_frames.len(),
            )
            .field("ignored_sequence_count", &self.ignored_sequences.len())
            .field("pending_frame_count", &self.pending_frames.len())
            .finish()
    }
}

impl RoomState {
    #[must_use]
    pub fn invitation(&self) -> Invitation {
        Invitation::new(self.channel.clone(), self.invite_secret.clone())
    }

    #[must_use]
    pub fn invitation_url(&self) -> String {
        self.invitation().invite_url(&current_base_url())
    }

    #[must_use]
    pub fn has_group(&self) -> bool {
        !self.group_id.is_empty()
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct VaultData {
    pub client: Option<ClientSnapshot>,
    pub rooms: Vec<RoomState>,
}

pub struct AppState {
    pub client: Option<Client>,
    pub rooms: Vec<RoomState>,
    pub selected_room: Option<usize>,
    pub draft: String,
    pub invite_input: String,
    pub status: String,
    pub error: Option<String>,
    pub vault: VaultStore,
    pub relay_backend: RelayBackend,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            client: None,
            rooms: Vec::new(),
            selected_room: None,
            draft: String::new(),
            invite_input: String::new(),
            status: "Create an account to begin".to_owned(),
            error: None,
            vault: VaultStore::new(),
            relay_backend: RelayBackend::Telegram,
        }
    }
}

impl AppState {
    pub async fn load() -> Result<Self> {
        let vault = VaultStore::new();
        let Some(vault_bytes) = vault.load().await? else {
            return Ok(Self {
                vault,
                invite_input: initial_invite_input(),
                ..Self::default()
            });
        };
        let vault_data: VaultData = serde_json::from_slice(&vault_bytes)?;
        let client = vault_data.client.map(Client::restore).transpose()?;
        Ok(Self {
            client,
            rooms: vault_data.rooms,
            selected_room: None,
            draft: String::new(),
            invite_input: initial_invite_input(),
            status: "Vault loaded".to_owned(),
            error: None,
            vault,
            relay_backend: RelayBackend::Telegram,
        })
    }

    pub async fn save(&self) -> Result<()> {
        let data = self.vault_data()?;
        self.vault.save(&serde_json::to_vec(&data)?).await
    }

    pub fn vault_data(&self) -> Result<VaultData> {
        Ok(VaultData {
            client: self.client.as_ref().map(Client::snapshot).transpose()?,
            rooms: self.rooms.clone(),
        })
    }

    pub async fn create_account(&mut self) -> Result<()> {
        self.create_account_local()?;
        self.save().await
    }

    pub fn create_account_local(&mut self) -> Result<()> {
        if self.client.is_some() {
            return Err(Error::Mls("an account already exists".to_owned()));
        }
        let mut identity = vec![0_u8; 32];
        getrandom::fill(&mut identity).map_err(Error::Random)?;
        self.client = Some(Client::new(identity)?);
        "Pseudonymous account created".clone_into(&mut self.status);
        self.error = None;
        Ok(())
    }

    pub async fn create_room(&mut self, name: &str) -> Result<usize> {
        let index = self.create_room_local(name)?;
        self.save().await?;
        Ok(index)
    }

    pub fn create_room_local(&mut self, name: &str) -> Result<usize> {
        let client = self
            .client
            .as_mut()
            .ok_or_else(|| Error::Mls("create an account first".to_owned()))?;
        let invitation = Invitation::random()?;
        let group_id = client.create_group()?;
        let room = RoomState {
            channel: invitation.channel_id().clone(),
            invite_secret: invitation.invite_secret().clone(),
            group_id,
            role: RoomRole::Creator,
            name: if name.trim().is_empty() {
                "New conversation".to_owned()
            } else {
                name.trim().to_owned()
            },
            last_sequence: 0,
            messages: Vec::new(),
            join_id: None,
            processed_join_requests: Vec::new(),
            processed_application_frames: Vec::new(),
            processed_commit_frames: Vec::new(),
            ignored_sequences: Vec::new(),
            pending_frames: Vec::new(),
        };
        self.rooms.push(room);
        self.selected_room = Some(self.rooms.len() - 1);
        "Conversation created".clone_into(&mut self.status);
        self.error = None;
        Ok(self.rooms.len() - 1)
    }

    pub async fn join_room(&mut self, invitation: &Invitation) -> Result<usize> {
        let (index, _, _) = self.prepare_join(invitation)?;
        self.save().await?;
        self.publish_pending(index).await?;
        self.save().await?;
        Ok(index)
    }

    pub fn prepare_join(
        &mut self,
        invitation: &Invitation,
    ) -> Result<(usize, RelayClient, Vec<u8>)> {
        let client = self
            .client
            .as_mut()
            .ok_or_else(|| Error::Mls("create an account first".to_owned()))?;
        let key_package = client.key_package()?;
        let join_id = JoinId::random()?;
        let request = crate::protocol::JoinRequest::new(join_id.clone(), key_package)?;
        let frame = encode_frame(FrameKind::JoinRequest, &request.encode());
        let relay = RelayClient::new(invitation, self.relay_backend.clone())?;
        let room = RoomState {
            channel: invitation.channel_id().clone(),
            invite_secret: invitation.invite_secret().clone(),
            group_id: Vec::new(),
            role: RoomRole::Joiner,
            name: "Invited conversation".to_owned(),
            last_sequence: 0,
            messages: Vec::new(),
            join_id: Some(join_id),
            processed_join_requests: Vec::new(),
            processed_application_frames: Vec::new(),
            processed_commit_frames: Vec::new(),
            ignored_sequences: Vec::new(),
            pending_frames: vec![PendingFrame {
                frame: frame.clone(),
                message_index: None,
            }],
        };
        self.rooms.push(room);
        self.selected_room = Some(self.rooms.len() - 1);
        "Invitation accepted; waiting for the group".clone_into(&mut self.status);
        self.error = None;
        Ok((self.rooms.len() - 1, relay, frame))
    }

    pub async fn send_selected(&mut self) -> Result<()> {
        let (index, _) = self.prepare_send()?;
        self.save().await?;
        self.publish_pending(index).await?;
        self.save().await
    }

    pub fn prepare_send(&mut self) -> Result<(usize, RelayClient)> {
        let index = self
            .selected_room
            .ok_or_else(|| Error::Mls("select a conversation first".to_owned()))?;
        let text = self.draft.trim().to_owned();
        if text.is_empty() {
            return Err(Error::Mls("message text is empty".to_owned()));
        }
        let room = self
            .rooms
            .get(index)
            .ok_or_else(|| Error::Mls("unknown conversation".to_owned()))?;
        if !room.has_group() {
            return Err(Error::Mls("conversation is not ready yet".to_owned()));
        }
        let client = self
            .client
            .as_mut()
            .ok_or_else(|| Error::Mls("create an account first".to_owned()))?;
        let frame = client.encrypt_application(&room.group_id, &text)?;
        let relay = self.relay_for_room(room)?;
        let message_index = self.rooms[index].messages.len();
        self.rooms[index].messages.push(StoredMessage {
            text,
            direction: MessageDirection::Outgoing,
            sequence: 0,
        });
        self.rooms[index].pending_frames.push(PendingFrame {
            frame,
            message_index: Some(message_index),
        });
        Ok((index, relay))
    }

    pub async fn poll_selected(&mut self) -> Result<()> {
        let index = self
            .selected_room
            .ok_or_else(|| Error::Mls("select a conversation first".to_owned()))?;
        self.poll_room(index).await
    }

    pub async fn poll_room(&mut self, index: usize) -> Result<()> {
        let (relay, after) = self.prepare_poll(index)?;
        let messages = relay.get_frames(after).await?;
        self.apply_poll(index, messages)?;
        self.save().await?;
        self.publish_pending(index).await?;
        self.save().await
    }

    pub fn prepare_poll(&self, index: usize) -> Result<(RelayClient, u64)> {
        let room = self
            .rooms
            .get(index)
            .ok_or_else(|| Error::Mls("unknown conversation".to_owned()))?;
        Ok((self.relay_for_room(room)?, room.last_sequence))
    }

    pub fn apply_poll(
        &mut self,
        index: usize,
        messages: Vec<crate::relay::DecodedRelayMessage>,
    ) -> Result<()> {
        if index >= self.rooms.len() {
            return Err(Error::Mls("unknown conversation".to_owned()));
        }
        let mut previous_sequence = self.rooms[index].last_sequence;
        for message in messages {
            let sequence = message.sequence.value();
            if sequence <= previous_sequence {
                return Err(Error::Relay(
                    "relay returned a non-increasing conversation sequence".to_owned(),
                ));
            }
            let decoded_frame = message.frame;
            if let Some(frame) = decoded_frame.as_ref()
                && self.handle_frame(index, frame, sequence).is_err()
            {
                self.remember_ignored_sequence(index, sequence);
            }
            if decoded_frame.is_none() {
                self.remember_ignored_sequence(index, sequence);
            }
            previous_sequence = sequence;
            self.rooms[index].last_sequence = sequence;
        }
        Ok(())
    }

    pub async fn publish_pending(&mut self, index: usize) -> Result<()> {
        let room_state = self
            .rooms
            .get(index)
            .ok_or_else(|| Error::Mls("unknown conversation".to_owned()))?;
        let relay = self.relay_for_room(room_state)?;
        let pending = room_state.pending_frames.clone();
        for item in pending {
            let sequence = relay.put_frame(&item.frame).await?;
            if let Some(room_mut) = self.rooms.get_mut(index) {
                room_mut
                    .pending_frames
                    .retain(|candidate| candidate.frame != item.frame);
                if let Some(message_index) = item.message_index
                    && let Some(message) = room_mut.messages.get_mut(message_index)
                {
                    message.sequence = sequence.value();
                }
            }
            if item.message_index.is_some() {
                self.draft.clear();
            }
            self.save().await?;
        }
        Ok(())
    }

    fn handle_frame(&mut self, index: usize, frame: &[u8], sequence: u64) -> Result<()> {
        if index >= self.rooms.len() {
            return Err(Error::Mls("unknown conversation".to_owned()));
        }
        let (kind, payload) = decode_frame(frame)?;
        match kind {
            FrameKind::JoinRequest => self.handle_join_request(index, &payload),
            FrameKind::JoinBundle => self.handle_join_bundle(index, &payload),
            FrameKind::Application => self.handle_application(index, &payload, sequence),
        }
    }

    fn handle_join_request(&mut self, index: usize, payload: &[u8]) -> Result<()> {
        if self.rooms[index].role != RoomRole::Creator || !self.rooms[index].has_group() {
            return Ok(());
        }
        let digest = URL_SAFE_NO_PAD.encode(Sha256::digest(payload));
        if self.rooms[index].processed_join_requests.contains(&digest) {
            return Ok(());
        }
        let request = crate::protocol::JoinRequest::decode(payload)?;
        let group_id = self.rooms[index].group_id.clone();
        let client = self
            .client
            .as_mut()
            .ok_or_else(|| Error::Mls("create an account first".to_owned()))?;
        let commit = client.add_member(&group_id, request.key_package())?;
        let bundle = commit.bundle(request.join_id().clone())?;
        self.rooms[index].processed_join_requests.push(digest);
        self.rooms[index]
            .processed_commit_frames
            .push(URL_SAFE_NO_PAD.encode(Sha256::digest(bundle.commit())));
        self.rooms[index].pending_frames.push(PendingFrame {
            frame: encode_frame(FrameKind::JoinBundle, &bundle.encode()),
            message_index: None,
        });
        Ok(())
    }

    fn handle_join_bundle(&mut self, index: usize, payload: &[u8]) -> Result<()> {
        let bundle = crate::protocol::JoinBundle::decode(payload)?;
        let digest = URL_SAFE_NO_PAD.encode(Sha256::digest(bundle.commit()));
        if self.rooms[index].processed_commit_frames.contains(&digest) {
            return Ok(());
        }
        if self.rooms[index].has_group() {
            let group_id = self.rooms[index].group_id.clone();
            let client = self
                .client
                .as_mut()
                .ok_or_else(|| Error::Mls("create an account first".to_owned()))?;
            client.process_commit(&group_id, bundle.commit())?;
            self.rooms[index].processed_commit_frames.push(digest);
            return Ok(());
        }
        if self.rooms[index].join_id.as_ref() != Some(bundle.join_id()) {
            return Ok(());
        }
        let client = self
            .client
            .as_mut()
            .ok_or_else(|| Error::Mls("create an account first".to_owned()))?;
        let group_id = client.join_group(bundle.welcome(), bundle.tree())?;
        self.rooms[index].group_id = group_id;
        self.rooms[index].join_id = None;
        self.rooms[index].processed_commit_frames.push(digest);
        "Joined encrypted conversation".clone_into(&mut self.status);
        Ok(())
    }

    fn handle_application(&mut self, index: usize, payload: &[u8], sequence: u64) -> Result<()> {
        if !self.rooms[index].has_group() {
            return Err(Error::Mls("received a message before joining".to_owned()));
        }
        let digest = URL_SAFE_NO_PAD.encode(Sha256::digest(payload));
        if self.rooms[index]
            .processed_application_frames
            .contains(&digest)
        {
            return Ok(());
        }
        let group_id = self.rooms[index].group_id.clone();
        let client = self
            .client
            .as_mut()
            .ok_or_else(|| Error::Mls("create an account first".to_owned()))?;
        let processed_message = client.process_application_optional(&group_id, payload)?;
        self.rooms[index].processed_application_frames.push(digest);
        if let Some(message) = processed_message {
            self.rooms[index].messages.push(StoredMessage {
                text: message.text,
                direction: MessageDirection::Incoming,
                sequence,
            });
        }
        Ok(())
    }

    fn remember_ignored_sequence(&mut self, index: usize, sequence: u64) {
        const MAX_IGNORED_SEQUENCES: usize = 1024;
        self.rooms[index].ignored_sequences.push(sequence);
        if self.rooms[index].ignored_sequences.len() > MAX_IGNORED_SEQUENCES {
            let excess = self.rooms[index].ignored_sequences.len() - MAX_IGNORED_SEQUENCES;
            drop(self.rooms[index].ignored_sequences.drain(..excess));
        }
    }

    fn relay_for_room(&self, room: &RoomState) -> Result<RelayClient> {
        RelayClient::new(&room.invitation(), self.relay_backend.clone())
    }
}

impl fmt::Debug for AppState {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("AppState")
            .field("client", &self.client.is_some())
            .field("rooms", &self.rooms.len())
            .field("selected_room", &self.selected_room)
            .field("draft", &"[redacted]")
            .field("invite_input", &"[redacted]")
            .field("status", &self.status)
            .field("error", &self.error)
            .field("vault", &self.vault)
            .field("relay_backend", &self.relay_backend)
            .finish()
    }
}

#[cfg(target_arch = "wasm32")]
fn initial_invite_input() -> String {
    crate::web::current_invitation()
        .and_then(std::result::Result::ok)
        .map(|invitation| invitation.to_fragment())
        .unwrap_or_default()
}

#[cfg(not(target_arch = "wasm32"))]
fn initial_invite_input() -> String {
    String::new()
}

#[cfg(target_arch = "wasm32")]
fn current_base_url() -> String {
    functora_egui::platform::web::location_href()
        .unwrap_or_else(|| "https://app.example/".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
fn current_base_url() -> String {
    "https://app.example/".to_owned()
}
