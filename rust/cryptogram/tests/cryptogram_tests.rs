use base64::Engine;
use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use chacha20poly1305::aead::{Aead, KeyInit, Payload};
use chacha20poly1305::{ChaCha20Poly1305, Key, Nonce};
use cryptogram::mls::Client;
use cryptogram::protocol::{
    ChatMessage, FrameKind, Invitation, InviteSecretExt, MAX_RELAY_SEQUENCE, OpaquePayload,
    OuterCipher, RelayMessage, RelayPutResponse, SequenceExt, decode_frame,
};
use cryptogram::relay::{MemoryRelay, RelayBackend, RelayClient};
use cryptogram::state::{AppState, PendingFrame, VaultData};
use cryptogram::vault::VaultStore;
use functora_egui::storage::Storage;

fn client(seed: u8) -> Result<Client, cryptogram::Error> {
    Client::new(vec![seed; 32])
}

type MemberParts = (Vec<u8>, Vec<u8>, Vec<u8>);

fn member_parts(commit: &cryptogram::mls::MemberCommit) -> MemberParts {
    (
        commit.welcome.clone(),
        commit.tree.clone(),
        commit.commit.clone(),
    )
}

fn noncanonical_base64url(encoded: &str) -> String {
    let mut characters = encoded.chars().collect::<Vec<_>>();
    let changed = characters.len() / 2;
    characters[changed] = if characters[changed] == 'A' { 'B' } else { 'A' };
    characters.into_iter().collect()
}

fn encrypted_vault_blob(key: &[u8; 32], plaintext: &[u8]) -> Result<String, cryptogram::Error> {
    let nonce = [1_u8; 12];
    let cipher = ChaCha20Poly1305::new(Key::from_slice(key));
    let ciphertext = cipher
        .encrypt(
            Nonce::from_slice(&nonce),
            Payload {
                msg: plaintext,
                aad: b"cryptogram encrypted vault v1",
            },
        )
        .map_err(cryptogram::Error::OuterCrypto)?;
    let mut envelope = nonce.to_vec();
    envelope.extend_from_slice(&ciphertext);
    Ok(URL_SAFE_NO_PAD.encode(envelope))
}

#[tokio::test(flavor = "current_thread")]
async fn vault_uses_chunked_storage_and_removes_legacy_values() -> Result<(), cryptogram::Error> {
    let storage = Storage::new("cryptogram-vault-test");
    let store = VaultStore::with_storage(storage);
    store.clear().await?;
    storage.try_persist("cryptogram_vault_data", &String::new())?;
    storage.try_persist("cryptogram_vault_key", &String::new())?;
    assert!(store.load().await?.is_none());

    let plaintext = vec![7_u8; 16 * 1024];
    store.save(&plaintext).await?;
    assert_eq!(store.load().await?.as_deref(), Some(plaintext.as_slice()));
    assert!(
        storage
            .try_load::<String>("cryptogram_vault_data")?
            .is_none()
    );
    assert!(
        storage
            .try_load::<serde_json::Value>("cryptogram_vault_manifest")?
            .is_some()
    );

    store.clear().await?;
    assert!(store.load().await?.is_none());
    drop(std::fs::remove_dir_all(storage.files_dir()?));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn vault_rejects_noncanonical_legacy_blob() -> Result<(), cryptogram::Error> {
    let storage = Storage::new("cryptogram-vault-canonical-test");
    let store = VaultStore::with_storage(storage);
    store.clear().await?;
    let key_bytes = [7_u8; 32];
    let key = URL_SAFE_NO_PAD.encode(key_bytes);
    storage.try_persist("cryptogram_vault_key", &key)?;
    let canonical = encrypted_vault_blob(&key_bytes, b"legacy")?;
    let noncanonical = noncanonical_base64url(&canonical);
    assert_ne!(
        URL_SAFE_NO_PAD.decode(&canonical)?,
        URL_SAFE_NO_PAD.decode(&noncanonical)?
    );
    storage.try_persist("cryptogram_vault_data", &noncanonical)?;
    assert!(store.load().await.is_err());
    drop(std::fs::remove_dir_all(storage.files_dir()?));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn outbox_persists_each_successful_removal_before_a_later_failure()
-> Result<(), cryptogram::Error> {
    let storage = Storage::new("cryptogram-outbox-test");
    let memory = MemoryRelay::default();
    let mut state = AppState {
        vault: VaultStore::with_storage(storage),
        relay_backend: RelayBackend::Memory(memory),
        ..AppState::default()
    };
    state.create_account_local()?;
    assert_eq!(state.create_room_local("group")?, 0);
    state.rooms[0].pending_frames.push(PendingFrame {
        frame: vec![1, 2, 3],
        message_index: None,
    });
    state.rooms[0].pending_frames.push(PendingFrame {
        frame: vec![4; 100 * 1024],
        message_index: None,
    });
    state.save().await?;

    let publication = state.publish_pending(0).await;
    let persisted = store_vault_data(&storage).await?;
    drop(std::fs::remove_dir_all(storage.files_dir()?));

    assert!(publication.is_err());
    assert_eq!(persisted.rooms[0].pending_frames.len(), 1);
    assert_eq!(persisted.rooms[0].pending_frames[0].frame.len(), 100 * 1024);
    Ok(())
}

async fn store_vault_data(storage: &Storage) -> Result<VaultData, cryptogram::Error> {
    let bytes = VaultStore::with_storage(*storage)
        .load()
        .await?
        .ok_or_else(|| cryptogram::Error::Vault("expected a persisted vault".to_owned()))?;
    Ok(serde_json::from_slice(&bytes)?)
}

#[test]
fn invitation_round_trips_without_exposing_the_secret_in_the_channel()
-> Result<(), cryptogram::Error> {
    let invitation = Invitation::random()?;
    let fragment = invitation.to_fragment();
    let parsed = Invitation::from_fragment(&fragment)?;
    assert_eq!(parsed.channel_id(), invitation.channel_id());
    assert_eq!(
        parsed.invite_secret().bytes(),
        invitation.invite_secret().bytes()
    );
    assert!(!fragment.contains("Telegram"));
    Ok(())
}

#[test]
fn telegram_fragment_rejects_malformed_extra_parts() -> Result<(), cryptogram::Error> {
    let invitation = Invitation::random()?;
    let fragment = invitation.to_fragment();
    let telegram_fragment = format!("{fragment}&tgWebAppData=opaque");
    assert_eq!(
        Invitation::from_telegram_fragment(&telegram_fragment)?.channel_id(),
        invitation.channel_id()
    );
    assert!(Invitation::from_telegram_fragment(&format!("{fragment}&broken")).is_err());
    assert!(Invitation::from_telegram_fragment(&format!("{fragment}&tgWebAppData")).is_err());
    Ok(())
}

#[test]
fn invitation_rejects_duplicate_unknown_and_noncanonical_fields() {
    let valid_channel = "AAAAAAAAAAAAAAAAAAAAAA";
    let valid_secret = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
    let duplicate = format!(
        "channel_id={valid_channel}&invite_secret={valid_secret}&channel_id={valid_channel}"
    );
    let unknown = format!("channel_id={valid_channel}&invite_secret={valid_secret}&extra=1");
    let noncanonical_channel = "AAAAAAAAAAAAAAAAAAAAAB";
    let noncanonical_secret = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB";

    assert!(Invitation::from_fragment(&duplicate).is_err());
    assert!(Invitation::from_fragment(&unknown).is_err());
    assert!(
        Invitation::from_fragment(&format!(
            "channel_id={noncanonical_channel}&invite_secret={valid_secret}"
        ))
        .is_err()
    );
    assert!(
        Invitation::from_fragment(&format!(
            "channel_id={valid_channel}&invite_secret={noncanonical_secret}"
        ))
        .is_err()
    );
}

#[test]
fn invitation_accepts_a_fragment_without_a_url_prefix() -> Result<(), cryptogram::Error> {
    let invitation = Invitation::random()?;
    let parsed = Invitation::from_url(&invitation.to_fragment())?;
    assert_eq!(parsed.channel_id(), invitation.channel_id());
    Ok(())
}

#[test]
fn oversized_chat_messages_are_rejected_during_deserialization() {
    let payload = serde_json::json!({ "text": "a".repeat(16 * 1024 + 1) });
    assert!(serde_json::from_value::<ChatMessage>(payload).is_err());
}

#[test]
fn invitation_rejects_oversized_fragments() {
    let oversized = format!(
        "channel_id={}&invite_secret={}",
        "A".repeat(257),
        "A".repeat(43)
    );
    assert!(Invitation::from_fragment(&oversized).is_err());
}

#[test]
fn outer_cipher_binds_the_payload_to_the_channel() -> Result<(), cryptogram::Error> {
    let invitation = Invitation::random()?;
    let cipher = OuterCipher::from_invitation(&invitation)?;
    let sealed = cipher.seal(b"opaque message")?;
    assert_eq!(cipher.open(&sealed)?, b"opaque message");

    let other = Invitation::random()?;
    let other_cipher = OuterCipher::from_invitation(&other)?;
    assert!(other_cipher.open(&sealed).is_err());
    Ok(())
}

#[test]
fn frame_tags_are_validated() -> Result<(), cryptogram::Error> {
    let encoded = cryptogram::protocol::encode_frame(FrameKind::Application, b"hello");
    assert_eq!(
        decode_frame(&encoded)?,
        (FrameKind::Application, b"hello".to_vec())
    );
    assert!(decode_frame(&[0]).is_err());
    Ok(())
}

#[test]
fn two_clients_exchange_an_mls_application_message() -> Result<(), cryptogram::Error> {
    let mut creator = client(1)?;
    let mut joiner = client(2)?;
    let group_id = creator.create_group()?;
    let key_package = joiner.key_package()?;
    let member_commit = creator.add_member(&group_id, &key_package)?;
    let (welcome, tree, _) = member_parts(&member_commit);
    let joined_group_id = joiner.join_group(&welcome, &tree)?;
    assert_eq!(joined_group_id, group_id);
    assert_eq!(creator.member_count(&group_id)?, 2);
    assert_eq!(joiner.member_count(&joined_group_id)?, 2);

    let frame = creator.encrypt_application(&group_id, "hello from the creator")?;
    let (frame_kind, frame_payload) = decode_frame(&frame)?;
    assert_eq!(frame_kind, FrameKind::Application);
    let creator_message = joiner.process_application(&joined_group_id, &frame_payload)?;
    assert_eq!(creator_message.text, "hello from the creator");

    let reply = joiner.encrypt_application(&joined_group_id, "hello from the joiner")?;
    let (reply_kind, reply_payload) = decode_frame(&reply)?;
    assert_eq!(reply_kind, FrameKind::Application);
    let joiner_message = creator.process_application(&group_id, &reply_payload)?;
    assert_eq!(joiner_message.text, "hello from the joiner");
    Ok(())
}

#[test]
fn a_third_member_can_join_the_same_mls_group() -> Result<(), cryptogram::Error> {
    let mut creator = client(3)?;
    let mut second = client(4)?;
    let mut third = client(5)?;
    let group_id = creator.create_group()?;
    let second_key_package = second.key_package()?;
    let second_commit = creator.add_member(&group_id, &second_key_package)?;
    let (second_welcome, second_tree, _) = member_parts(&second_commit);
    let second_group_id = second.join_group(&second_welcome, &second_tree)?;

    let third_key_package = third.key_package()?;
    let third_commit = creator.add_member(&group_id, &third_key_package)?;
    let (third_welcome, third_tree, third_commit_bytes) = member_parts(&third_commit);
    second.process_commit(&second_group_id, &third_commit_bytes)?;
    let third_group_id = third.join_group(&third_welcome, &third_tree)?;

    assert_eq!(creator.member_count(&group_id)?, 3);
    assert_eq!(second.member_count(&second_group_id)?, 3);
    assert_eq!(third.member_count(&third_group_id)?, 3);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn relay_stores_only_outer_ciphertext_and_preserves_cursor_order()
-> Result<(), cryptogram::Error> {
    let invitation = Invitation::random()?;
    let memory = MemoryRelay::default();
    let client = RelayClient::new(&invitation, RelayBackend::Memory(memory.clone()))?;
    let first = client.put_frame(&[1, 2, 3]).await?;
    let second = client.put_frame(&[4, 5, 6]).await?;
    assert!(first.value() < second.value());

    let stored = memory.get(client.channel(), 0)?;
    assert_eq!(stored.len(), 2);
    assert!(
        stored
            .iter()
            .all(|message| !message.payload.as_str().contains("AQIDBA"))
    );

    let after_first = client.get_frames(first.value()).await?;
    assert_eq!(after_first.len(), 1);
    assert_eq!(after_first[0].sequence.value(), second.value());
    assert_eq!(after_first[0].frame.as_deref(), Some(&[4, 5, 6][..]));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn application_state_completes_the_invitation_and_message_flow()
-> Result<(), cryptogram::Error> {
    let memory = MemoryRelay::default();
    let mut creator = AppState {
        relay_backend: RelayBackend::Memory(memory.clone()),
        ..AppState::default()
    };
    let mut joiner = AppState {
        relay_backend: RelayBackend::Memory(memory),
        ..AppState::default()
    };
    creator.create_account_local()?;
    joiner.create_account_local()?;
    assert_eq!(creator.create_room_local("group")?, 0);
    let invitation = creator.rooms[0].invitation();

    let (_, _, _) = joiner.prepare_join(&invitation)?;
    joiner.publish_pending(0).await?;

    let (creator_relay, creator_after) = creator.prepare_poll(0)?;
    let creator_messages = creator_relay.get_frames(creator_after).await?;
    creator.apply_poll(0, creator_messages)?;
    creator.publish_pending(0).await?;

    let (joiner_relay, joiner_after) = joiner.prepare_poll(0)?;
    let joiner_messages = joiner_relay.get_frames(joiner_after).await?;
    joiner.apply_poll(0, joiner_messages)?;
    joiner.publish_pending(0).await?;
    assert!(joiner.rooms[0].has_group());

    creator.draft = "state flow".to_owned();
    let (index, _) = creator.prepare_send()?;
    creator.publish_pending(index).await?;

    let (final_relay, final_after) = joiner.prepare_poll(0)?;
    let final_messages = final_relay.get_frames(final_after).await?;
    joiner.apply_poll(0, final_messages)?;
    assert!(joiner.rooms[0].pending_frames.is_empty());
    assert_eq!(
        joiner.rooms[0]
            .messages
            .last()
            .map(|message| message.text.as_str()),
        Some("state flow")
    );
    Ok(())
}

#[test]
fn relay_sequence_deserialization_rejects_unsafe_javascript_integers() {
    let unsafe_sequence = 9_007_199_254_740_992_u64;
    let message = serde_json::json!({ "sequence": unsafe_sequence, "payload": "AQ" });
    assert!(serde_json::from_value::<RelayMessage>(message).is_err());
    let response = serde_json::json!({ "sequence": unsafe_sequence });
    assert!(serde_json::from_value::<RelayPutResponse>(response).is_err());
}

#[tokio::test(flavor = "current_thread")]
async fn relay_rejects_an_unsafe_sequence_cursor() -> Result<(), cryptogram::Error> {
    let invitation = Invitation::random()?;
    let client = RelayClient::new(&invitation, RelayBackend::Memory(MemoryRelay::default()))?;
    assert!(
        client
            .get_frames(MAX_RELAY_SEQUENCE.saturating_add(1))
            .await
            .is_err()
    );
    Ok(())
}

#[test]
fn relay_payload_deserialization_rejects_noncanonical_data() {
    let malformed = r#"{"sequence":1,"payload":"A"}"#;
    assert!(serde_json::from_str::<cryptogram::protocol::RelayMessage>(malformed).is_err());
}

#[test]
fn polling_returns_pending_frames_kept_after_a_partial_poll() -> Result<(), cryptogram::Error> {
    let mut state = AppState::default();
    state.create_account_local()?;
    assert_eq!(state.create_room_local("group")?, 0);
    state.rooms[0].pending_frames.push(PendingFrame {
        frame: vec![1, 2, 3],
        message_index: None,
    });

    state.apply_poll(0, Vec::new())?;
    assert_eq!(state.rooms[0].pending_frames.len(), 1);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn a_bad_outer_payload_does_not_block_later_frames() -> Result<(), cryptogram::Error> {
    let invitation = Invitation::random()?;
    let memory = MemoryRelay::default();
    let client = RelayClient::new(&invitation, RelayBackend::Memory(memory.clone()))?;
    let other_invitation = Invitation::random()?;
    let other_cipher = OuterCipher::from_invitation(&other_invitation)?;
    assert!(
        memory.put(
            client.channel(),
            &OpaquePayload::new(other_cipher.seal(b"invalid outer payload")?)?,
        )? > 0
    );
    assert!(client.put_frame(b"valid frame").await?.value() > 0);

    let messages = client.get_frames(0).await?;
    assert_eq!(messages.len(), 2);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn simultaneous_joiners_join_the_same_group() -> Result<(), cryptogram::Error> {
    let memory = MemoryRelay::default();
    let mut creator = AppState {
        relay_backend: RelayBackend::Memory(memory.clone()),
        ..AppState::default()
    };
    let mut first = AppState {
        relay_backend: RelayBackend::Memory(memory.clone()),
        ..AppState::default()
    };
    let mut second = AppState {
        relay_backend: RelayBackend::Memory(memory),
        ..AppState::default()
    };
    creator.create_account_local()?;
    first.create_account_local()?;
    second.create_account_local()?;
    assert_eq!(creator.create_room_local("group")?, 0);
    let invitation = creator.rooms[0].invitation();

    drop(first.prepare_join(&invitation)?);
    first.publish_pending(0).await?;
    drop(second.prepare_join(&invitation)?);
    second.publish_pending(0).await?;

    let (creator_relay, creator_after) = creator.prepare_poll(0)?;
    let creator_messages = creator_relay.get_frames(creator_after).await?;
    creator.apply_poll(0, creator_messages)?;
    creator.publish_pending(0).await?;

    let (first_relay, first_after) = first.prepare_poll(0)?;
    let first_messages = first_relay.get_frames(first_after).await?;
    first.apply_poll(0, first_messages)?;
    first.publish_pending(0).await?;

    let (second_relay, second_after) = second.prepare_poll(0)?;
    let second_messages = second_relay.get_frames(second_after).await?;
    second.apply_poll(0, second_messages)?;
    second.publish_pending(0).await?;

    assert!(first.rooms[0].has_group());
    assert!(second.rooms[0].has_group());
    Ok(())
}

#[test]
fn preparing_a_join_and_send_creates_durable_outbox_entries() -> Result<(), cryptogram::Error> {
    let mut creator = AppState::default();
    let mut joiner = AppState::default();
    creator.create_account_local()?;
    joiner.create_account_local()?;
    assert_eq!(creator.create_room_local("group")?, 0);
    let invitation = creator.rooms[0].invitation();

    drop(joiner.prepare_join(&invitation)?);
    assert_eq!(joiner.rooms.len(), 1);
    assert_eq!(joiner.rooms[0].pending_frames.len(), 1);

    creator.draft = "queued".to_owned();
    drop(creator.prepare_send()?);
    assert_eq!(creator.rooms[0].pending_frames.len(), 1);
    assert_eq!(creator.rooms[0].messages.len(), 1);
    Ok(())
}

#[test]
fn telegram_sdk_does_not_log_event_data_or_persist_invitation_fields() {
    let sdk = include_str!("../assets/telegram-web-app.js");
    assert!(!sdk.contains("console.log('[Telegram.WebView] > postEvent'"));
    assert!(!sdk.contains("console.log('[Telegram.WebView] < receiveEvent'"));
    assert!(!sdk.contains("trustedTarget = '*'"));
    assert!(sdk.contains("sessionStorageSet('initParams', persistentParams)"));
}

#[test]
fn client_state_survives_snapshot_restore() -> Result<(), cryptogram::Error> {
    let mut creator = client(6)?;
    let mut joiner = client(7)?;
    let group_id = creator.create_group()?;
    let key_package = joiner.key_package()?;
    let member_commit = creator.add_member(&group_id, &key_package)?;
    let (welcome, tree, _) = member_parts(&member_commit);
    let joined_group_id = joiner.join_group(&welcome, &tree)?;
    assert_eq!(joined_group_id, group_id);

    let mut restored_client = Client::restore(creator.snapshot()?)?;
    let frame = restored_client.encrypt_application(&group_id, "persisted message")?;
    let (_, payload) = decode_frame(&frame)?;
    let message = joiner.process_application(&group_id, &payload)?;
    assert_eq!(message.text, "persisted message");
    Ok(())
}
