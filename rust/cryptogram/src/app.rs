use std::sync::{Arc, Mutex, MutexGuard};

use functora_egui::white_label::{AppAttrs, WhiteLabelApp};
use functora_egui::{
    Alert, Badge, Button, Card, CommonMarkCache, Input, LucideIcon, ScrollArea, Textarea,
};

use crate::error::Error;
use crate::protocol::{Invitation, SequenceExt};
use crate::relay::RelayClient;
use crate::route::Screen;
use crate::state::{AppState, MessageDirection};

const ATTRS: AppAttrs = AppAttrs {
    app: "cryptogram",
    vsn: env!("CARGO_PKG_VERSION"),
    org: "functora",
    src: Some("rust"),
    dst: "apps",
    description: env!("CARGO_PKG_DESCRIPTION"),
};

const NAV_ENTRIES: [(Screen, LucideIcon); 4] = [
    (Screen::Home, LucideIcon::House),
    (Screen::Chat, LucideIcon::MessageCircle),
    (Screen::Security, LucideIcon::ShieldCheck),
    (Screen::About, LucideIcon::Info),
];

#[derive(Debug)]
enum AsyncResult {
    Load(Result<Box<AppState>, String>),
    Operation(Result<(), String>),
}

type AsyncOutcome = Result<AsyncResult, String>;

#[derive(Debug, Clone, Copy)]
enum UiAction {
    CreateAccount,
    CreateRoom,
    JoinRoom,
    Send,
    Poll,
    CopyInvite,
}

pub struct CryptogramApp {
    shell: WhiteLabelApp<Screen, ()>,
    state: Arc<Mutex<AppState>>,
    markdown_cache: CommonMarkCache,
    pending: Option<std::sync::mpsc::Receiver<AsyncOutcome>>,
}

impl CryptogramApp {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let state = Arc::new(Mutex::new(AppState::default()));
        let pending = Some(functora_egui::spawn_async(async move {
            Ok(AsyncResult::Load(
                AppState::load()
                    .await
                    .map(Box::new)
                    .map_err(|error| error.to_string()),
            ))
        }));
        Self {
            shell: WhiteLabelApp::new(&cc.egui_ctx, ATTRS, ()).with_nav_entries(NAV_ENTRIES),
            state,
            markdown_cache: CommonMarkCache::default(),
            pending,
        }
    }

    fn state_lock(&self) -> MutexGuard<'_, AppState> {
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    fn poll_receiver(&mut self, ctx: &egui::Context) {
        let Some(outcome) = functora_egui::poll_channel(&mut self.pending) else {
            return;
        };
        match outcome {
            functora_egui::PollOutcome::Disconnected
            | functora_egui::PollOutcome::Ready(Ok(AsyncResult::Operation(Ok(())))) => {}
            functora_egui::PollOutcome::Ready(
                Err(error) | Ok(AsyncResult::Load(Err(error)) | AsyncResult::Operation(Err(error))),
            ) => {
                self.state_lock().error = Some(error);
            }
            functora_egui::PollOutcome::Ready(Ok(AsyncResult::Load(Ok(state)))) => {
                *self.state_lock() = *state;
            }
        }
        ctx.request_repaint();
    }

    fn start_action(&mut self, action: UiAction, ctx: &egui::Context) {
        if self.pending.is_some() {
            return;
        }
        match action {
            UiAction::CreateAccount => {
                let state = Arc::clone(&self.state);
                self.pending = Some(functora_egui::spawn_async(async move {
                    let account_result = {
                        let mut guard = state
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                        guard.create_account_local()
                    };
                    let operation_result = match account_result {
                        Ok(()) => persist(&state).await,
                        Err(error) => Err(error),
                    };
                    Ok(AsyncResult::Operation(
                        operation_result.map_err(|error| error.to_string()),
                    ))
                }));
            }
            UiAction::CreateRoom => {
                let state = Arc::clone(&self.state);
                self.pending = Some(functora_egui::spawn_async(async move {
                    let room_result = {
                        let mut guard = state
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                        guard.create_room_local("New conversation").map(|_| ())
                    };
                    let operation_result = match room_result {
                        Ok(()) => persist(&state).await,
                        Err(error) => Err(error),
                    };
                    Ok(AsyncResult::Operation(
                        operation_result.map_err(|error| error.to_string()),
                    ))
                }));
            }
            UiAction::JoinRoom => {
                let parsed_invitation = {
                    let guard = self.state_lock();
                    Invitation::from_url(guard.invite_input.trim())
                };
                let invitation = match parsed_invitation {
                    Ok(parsed) => parsed,
                    Err(error) => {
                        self.state_lock().error = Some(error.to_string());
                        return;
                    }
                };
                let state = Arc::clone(&self.state);
                self.pending = Some(functora_egui::spawn_async(async move {
                    let prepared = {
                        let mut guard = state
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                        guard.prepare_join(&invitation)
                    };
                    let operation_result = match prepared {
                        Ok((index, _, _)) => {
                            async {
                                persist(&state).await?;
                                publish_pending(&state, index).await?;
                                persist(&state).await
                            }
                            .await
                        }
                        Err(error) => Err(error),
                    };
                    Ok(AsyncResult::Operation(
                        operation_result.map_err(|error| error.to_string()),
                    ))
                }));
            }
            UiAction::Send => {
                let state = Arc::clone(&self.state);
                self.pending = Some(functora_egui::spawn_async(async move {
                    let prepared = {
                        let mut guard = state
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                        guard.prepare_send()
                    };
                    let operation_result = match prepared {
                        Ok((index, _)) => {
                            async {
                                persist(&state).await?;
                                publish_pending(&state, index).await?;
                                persist(&state).await
                            }
                            .await
                        }
                        Err(error) => Err(error),
                    };
                    Ok(AsyncResult::Operation(
                        operation_result.map_err(|error| error.to_string()),
                    ))
                }));
            }
            UiAction::Poll => {
                let state = Arc::clone(&self.state);
                self.pending = Some(functora_egui::spawn_async(async move {
                    let prepared = {
                        let guard = state
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                        guard
                            .selected_room
                            .map(|index| {
                                guard.prepare_poll(index).map(|prepared| (index, prepared))
                            })
                            .transpose()
                    };
                    let operation_result = match prepared {
                        Ok(Some((index, (relay, after)))) => {
                            async {
                                let messages = relay.get_frames(after).await?;
                                {
                                    let mut guard = state
                                        .lock()
                                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                                    guard.apply_poll(index, messages)?
                                };
                                persist(&state).await?;
                                publish_pending(&state, index).await?;
                                persist(&state).await
                            }
                            .await
                        }
                        Ok(None) => Err(Error::Mls("select a conversation first".to_owned())),
                        Err(error) => Err(error),
                    };
                    Ok(AsyncResult::Operation(
                        operation_result.map_err(|error| error.to_string()),
                    ))
                }));
            }
            UiAction::CopyInvite => {
                let invitation_url = {
                    let guard = self.state_lock();
                    guard
                        .selected_room
                        .and_then(|index| guard.rooms.get(index))
                        .map(crate::state::RoomState::invitation_url)
                };
                if let Some(url) = invitation_url {
                    ctx.copy_text(url);
                }
            }
        }
        ctx.request_repaint();
    }
}

impl eframe::App for CryptogramApp {
    fn ui(&mut self, ui: &mut egui::Ui, frame: &mut eframe::Frame) {
        self.poll_receiver(ui.ctx());
        let language = self.shell.lang();
        let mut actions = Vec::new();
        let Self {
            shell,
            state,
            markdown_cache,
            ..
        } = self;
        let mut route_content = |content_ui: &mut egui::Ui, route: &Screen, _unit: &()| {
            let mut state_guard = state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let mut local_actions = Vec::new();
            match route {
                Screen::Home => render_home(content_ui, &mut state_guard, &mut local_actions),
                Screen::Chat => render_chat(content_ui, &mut state_guard, &mut local_actions),
                Screen::Security => render_security(content_ui, &state_guard),
                Screen::About => functora_egui::white_label::screen_about(
                    content_ui,
                    markdown_cache,
                    &ATTRS,
                    language,
                ),
                Screen::License => functora_egui::white_label::screen_license(
                    content_ui,
                    markdown_cache,
                    &ATTRS,
                    language,
                ),
                Screen::Privacy => functora_egui::white_label::screen_privacy(
                    content_ui,
                    markdown_cache,
                    &ATTRS,
                    language,
                ),
            }
            actions.extend(local_actions);
        };
        if let Some(action) = shell.ui(ui, frame, &mut route_content) {
            match action {
                functora_egui::NavAction::Back => {
                    let _ = shell.router_mut().go_back();
                }
                functora_egui::NavAction::Forward => {
                    let _ = shell.router_mut().go_forward();
                }
                functora_egui::NavAction::Route(route) => shell.navigate(route),
            }
        }
        for action in actions {
            self.start_action(action, ui.ctx());
        }
        if self.pending.is_some() {
            ui.ctx()
                .request_repaint_after(std::time::Duration::from_millis(250));
        }
    }
}

fn render_home(ui: &mut egui::Ui, state: &mut AppState, actions: &mut Vec<UiAction>) {
    let _ = Card::new()
        .heading("End-to-end encrypted conversations")
        .show(ui, |card_ui| {
            let _ = card_ui.label(
                "Messages are encrypted on this device with OpenMLS before they reach the relay.",
            );
            card_ui.add_space(8.0);
            if state.client.is_none() {
                if Button::new("Create pseudonymous account")
                    .show(card_ui)
                    .clicked()
                {
                    actions.push(UiAction::CreateAccount);
                }
            } else {
                let _ = card_ui.label("Account ready");
                let _ = Badge::new("MLS client ready").show(card_ui);
            }
        });
    ui.add_space(12.0);
    let _ = Card::new().heading("Start or join").show(ui, |card_ui| {
        if state.client.is_some() && Button::new("New conversation").show(card_ui).clicked() {
            actions.push(UiAction::CreateRoom);
        }
        card_ui.add_space(6.0);
        let _ = Input::new(&mut state.invite_input)
            .placeholder("Paste an invitation URL")
            .show(card_ui);
        if Button::new("Join with invitation").show(card_ui).clicked() {
            actions.push(UiAction::JoinRoom);
        }
    });
    if let Some(error) = &state.error {
        ui.add_space(8.0);
        let _ = Alert::new().title("Error").show(ui, |alert_ui| {
            let _ = alert_ui.label(error);
            let _ = alert_ui.label("The operation could not be completed.");
        });
    }
}

fn render_chat(ui: &mut egui::Ui, state: &mut AppState, actions: &mut Vec<UiAction>) {
    if state.rooms.is_empty() {
        let _ = ui.label("Create a conversation or join one from Home.");
        return;
    }
    let _ = Card::new().heading("Conversations").show(ui, |list_ui| {
        for (index, room) in state.rooms.iter().enumerate() {
            let selected = state.selected_room == Some(index);
            let label = if selected {
                format!("{} - selected", room.name)
            } else {
                room.name.clone()
            };
            if Button::new(label).show(list_ui).clicked() {
                state.selected_room = Some(index);
            }
        }
    });
    let Some(index) = state.selected_room else {
        return;
    };
    let Some(room) = state.rooms.get(index).cloned() else {
        return;
    };
    ui.add_space(10.0);
    let _ = Card::new()
        .heading(format!("{} - encrypted group", room.name))
        .show(ui, |chat_ui| {
            let _ = chat_ui.label(format!("Relay channel: {}", room.channel.as_str()));
            let _ = chat_ui.label(format!("Invitation: {}", room.invitation_url()));
            if Button::new("Copy invitation").show(chat_ui).clicked() {
                actions.push(UiAction::CopyInvite);
            }
            if Button::new("Refresh messages").show(chat_ui).clicked() {
                actions.push(UiAction::Poll);
            }
            chat_ui.add_space(6.0);
            let _ = ScrollArea::new(360.0).show(chat_ui, |scroll_ui| {
                for message in &room.messages {
                    let prefix = match message.direction {
                        MessageDirection::Incoming => "Peer",
                        MessageDirection::Outgoing => "You",
                    };
                    let _ = scroll_ui.label(format!("{prefix}: {}", message.text));
                }
            });
            chat_ui.add_space(6.0);
            let _ = Textarea::new(&mut state.draft)
                .placeholder("Write an encrypted message")
                .desired_width(chat_ui.available_width())
                .show(chat_ui);
            if Button::new("Send encrypted message")
                .show(chat_ui)
                .clicked()
            {
                actions.push(UiAction::Send);
            }
        });
}

fn render_security(ui: &mut egui::Ui, state: &AppState) {
    let _ = Card::new()
        .heading("Security model")
        .show(ui, |security_ui| {
            let _ = security_ui.label(
                "OpenMLS provides the group key schedule and authenticated membership changes.",
            );
            let _ = security_ui
                .label("Every relay payload is additionally sealed with ChaCha20-Poly1305.");
            let _ = security_ui.label(
                "The relay sees only an opaque channel id, ciphertext, and a sequence number.",
            );
            let _ = security_ui.label(
                "Invitation secrets stay in the URL fragment and are never sent to the relay.",
            );
            security_ui.add_space(6.0);
            if state.client.is_some() {
                let _ = security_ui.label("Local account: pseudonymous identity active.");
            } else {
                let _ = security_ui.label("No local account has been created.");
            }
        });
}

async fn publish_pending(state: &Arc<Mutex<AppState>>, index: usize) -> Result<(), Error> {
    let (relay, pending) = {
        let guard = state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let room = guard
            .rooms
            .get(index)
            .ok_or_else(|| Error::Mls("unknown conversation".to_owned()))?;
        (
            RelayClient::new(&room.invitation(), guard.relay_backend.clone())?,
            room.pending_frames.clone(),
        )
    };
    for item in pending {
        let sequence = relay.put_frame(&item.frame).await?;
        {
            let mut guard = state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let room = guard
                .rooms
                .get_mut(index)
                .ok_or_else(|| Error::Mls("unknown conversation".to_owned()))?;
            room.pending_frames
                .retain(|candidate| candidate.frame != item.frame);
            if let Some(message_index) = item.message_index
                && let Some(message) = room.messages.get_mut(message_index)
            {
                message.sequence = sequence.value();
            }
        }
        if item.message_index.is_some() {
            state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .draft
                .clear();
        }
        persist(state).await?;
    }
    Ok(())
}

async fn persist(state: &Arc<Mutex<AppState>>) -> Result<(), Error> {
    let (vault, data) = {
        let guard = state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        (guard.vault, guard.vault_data()?)
    };
    vault.save(&serde_json::to_vec(&data)?).await
}
