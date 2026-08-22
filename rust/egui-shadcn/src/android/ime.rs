use std::sync::{Mutex, PoisonError};

use android_activity::input::{TextInputState, TextSpan};
use android_activity::AndroidApp;

static APP: Mutex<Option<AndroidApp>> = Mutex::new(None);

struct ImeTracker {
    last: Option<TextInputState>,
    last_focused: Option<egui::Id>,
}

static TRACKER: Mutex<ImeTracker> = Mutex::new(ImeTracker {
    last: None,
    last_focused: None,
});

pub fn store_app(app: AndroidApp) {
    _ = APP
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .replace(app);
}

pub fn poll_ime(ctx: &egui::Context) {
    let guard = APP.lock().unwrap_or_else(PoisonError::into_inner);
    let Some(app) = guard.as_ref() else { return };
    let focused = ctx.memory(egui::Memory::focused);
    let mut tracker = TRACKER.lock().unwrap_or_else(PoisonError::into_inner);
    if tracker.last_focused != focused {
        tracker.last_focused = focused;
        app.set_text_input_state(TextInputState::default());
        if focused.is_some() {
            app.show_soft_input(false);
        }
        tracker.last = None;
        ctx.input_mut(|input| input.events.push(preedit(&[])));
        ctx.request_repaint();
        return;
    }
    let state = app.text_input_state();
    let prev = tracker.last.replace(state.clone()).unwrap_or_default();
    let unchanged = prev.text == state.text
        && prev.compose_region.map(|s| (s.start, s.end))
            == state.compose_region.map(|s| (s.start, s.end));
    if unchanged {
        if focused.is_some() {
            ctx.request_repaint();
        }
        return;
    }
    let mut changed = false;
    ctx.input_mut(|input| changed = sync_ime(&prev, &state, &mut input.events));
    if changed {
        ctx.request_repaint();
    }
}

fn sync_ime(prev: &TextInputState, cur: &TextInputState, events: &mut Vec<egui::Event>) -> bool {
    let prev_chars: Vec<char> = prev.text.chars().collect();
    let cur_chars: Vec<char> = cur.text.chars().collect();
    let prev_committed = committed(&prev_chars, prev.compose_region);
    let cur_committed = committed(&cur_chars, cur.compose_region);
    let prev_composing = composing(&prev_chars, prev.compose_region);
    let cur_composing = composing(&cur_chars, cur.compose_region);
    let mut changed = false;
    if prev_composing.is_empty() || !cur_composing.is_empty() {
        if let Some(extra) = extra(prev_committed, cur_committed) {
            if !extra.is_empty() {
                events.push(egui::Event::Text(extra.iter().collect()));
                changed = true;
            }
        } else if prev_composing.is_empty()
            && cur_composing.is_empty()
            && cur_committed.len() < prev_committed.len()
            && !cur.text.is_empty()
        {
            events.push(egui::Event::Ime(egui::ImeEvent::DeleteSurrounding {
                before_chars: prev_committed.len() - cur_committed.len(),
                after_chars: 0,
            }));
            changed = true;
        }
    }
    match (prev_composing.is_empty(), cur_composing.is_empty()) {
        (_, false)
            if prev_composing.is_empty()
                || prev_committed != cur_committed
                || prev_composing != cur_composing =>
        {
            events.push(preedit(cur_composing));
            changed = true;
        }
        (false, true) => {
            if let Some(extra) = extra(prev_committed, cur_committed) {
                let commit: String = extra.iter().collect();
                if commit.is_empty() {
                    events.push(preedit(&[]));
                } else {
                    events.push(egui::Event::Ime(egui::ImeEvent::Commit(commit)));
                }
            } else {
                events.push(preedit(&[]));
                if cur_committed.len() < prev_committed.len() && !cur.text.is_empty() {
                    events.push(egui::Event::Ime(egui::ImeEvent::DeleteSurrounding {
                        before_chars: prev_committed.len() - cur_committed.len(),
                        after_chars: 0,
                    }));
                }
            }
            changed = true;
        }
        (true, _) | (false, false) => {}
    }
    changed
}

fn committed(text: &[char], compose: Option<TextSpan>) -> &[char] {
    let end = compose
        .map_or(text.len(), |span| span.start)
        .min(text.len());
    &text[..end]
}

fn composing(text: &[char], compose: Option<TextSpan>) -> &[char] {
    let Some(span) = compose else { return &[] };
    let start = span.start.min(text.len());
    let end = span.end.clamp(start, text.len());
    &text[start..end]
}

fn extra<'a>(prev: &'a [char], cur: &'a [char]) -> Option<&'a [char]> {
    cur.strip_prefix(prev)
}

fn preedit(chars: &[char]) -> egui::Event {
    egui::Event::Ime(egui::ImeEvent::Preedit {
        text: chars.iter().collect(),
        active_range_chars: None,
    })
}
