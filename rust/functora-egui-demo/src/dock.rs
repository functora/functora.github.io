use functora_egui::i18n::Language;
use functora_egui::route::RouteMetadata;
use functora_egui::{Button, ButtonVariant, Card, Flex, LucideIcon, ResponsiveExt};

use crate::route::AppRoute;

/// The buttons of the screen dock: the previous and the next screen.
#[derive(Debug, Default)]
pub struct ScreenDock {
    pub previous: Option<egui::Response>,
    pub next: Option<egui::Response>,
}

/// Which side of the dock a button sits on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DockSide {
    Previous,
    Next,
}

impl crate::state::ShowcaseApp {
    /// The dock at the bottom of every screen.
    ///
    /// It jumps to the previous and the next screen in the catalog order the
    /// sidebar, the command palette and the overview share. A screen without a
    /// neighbour on one side renders no button for it, and the remaining
    /// button keeps the side it belongs to.
    pub fn show_screen_dock(&mut self, ui: &mut egui::Ui, lang: Language) -> ScreenDock {
        let (previous, next) = self.router.current().neighbors();
        let ctx = ui.ctx().clone();
        let mut dock = ScreenDock::default();

        _ = Card::new().show(ui, |card_ui| {
            let mobile = card_ui.on_mobile();
            let flex = if mobile {
                Flex::column().gap(8.0).w_full()
            } else if next.is_some() && previous.is_some() {
                Flex::row().no_wrap().justify_between().gap(8.0).w_full()
            } else if next.is_some() {
                Flex::row().no_wrap().justify_end().gap(8.0).w_full()
            } else {
                Flex::row().no_wrap().justify_start().gap(8.0).w_full()
            };
            _ = flex.show(card_ui, |f| {
                dock.previous = previous.as_ref().map(|route| {
                    f.add(dock_button(route, lang, DockSide::Previous, mobile))
                        .inner
                });
                dock.next = next.as_ref().map(|route| {
                    f.add(dock_button(route, lang, DockSide::Next, mobile))
                        .inner
                });
            });
        });

        if dock.previous.as_ref().is_some_and(egui::Response::clicked) {
            self.navigate_to(previous.and_then(|route| route.component()));
            ctx.request_repaint();
        }
        if dock.next.as_ref().is_some_and(egui::Response::clicked) {
            self.navigate_to(next.and_then(|route| route.component()));
            ctx.request_repaint();
        }

        dock
    }
}

/// The button that jumps to the screen next to the current one.
fn dock_button(
    route: &AppRoute,
    lang: Language,
    side: DockSide,
    full_width: bool,
) -> Button<'static> {
    let (icon, variant) = match side {
        DockSide::Previous => (LucideIcon::ArrowLeft, ButtonVariant::Outline),
        DockSide::Next => (LucideIcon::ArrowRight, ButtonVariant::Default),
    };
    let button = Button::new(route.label(lang).into_owned())
        .icon(icon)
        .variant(variant)
        .single_line();
    if full_width {
        button.full_width()
    } else {
        button
    }
}
