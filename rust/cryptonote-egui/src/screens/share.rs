use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::screens::Screen;
use crate::state::{ActionMode, External};
use egui::Vec2;
use egui_shadcn::{Button, ButtonVariant, Card, LucideIcon, Spinner};
use functora_core::messages::Msg as BaseMsg;

impl CryptonoteApp {
    pub(crate) fn render_share(&mut self, ui: &mut egui::Ui) {
        let (url, qr) = match &self.external {
            External::Note(n) => (n.url.clone(), n.qr.clone()),
            _ => (String::new(), String::new()),
        };
        if !url.is_empty() {
            self.render_share_note(ui, &url, &qr);
        } else if matches!(self.external, External::Archive(_)) {
            self.render_share_archive(ui);
        } else if self.message.is_some() {
            self.render_note_buttons(ui, false);
        } else {
            let loading = self.text(&Msg::Base(BaseMsg::Loading));
            let _ = Card::new().heading(loading).show(ui, |card| {
                let _centered = card.horizontal(|row| {
                    _ = row.with_layout(
                        egui::Layout::centered_and_justified(egui::Direction::TopDown),
                        |center| {
                            _ = center.add(Spinner::new().size(40.0));
                        },
                    );
                });
            });
        }
    }

    fn render_share_note(&mut self, ui: &mut egui::Ui, url: &str, qr: &str) {
        let heading = self.text(&Msg::Share);
        let qr_image = if qr.is_empty() {
            None
        } else {
            self.qr_texture(url).map(|tex| (tex.id(), tex.size_vec2()))
        };
        let _ = Card::new().heading(heading).show(ui, |card| {
            if let Some((id, size)) = qr_image {
                let _centered = card.horizontal(|row| {
                    _ = row.with_layout(
                        egui::Layout::centered_and_justified(egui::Direction::TopDown),
                        |center| {
                            let side = center.available_width().min(size.x);
                            _ = center.add(egui::Image::new((id, Vec2::splat(side))));
                        },
                    );
                });
            }
            _ = card.add(
                egui::Label::new(egui::RichText::new(url).monospace())
                    .wrap()
                    .selectable(true),
            );
        });
        self.render_note_buttons(ui, true);
    }

    fn render_share_archive(&mut self, ui: &mut egui::Ui) {
        let heading = self.text(&Msg::ArchiveReady);
        let _ = Card::new().heading(heading).show(ui, |_card| {});
        let download_label = self.text(&Msg::Download);
        let view_label = self.text(&Msg::ViewButton);
        let edit_label = self.text(&Msg::EditNote);
        let reset_label = self.text(&Msg::CreateNewNote);
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&download_label).icon(LucideIcon::Download))
                .clicked()
            {
                if let Some(bytes) = app.external.archive_bytes() {
                    app.download("archive.cryptonote".to_string(), bytes);
                }
            }
            if row
                .add(
                    Button::new(&view_label)
                        .icon(LucideIcon::Eye)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.navigate(Screen::View);
            }
            if row
                .add(
                    Button::new(&edit_label)
                        .icon(LucideIcon::Pencil)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.action = ActionMode::Create;
                app.navigate(Screen::Home);
            }
            if row
                .add(
                    Button::new(&reset_label)
                        .icon(LucideIcon::Trash2)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.reset();
            }
        });
    }

    fn render_note_buttons(&mut self, ui: &mut egui::Ui, with_share: bool) {
        let url = self.external.note_url();
        let copy_label = self.text(&Msg::Base(BaseMsg::Copy));
        let share_label = self.text(&Msg::Share);
        let print_label = self.text(&Msg::Print);
        let view_label = self.text(&Msg::ViewButton);
        let edit_label = self.text(&Msg::EditNote);
        let reset_label = self.text(&Msg::CreateNewNote);
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if !url.is_empty()
                && row
                    .add(Button::new(&copy_label).icon(LucideIcon::Copy))
                    .clicked()
            {
                app.copy_text(url.clone());
            }
            if with_share {
                if row
                    .add(
                        Button::new(&share_label)
                            .icon(LucideIcon::Share2)
                            .variant(ButtonVariant::Outline),
                    )
                    .clicked()
                {
                    let text = app.text(&Msg::SharedNoteText);
                    app.social_share(text, url.clone());
                }
                if row
                    .add(Button::new(&print_label).variant(ButtonVariant::Outline))
                    .clicked()
                {
                    app.print();
                }
            }
            if row
                .add(
                    Button::new(&view_label)
                        .icon(LucideIcon::Eye)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.navigate(Screen::View);
            }
            if row
                .add(
                    Button::new(&edit_label)
                        .icon(LucideIcon::Pencil)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.action = ActionMode::Create;
                app.navigate(Screen::Home);
            }
            if row
                .add(
                    Button::new(&reset_label)
                        .icon(LucideIcon::Trash2)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.reset();
            }
        });
    }

    fn qr_texture(&mut self, url: &str) -> Option<&egui::TextureHandle> {
        if self.qr_texture.is_none() {
            if let Some((w, h, rgba)) = functora_core::qr::qr_rgba(url, 512) {
                let image =
                    egui::ColorImage::from_rgba_unmultiplied([w as usize, h as usize], &rgba);
                self.qr_texture = Some(self.ctx.load_texture(
                    "note-qr",
                    image,
                    egui::TextureOptions::NEAREST,
                ));
            }
        }
        self.qr_texture.as_ref()
    }
}
