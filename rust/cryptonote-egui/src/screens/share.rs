use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::screens::Screen;
use crate::state::{ActionMode, External};
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
            _ = ui.label(self.text(&Msg::ArchiveReady));
            self.render_archive_buttons(ui);
        } else if self.message.is_some() {
            self.render_note_buttons(ui, false);
        } else {
            _ = ui.label(self.text(&Msg::Base(BaseMsg::Loading)));
        }
    }

    fn render_share_note(&mut self, ui: &mut egui::Ui, url: &str, qr: &str) {
        if !qr.is_empty() {
            let texture = self.qr_texture(url);
            if let Some(tex) = &texture {
                _ = ui.add(egui::Image::new((tex.id(), tex.size_vec2())));
            }
        }
        _ = ui.add(
            egui::Label::new(egui::RichText::new(url).monospace())
                .wrap()
                .selectable(true),
        );
        self.render_note_buttons(ui, true);
    }

    fn render_note_buttons(&mut self, ui: &mut egui::Ui, with_share: bool) {
        let url = self.external.note_url();
        let _buttons = ui.horizontal(|buttons| {
            if !url.is_empty()
                && buttons
                    .button(self.text(&Msg::Base(BaseMsg::Copy)))
                    .clicked()
            {
                self.copy_text(url.clone());
            }
            if with_share {
                if buttons.button(self.text(&Msg::Share)).clicked() {
                    let text = self.text(&Msg::SharedNoteText);
                    self.social_share(text, url.clone());
                }
                if buttons.button(self.text(&Msg::Print)).clicked() {
                    self.print();
                }
            }
            if buttons.button(self.text(&Msg::ViewButton)).clicked() {
                self.navigate(Screen::View);
            }
            if buttons.button(self.text(&Msg::EditNote)).clicked() {
                self.action = ActionMode::Create;
                self.navigate(Screen::Home);
            }
            if buttons.button(self.text(&Msg::CreateNewNote)).clicked() {
                self.reset();
            }
        });
    }

    fn render_archive_buttons(&mut self, ui: &mut egui::Ui) {
        let _buttons = ui.horizontal(|buttons| {
            if buttons.button(self.text(&Msg::Download)).clicked() {
                if let Some(bytes) = self.external.archive_bytes() {
                    self.download("archive.cryptonote".to_string(), bytes);
                }
            }
            if buttons.button(self.text(&Msg::ViewButton)).clicked() {
                self.navigate(Screen::View);
            }
            if buttons.button(self.text(&Msg::EditNote)).clicked() {
                self.action = ActionMode::Create;
                self.navigate(Screen::Home);
            }
            if buttons.button(self.text(&Msg::CreateNewNote)).clicked() {
                self.reset();
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
