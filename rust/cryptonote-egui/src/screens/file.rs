use crate::app::{BYTES_URI_PREFIX, CryptonoteApp};
use crate::error::AppError;
use crate::messages::Msg;
use crate::progress::{Stage, claim_job};
use egui::ScrollArea;
use functora_egui::files::{Preview, format_size, preview};
use functora_egui::i18n::I18N;
use functora_egui::{Alert, Button, ButtonVariant, Progress};

impl CryptonoteApp {
    pub(crate) fn screen_file(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let att_opt = self
            .temporary
            .attachment
            .and_then(|idx| self.temporary.attachments.get(idx.get()).cloned());
        if let Some(att) = att_opt.clone() {
            let size = format_size(att.data.len() as u64);
            _ = ui.label(egui::RichText::new(&att.name).size(18.0).strong());
            _ = ui.label(egui::RichText::new(size).small().weak());
            let () = ui.add_space(8.0);
            let preview = preview(&att.name, &att.data);
            match preview {
                Preview::Image(_) => {
                    let uri = format!("{BYTES_URI_PREFIX}{}", att.name);
                    _ = ui
                        .add(egui::Image::from_bytes(uri, att.data.clone()).max_width(ui.available_width().min(400.0)));
                }
                Preview::Text(t) => {
                    _ = ScrollArea::vertical().max_height(400.0).show(ui, |inner| {
                        _ = inner.label(egui::RichText::new(t).monospace());
                    });
                }
                Preview::Markdown(t) => {
                    let rendered = crate::markdown::render_markdown(&t);
                    _ = ui.label(rendered);
                }
                Preview::Video(_) | Preview::Audio(_) | Preview::Pdf(_) => {
                    _ = Alert::new()
                        .title(Msg::PreviewUnavailable.render(lang))
                        .show(ui, |inner| {
                            _ = inner.label(Msg::PreviewUnavailable.render(lang));
                        });
                }
                Preview::Download => {
                    _ = ui.label(Msg::PreviewUnavailable.render(lang));
                }
                Preview::Missing => {
                    _ = ui.label(Msg::FileNotFound.render(lang));
                }
            }
            let () = ui.add_space(12.0);
            if ui
                .add(
                    Button::new(Msg::Download.render(lang))
                        .icon(functora_egui::LucideIcon::Download)
                        .variant(ButtonVariant::Default),
                )
                .clicked()
                && claim_job(&mut self.temporary.progress, Stage::Download).is_some()
            {
                let data = att.data.to_vec();
                let name = att.name.clone();
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::download::download(data, &name)
                        .await
                        .map_err(AppError::from)
                });
                self.download_rx = Some(rx);
            }
            if let Some(job) = self.temporary.progress.clone() {
                let () = ui.add_space(8.0);
                _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
            }
        } else {
            _ = Alert::new()
                .title(Msg::FileNotFound.render(lang))
                .variant(functora_egui::AlertVariant::Destructive)
                .show(ui, |inner| {
                    _ = inner.label(Msg::FileNotFound.render(lang));
                });
        }
    }
}
