use crate::app::CryptonoteApp;
use crate::error::AppError;
use crate::messages::Msg;
use crate::progress::{Stage, claim_job};
use egui::ScrollArea;
use functora_egui::files::{Preview, format_size};
use functora_egui::i18n::I18N;
use functora_egui::{Alert, Button, ButtonVariant, Flex, Progress};

impl CryptonoteApp {
    pub(crate) fn screen_file(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let att_opt = self
            .temporary
            .attachment
            .and_then(|idx| self.temporary.attachments.get(idx.get()).cloned());
        if let Some(att) = att_opt {
            let size = format_size(att.data.len() as u64);
            _ = ui.label(egui::RichText::new(&att.name).size(18.0).strong());
            _ = ui.label(egui::RichText::new(size).small().weak());
            let () = ui.add_space(8.0);
            let preview = functora_egui::files::preview_cached(&att.name, &att.data);
            match preview {
                Preview::Image(_) => {
                    let uri = crate::screens::attachment_thumb::attachment_uri(&att.name);
                    _ = ui
                        .add(egui::Image::from_bytes(uri, att.data.clone()).max_width(ui.available_width().min(400.0)));
                }
                Preview::Text(t) => {
                    _ = ScrollArea::vertical().max_height(400.0).show(ui, |inner| {
                        _ = inner.label(egui::RichText::new(t).monospace());
                    });
                }
                Preview::Markdown(t) => {
                    _ = ScrollArea::vertical().max_height(400.0).show(ui, |inner| {
                        _ = functora_egui::markdown_view::show(inner, &mut self.md_cache, &t);
                    });
                }
                Preview::Video(_) | Preview::Audio(_) | Preview::Pdf(_) | Preview::Download => {
                    _ = Alert::new()
                        .title(Msg::PreviewUnavailable.render(lang))
                        .show(ui, |_| {});
                }
                Preview::Missing => {
                    _ = ui.label(Msg::FileNotFound.render(lang));
                }
            }
            let () = ui.add_space(12.0);
            _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
                self.dock_back_flex(f);
                if f.add(
                    Button::new(Msg::Download.render(lang))
                        .icon(functora_egui::LucideIcon::Download)
                        .variant(ButtonVariant::Default),
                )
                .inner
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
            });
            if let Some(job) = self.temporary.progress.clone() {
                let () = ui.add_space(8.0);
                _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
            }
        } else {
            _ = Alert::new()
                .title(Msg::FileNotFound.render(lang))
                .variant(functora_egui::AlertVariant::Destructive)
                .show(ui, |_| {});
        }
    }
}
