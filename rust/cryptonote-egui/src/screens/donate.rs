use crate::app::CryptonoteApp;
use crate::encoding::generate_qr_code;
use crate::error::AppError;
use crate::route::Screen;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Button, ButtonVariant, Card, ComponentSize};

impl CryptonoteApp {
    pub(crate) fn screen_donate(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        _ = ui.label(egui::RichText::new(BaseMsg::Donate.render(lang)).size(20.0).strong());
        let () = ui.add_space(8.0);
        _ = ui.label(BaseMsg::DonateGreeting.render(lang));
        let () = ui.add_space(4.0);
        _ = ui.label(BaseMsg::DonateIntro.render(lang));
        let () = ui.add_space(8.0);
        for block in functora_egui::white_label::donate_blocks() {
            _ = Card::new().show(ui, |inner| {
                _ = inner.label(egui::RichText::new(&block.label).strong());
                let () = inner.add_space(4.0);
                if let Ok(svg) = generate_qr_code(&block.address) {
                    _ = inner.add(
                        egui::Image::from_bytes(format!("bytes://donate-{}.svg", block.label), svg.into_bytes())
                            .maintain_aspect_ratio(true)
                            .max_width(200.0),
                    );
                    let () = inner.add_space(4.0);
                }
                _ = inner.label(&block.address);
                let () = inner.add_space(4.0);
                if inner
                    .add(
                        Button::new(BaseMsg::Copy.render(lang))
                            .icon(functora_egui::LucideIcon::Copy)
                            .size(ComponentSize::Sm),
                    )
                    .clicked()
                {
                    let addr = block.address.clone();
                    let rx = functora_egui::spawn_async(async move {
                        functora_egui::clipboard::write(addr).await.map_err(AppError::from)
                    });
                    self.clipboard_write_rx = Some(rx);
                }
            });
            let () = ui.add_space(8.0);
        }
        if ui
            .add(
                Button::new(BaseMsg::Back.render(lang))
                    .icon(functora_egui::LucideIcon::ArrowLeft)
                    .variant(ButtonVariant::Ghost),
            )
            .clicked()
        {
            self.navigate(Screen::Home);
        }
    }
}
