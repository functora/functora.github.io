use crate::app::CryptonoteApp;
use crate::encoding::generate_qr_code;
use crate::error::AppError;
use crate::messages::Msg;
use crate::route::Screen;
use crate::storage::APP_ATTRS;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Button, ButtonVariant, Flex};

impl CryptonoteApp {
    pub(crate) fn screen_about(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let note = Msg::AboutText.render(lang);
        _ = functora_egui::markdown_view::show(ui, &mut self.md_cache, &note);
        let () = ui.add_space(12.0);
        _ = functora_egui::Hypertext::new()
            .text(BaseMsg::AboutAndroidBeta1.render(lang))
            .text(" ")
            .link(
                BaseMsg::AboutAndroidBetaLink1.render(lang),
                APP_ATTRS.beta_url(),
            )
            .text(format!(" {} ", BaseMsg::AboutAndroidBeta2.render(lang)))
            .link(
                BaseMsg::AboutAndroidBetaLink2.render(lang),
                APP_ATTRS.google_play_url(),
            )
            .text(BaseMsg::AboutAndroidBeta3.render(lang))
            .text(" ")
            .link(
                BaseMsg::AboutAndroidBetaLink3.render(lang),
                APP_ATTRS.apk_url(),
            )
            .text(format!(" {} ", BaseMsg::AboutAndroidBeta4.render(lang)))
            .size(12.0)
            .show(ui);
        let () = ui.add_space(12.0);
        let app_url = APP_ATTRS.app_url();
        let ctx = ui.ctx().clone();
        if let Ok(svg) = generate_qr_code(&app_url) {
            _ = ui.add(
                egui::Image::from_bytes("bytes://app-qr.svg", svg.into_bytes())
                    .maintain_aspect_ratio(true)
                    .max_width(ui.available_width().min(280.0)),
            );
            let () = ui.add_space(8.0);
        }
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(
                Button::new(BaseMsg::CopyAppLink.render(lang))
                    .icon(functora_egui::LucideIcon::Copy)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                let text = format!("{}\n{app_url}", Msg::ShareAppDesc.render(lang));
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::clipboard::write(text).await.map_err(AppError::from)
                });
                self.clipboard_write_rx = Some(rx);
            }
            if f.add(
                Button::new(BaseMsg::ShareAppLink.render(lang))
                    .icon(functora_egui::LucideIcon::Share2)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                let data = functora_egui::share::ShareData {
                    title: APP_ATTRS.app_name(),
                    text: Msg::ShareAppDesc.render(lang),
                    url: app_url.clone(),
                };
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::share::share(data).await.map_err(AppError::from)
                });
                self.share_rx = Some(rx);
            }
            #[cfg(all(target_arch = "wasm32", not(target_os = "android")))]
            {
                if f.add(
                    Button::new(BaseMsg::PwaInstallPrompt.render(lang))
                        .icon(functora_egui::LucideIcon::Download)
                        .variant(ButtonVariant::Outline),
                )
                .inner
                .clicked()
                {
                    let rx = functora_egui::spawn_async(async move {
                        match functora_egui::camera::trigger_pwa_install().await {
                            Ok(functora_egui::camera::PwaInstallOutcome::Accepted) => Ok(BaseMsg::PwaInstallSuccess),
                            Ok(functora_egui::camera::PwaInstallOutcome::Rejected) => Ok(BaseMsg::PwaInstallRejected),
                            Ok(functora_egui::camera::PwaInstallOutcome::NotAvailable) => {
                                functora_egui::camera::install_hint()
                                    .await
                                    .map(|hint| match hint {
                                        functora_egui::camera::InstallHint::Ios => BaseMsg::PwaInstallIos,
                                        functora_egui::camera::InstallHint::Mac => BaseMsg::PwaInstallMac,
                                        functora_egui::camera::InstallHint::Unavailable => {
                                            BaseMsg::PwaInstallUnavailable
                                        }
                                    })
                                    .map_err(AppError::from)
                            }
                            Err(e) => Err(AppError::from(e)),
                        }
                    });
                    self.pwa_rx = Some(rx);
                }
            }
            if f.add(
                Button::new(BaseMsg::JoinTestingButton.render(lang))
                    .icon(functora_egui::LucideIcon::Users)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                ctx.open_url(egui::OpenUrl::new_tab(APP_ATTRS.beta_url()));
            }
            if f.add(
                Button::new(BaseMsg::GooglePlayButton.render(lang))
                    .icon(functora_egui::LucideIcon::Play)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                ctx.open_url(egui::OpenUrl::new_tab(APP_ATTRS.google_play_url()));
            }
            if f.add(
                Button::new(BaseMsg::DownloadApkButton.render(lang))
                    .icon(functora_egui::LucideIcon::Download)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                ctx.open_url(egui::OpenUrl::new_tab(APP_ATTRS.apk_url()));
            }
            if f.add(
                Button::new(BaseMsg::SourceCodeButton.render(lang))
                    .icon(functora_egui::LucideIcon::Github)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                ctx.open_url(egui::OpenUrl::new_tab(APP_ATTRS.source_url()));
            }
            if f.add(
                Button::new(BaseMsg::AuthorButton.render(lang))
                    .icon(functora_egui::LucideIcon::User)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                ctx.open_url(egui::OpenUrl::new_tab(APP_ATTRS.author_url()));
            }
            if f.add(
                Button::new(BaseMsg::Donate.render(lang))
                    .icon(functora_egui::LucideIcon::Heart)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.navigate(Screen::Donate);
            }
            if f.add(
                Button::new(BaseMsg::Back.render(lang))
                    .icon(functora_egui::LucideIcon::ArrowLeft)
                    .variant(ButtonVariant::Ghost),
            )
            .inner
            .clicked()
            {
                self.navigate(Screen::Home);
            }
        });
    }
}
