use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::route::Screen;
use crate::storage::APP_ATTRS;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Button, ButtonVariant, Flex, TextareaPasteClear};

impl CryptonoteApp {
    pub(crate) fn screen_about(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let note = Msg::AboutText.render(lang);
        _ = functora_egui::markdown_view::show(ui, &mut self.md_cache, &note);
        let () = ui.add_space(12.0);
        _ = functora_egui::Hypertext::new()
            .text(BaseMsg::AboutAndroidBeta1.render(lang))
            .text(" ")
            .link(BaseMsg::AboutAndroidBetaLink1.render(lang), APP_ATTRS.beta_url())
            .text(format!(" {} ", BaseMsg::AboutAndroidBeta2.render(lang)))
            .link(BaseMsg::AboutAndroidBetaLink2.render(lang), APP_ATTRS.google_play_url())
            .text(BaseMsg::AboutAndroidBeta3.render(lang))
            .text(" ")
            .link(BaseMsg::AboutAndroidBetaLink3.render(lang), APP_ATTRS.apk_url())
            .text(format!(" {} ", BaseMsg::AboutAndroidBeta4.render(lang)))
            .size(12.0)
            .show(ui);
        let () = ui.add_space(12.0);
        let app_url = APP_ATTRS.app_url();
        let ctx = ui.ctx().clone();
        _ = functora_egui::QrImage::new(&app_url).show(ui);
        let () = ui.add_space(8.0);
        let mut app_url_text = app_url.clone();
        let resp = TextareaPasteClear::new(&mut app_url_text)
            .readonly()
            .min_height(80.0)
            .show(ui);
        self.paste_clear_feedback(resp, ui.ctx().input(|i| i.time));
        let () = ui.add_space(8.0);
        _ = Flex::row().gap(8.0).wrap().single_line_items().show(ui, |f| {
            self.dock_back_flex(f);
            if f.add(
                Button::new(BaseMsg::CopyAppLink.render(lang))
                    .icon(functora_egui::LucideIcon::Copy)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                let text = format!("{}\n{app_url}", Msg::ShareAppDesc.render(lang));
                self.clipboard_write_rx = Some(functora_egui::spawn_async(async move {
                    functora_egui::clipboard::write(text).await?;
                    Ok(())
                }));
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
                self.share_rx = Some(functora_egui::spawn_async(async move {
                    functora_egui::share::share(data).await?;
                    Ok(())
                }));
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
                    self.pwa_rx = Some(functora_egui::spawn_async(async move {
                        let outcome = functora_egui::pwa::trigger_pwa_install().await?;
                        Ok(match outcome {
                            functora_egui::pwa::PwaInstallOutcome::Accepted => BaseMsg::PwaInstallSuccess,
                            functora_egui::pwa::PwaInstallOutcome::Rejected => BaseMsg::PwaInstallRejected,
                            functora_egui::pwa::PwaInstallOutcome::NotAvailable => {
                                match functora_egui::pwa::install_hint().await? {
                                    functora_egui::pwa::InstallHint::Ios => BaseMsg::PwaInstallIos,
                                    functora_egui::pwa::InstallHint::Mac => BaseMsg::PwaInstallMac,
                                    functora_egui::pwa::InstallHint::Unavailable => BaseMsg::PwaInstallUnavailable,
                                }
                            }
                        })
                    }));
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
                    .icon(functora_egui::LucideIcon::Save)
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
        });
    }
}
