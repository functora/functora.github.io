use crate::catalog::{
    CATEGORIES, CategoryId, ComponentId, FooterSuffix, SearchCommandPlaceholder, SearchLabel,
    category_header, palette_entries, section_button,
};
use crate::route::AppRoute;
use crate::state::ShowcaseApp;
use functora_egui::i18n::{I18N, Language};
use functora_egui::state::PersistentState;
use functora_egui::storage::persist_value;
use functora_egui::{
    AlertDialog, AlertDialogResult, Button, ButtonVariant, CommandValue, Dialog, FieldDescription,
    Flex, Footer, Hypertext, Item, Label, LucideIcon, ResponsiveExt, Sheet, Shell, ToastVariant,
    Typography,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PendingNav {
    Overview,
    Component(ComponentId),
}

impl ShowcaseApp {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        functora_egui::setup_fonts(&cc.egui_ctx);
        functora_egui::setup_image_loaders(&cc.egui_ctx);
        let persistent =
            PersistentState::load_or_default(&cc.egui_ctx, "functora_egui_demo_persistent", ());
        functora_egui::theme_extra::set_theme(&cc.egui_ctx, persistent.theme);
        let initial_collapsed = functora_egui::initial_sidebar_collapsed(&cc.egui_ctx);
        let mut this = Self {
            persistent,
            sidebar_collapsed: initial_collapsed,
            ..Default::default()
        };
        this.apply_theme(&cc.egui_ctx);
        let initial = this.router.current().component();
        this.selected = initial;
        this.prev_selected = initial;
        this
    }

    fn apply_theme(&self, ctx: &egui::Context) {
        functora_egui::theme_extra::set_theme(ctx, self.persistent.theme);
    }

    fn reset_to_home(&mut self, ctx: &egui::Context) {
        let prev_persistent = self.persistent.clone();
        let previous = self.selected;
        *self = Self::default();
        self.persistent = PersistentState::with_system_defaults(ctx, ());
        if self.persistent != prev_persistent {
            persist_value("functora_egui_demo_persistent", &self.persistent);
        }
        self.sidebar_collapsed = ctx.on_mobile();
        self.prev_selected = previous;
        self.router.reset(AppRoute::default());
        self.selected = None;
        self.apply_theme(ctx);
        ctx.request_repaint();
    }

    fn handle_shortcuts(&mut self, ctx: &egui::Context) {
        if ctx.input(|i| i.modifiers.command && i.key_pressed(egui::Key::K)) {
            self.dialogs.command_open = !self.dialogs.command_open;
            self.command_search.clear();
            ctx.request_repaint();
        }
    }

    pub fn navigate_to(&mut self, target: Option<ComponentId>) {
        self.selected = target;
        let route = match target {
            Some(id) => AppRoute::Component(id),
            None => AppRoute::Overview,
        };
        self.router.navigate(route);
    }

    fn sync_from_router(&mut self) {
        let current = self.router.current().component();
        if current != self.selected {
            self.selected = current;
        }
    }

    pub fn show_app_footer(ui: &mut egui::Ui, lang: Language) {
        let suffix = FooterSuffix.render(lang);
        let _ = Footer::new().show(ui, |inner| {
            let _ = Hypertext::new()
                .text(format!("© {} ", functora_egui::FUNCTORA_CORE_YEAR))
                .link("Functora", "https://functora.github.io/")
                .text(suffix)
                .text(format!(
                    " {} {}.",
                    functora_egui::messages::Msg::VersionLabel.render(lang),
                    Self::DEMO_ATTRS.vsn
                ))
                .centered()
                .show(inner);
        });
    }

    fn render_overlays(&mut self, ctx: &egui::Context) {
        if self.dialogs.dialog_open {
            let mut close = false;
            Dialog::new()
                .title("Edit Profile")
                .description("Make changes to your profile here.")
                .show(ctx, &mut self.dialogs.dialog_open, |ui| {
                    _ = Label::new("Full name").show(ui);
                    ui.add_space(8.0);
                    _ = ui.add(
                        functora_egui::Input::new(&mut self.form.form_name)
                            .placeholder("Ada Lovelace"),
                    );
                    ui.add_space(8.0);
                    _ = Label::new("Bio").show(ui);
                    ui.add_space(8.0);
                    _ = ui.add(
                        functora_egui::Textarea::new(&mut self.form.form_comments)
                            .placeholder("Tell us about yourself...")
                            .desired_width(ui.available_width()),
                    );
                    ui.add_space(12.0);
                    _ = Flex::row().justify_end().gap(8.0).show(ui, |f| {
                        _ = f.add(
                            Button::new("Cancel")
                                .variant(ButtonVariant::Outline)
                                .size(functora_egui::ComponentSize::Sm),
                        );
                        if f.add(
                            Button::new("Save Changes")
                                .size(functora_egui::ComponentSize::Sm)
                                .icon(LucideIcon::Check),
                        )
                        .inner
                        .clicked()
                        {
                            close = true;
                        }
                    });
                });
            if close {
                self.dialogs.dialog_open = false;
                self.toast.add(
                    "Profile updated",
                    ToastVariant::Success,
                    ctx.input(|i| i.time),
                );
            }
        }

        if self.dialogs.alert_dialog_open {
            let result = AlertDialog::new(
                "Are you absolutely sure?",
                "This action cannot be undone. This will permanently delete your account.",
            )
            .destructive()
            .show(ctx, &mut self.dialogs.alert_dialog_open);
            if matches!(result, AlertDialogResult::Confirmed) {
                self.toast.add(
                    "Account deleted",
                    ToastVariant::Error,
                    ctx.input(|i| i.time),
                );
            }
        }

        if self.sheet_state.sheet_open {
            Sheet::new()
                .title("Sheet Panel")
                .description("A side sheet that slides in from the edge.")
                .side(self.sheet_state.sheet_side)
                .show(ctx, &mut self.sheet_state.sheet_open, |ui| {
                    _ = Label::new("Notifications").show(ui);
                    ui.add_space(4.0);
                    for (label, desc) in [
                        ("New comment", "Alice commented on your post."),
                        ("Build passed", "The release pipeline finished."),
                        ("Update ready", "functora-egui 0.2 is available."),
                    ] {
                        _ = Item::new().show(ui, |ui5| {
                            _ = ui5.vertical(|ui6| {
                                _ = Label::new(label).show(ui6);
                                FieldDescription::show(ui6, desc);
                            });
                        });
                    }
                });
        }

        if self.dialogs.command_open {
            let lang = self.persistent.language;
            let placeholder = SearchCommandPlaceholder.render(lang);
            let entries = palette_entries(lang);
            if let Some(target) = CommandValue::new(entries).placeholder(placeholder).show(
                ctx,
                &mut self.dialogs.command_open,
                &mut self.command_search,
            ) {
                self.navigate_to(target);
                ctx.request_repaint();
            }
        }
    }

    fn render_component(&mut self, ui: &mut egui::Ui, lang: Language) {
        _ = Typography::h3(self.selected.map_or("Overview", |id| id.name())).show(ui);
        ui.add_space(4.0);
        match self.selected {
            None => self.demo_overview(ui, lang),
            Some(ComponentId::Button) => self.demo_button(ui),
            Some(ComponentId::ButtonGroup) => self.demo_button_group(ui),
            Some(ComponentId::Checkbox) => self.demo_checkbox(ui),
            Some(ComponentId::Switch) => self.demo_switch(ui),
            Some(ComponentId::Radio) => self.demo_radio(ui),
            Some(ComponentId::RadioGroup) => self.demo_radio_group(ui),
            Some(ComponentId::Toggle) => self.demo_toggle(ui),
            Some(ComponentId::ToggleGroup) => self.demo_toggle_group(ui),
            Some(ComponentId::Slider) => self.demo_slider(ui),
            Some(ComponentId::Input) => self.demo_input(ui),
            Some(ComponentId::NumberInput) => self.demo_number_input(ui),
            Some(ComponentId::InputGroup) => self.demo_input_group(ui),
            Some(ComponentId::InputPasteClear) => self.demo_input_paste_clear(ui),
            Some(ComponentId::TextareaPasteClear) => self.demo_textarea_paste_clear(ui),
            Some(ComponentId::Textarea) => self.demo_textarea(ui),
            Some(ComponentId::Select) => self.demo_select(ui),
            Some(ComponentId::SelectValue) => self.demo_select_value(ui),
            Some(ComponentId::Combobox) => self.demo_combobox(ui),
            Some(ComponentId::InputOtp) => self.demo_input_otp(ui),
            Some(ComponentId::DatePicker) => self.demo_date_picker(ui),
            Some(ComponentId::ColorSwatch) => self.demo_color_swatch(ui),
            Some(ComponentId::Flex) => self.demo_flex(ui),
            Some(ComponentId::AspectRatio) => Self::demo_aspect_ratio(ui),
            Some(ComponentId::Card) => Self::demo_card(ui),
            Some(ComponentId::Collapsible) => self.demo_collapsible(ui),
            Some(ComponentId::Resizable) => self.demo_resizable(ui),
            Some(ComponentId::ScrollArea) => Self::demo_scroll_area(ui),
            Some(ComponentId::Separator) => Self::demo_separator(ui),
            Some(ComponentId::StatusBar) => Self::demo_status_bar(ui),
            Some(ComponentId::Tabs) => self.demo_tabs(ui),
            Some(ComponentId::IconTabs) => self.demo_icon_tabs(ui),
            Some(ComponentId::Toolbar) => self.demo_toolbar(ui),
            Some(ComponentId::Accordion) => self.demo_accordion(ui),
            Some(ComponentId::Navbar) => self.demo_navbar(ui),
            Some(ComponentId::Footer) => Self::demo_footer(ui),
            Some(ComponentId::Dialog) => self.demo_dialog(ui),
            Some(ComponentId::AlertDialog) => self.demo_alert_dialog(ui),
            Some(ComponentId::Sheet) => self.demo_sheet(ui),
            Some(ComponentId::Popover) => Self::demo_popover(ui),
            Some(ComponentId::HoverCard) => Self::demo_hover_card(ui),
            Some(ComponentId::Tooltip) => Self::demo_tooltip(ui),
            Some(ComponentId::ContextMenu) => self.demo_context_menu(ui),
            Some(ComponentId::DropdownMenu) => self.demo_dropdown_menu(ui),
            Some(ComponentId::Command) => self.demo_command(ui),
            Some(ComponentId::Menubar) => self.demo_menubar(ui),
            Some(ComponentId::NavigationMenu) => self.demo_navigation_menu(ui),
            Some(ComponentId::BlockingOverlay) => self.demo_blocking_overlay(ui),
            Some(ComponentId::Alert) => Self::demo_alert(ui),
            Some(ComponentId::Badge) => Self::demo_badge(ui),
            Some(ComponentId::Progress) => self.demo_progress(ui),
            Some(ComponentId::Skeleton) => Self::demo_skeleton(ui),
            Some(ComponentId::Spinner) => Self::demo_spinner(ui),
            Some(ComponentId::Toast) => self.demo_toast(ui),
            Some(ComponentId::Empty) => Self::demo_empty(ui),
            Some(ComponentId::Avatar) => Self::demo_avatar(ui),
            Some(ComponentId::Breadcrumb) => self.demo_breadcrumb(ui),
            Some(ComponentId::Calendar) => self.demo_calendar(ui),
            Some(ComponentId::Carousel) => self.demo_carousel(ui),
            Some(ComponentId::Pagination) => self.demo_pagination(ui),
            Some(ComponentId::Sidebar) => Self::demo_sidebar(ui),
            Some(ComponentId::Table) => Self::demo_table(ui),
            Some(ComponentId::AreaChart) => Self::demo_area_chart(ui),
            Some(ComponentId::Typography) => Self::demo_typography(ui),
            Some(ComponentId::Label) => self.demo_label(ui),
            Some(ComponentId::Kbd) => Self::demo_kbd(ui),
            Some(ComponentId::Item) => self.demo_item(ui),
            Some(ComponentId::Icons) => self.demo_icons(ui),
            Some(ComponentId::Image) => self.demo_image(ui),
            Some(ComponentId::Hyperlink) => Self::demo_hyperlink(ui),
            Some(ComponentId::Hypertext) => self.demo_hypertext(ui),
            Some(ComponentId::CodeSnippet) => Self::demo_code_snippet(ui),
            Some(ComponentId::FieldGroup) => self.demo_field_group(ui),
            Some(ComponentId::FieldSet) => self.demo_field_set(ui),
            Some(ComponentId::FieldLegend) => Self::demo_field_legend(ui),
            Some(ComponentId::FieldDescription) => self.demo_field_description(ui),
            Some(ComponentId::PropertyGrid) => self.demo_property_grid(ui),
            Some(ComponentId::PropertyRow) => self.demo_property_row(ui),
            Some(ComponentId::Breakpoint) => Self::demo_breakpoint(ui),
            Some(ComponentId::Spacing) => Self::demo_spacing(ui),
            Some(ComponentId::FlexWrap) => Self::demo_flex_wrap(ui),
            Some(ComponentId::TouchTarget) => self.demo_touch_target(ui),
            Some(ComponentId::Storage) => self.demo_storage(ui),
            Some(ComponentId::Clipboard) => self.demo_clipboard(ui),
            Some(ComponentId::Share) => self.demo_share(ui),
            Some(ComponentId::DeepLink) => self.demo_deep_link(ui),
            Some(ComponentId::Files) => self.demo_files(ui),
            Some(ComponentId::Download) => self.demo_download(ui),
            Some(ComponentId::Nav) => self.demo_nav(ui),
            Some(ComponentId::ProgressWorker) => self.demo_progress_worker(ui),
            Some(ComponentId::Pwa) => self.demo_pwa(ui),
            Some(ComponentId::Encoding) => self.demo_encoding(ui),
            Some(ComponentId::InFlight) => self.demo_in_flight(ui),
            Some(ComponentId::Camera) => self.demo_camera(ui),
            Some(ComponentId::QrScanner) => self.demo_qr_scanner(ui),
            Some(ComponentId::QrImage) => self.demo_qr_image(ui),
            Some(ComponentId::Thumbnail) => self.demo_thumbnail(ui),
            Some(ComponentId::Zip) => self.demo_zip(ui),
            Some(ComponentId::Crypto) => self.demo_crypto(ui),
            Some(ComponentId::Worker) => self.demo_worker(ui),
            Some(ComponentId::PlatformInfo) => self.demo_platform_info(ui),
            Some(ComponentId::Messages) => Self::demo_messages(ui),
            Some(ComponentId::Markdown) => self.demo_markdown(ui),
            Some(ComponentId::Package) => Self::demo_package(ui),
            Some(ComponentId::WhiteLabel) => Self::demo_white_label(ui),
        }
    }
}

impl eframe::App for ShowcaseApp {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        let ctx = ui.ctx().clone();
        self.apply_theme(&ctx);
        self.handle_shortcuts(&ctx);
        self.poll_platform_promises(&ctx);
        self.router.ui(ui);
        self.sync_from_router();
        #[cfg(target_os = "android")]
        crate::android::poll_ime(&ctx);
        let should_scroll_top = self.selected != self.prev_selected;
        if should_scroll_top {
            self.prev_selected = self.selected;
        }
        let mut persistent = std::mem::take(&mut self.persistent);
        let prev_persistent = persistent.clone();
        let mut collapsed_val = self.sidebar_collapsed;
        let route = self.router.current().clone();
        let history = self.router.history().clone();
        let needs_reset = std::cell::Cell::new(false);
        let needs_search = std::cell::Cell::new(false);
        let pending_nav = std::cell::Cell::new(None::<PendingNav>);
        let selected_snapshot = self.selected;
        let lang_cell = std::cell::Cell::new(persistent.language);
        let breadcrumb_action = Shell::new("functora-egui", &mut collapsed_val, {
            let lang_ref = &lang_cell;
            let pending_ref = &pending_nav;
            move |side_ui| {
                let cur_lang = lang_ref.get();
                let mut close = false;
                for (cat_id, _, items) in CATEGORIES {
                    let is_overview = *cat_id == CategoryId::Overview;
                    if is_overview {
                        side_ui.add_space(8.0);
                    } else {
                        category_header(side_ui, *cat_id, cur_lang);
                        side_ui.add_space(8.0);
                    }
                    for def in *items {
                        let (is_selected, pending) = match def.id {
                            None => (
                                selected_snapshot.is_none()
                                    && pending_ref
                                        .get()
                                        .is_none_or(|next| next == PendingNav::Overview),
                                PendingNav::Overview,
                            ),
                            Some(id) => (
                                Some(id) == selected_snapshot
                                    && pending_ref
                                        .get()
                                        .is_none_or(|next| next == PendingNav::Component(id)),
                                PendingNav::Component(id),
                            ),
                        };
                        if side_ui
                            .add(section_button(def, is_selected).full_width())
                            .clicked()
                        {
                            pending_ref.set(Some(pending));
                            close |= side_ui.on_mobile();
                            side_ui.ctx().request_repaint();
                        }
                    }
                    side_ui.add_space(8.0);
                }
                close
            }
        })
        .theme(&mut persistent.theme)
        .language(&lang_cell)
        .search(&SearchLabel.render(lang_cell.get()), Some("Ctrl K"))
        .on_brand(|| needs_reset.set(true))
        .on_search(|| needs_search.set(true))
        .sidebar_labels(
            CATEGORIES
                .iter()
                .flat_map(|(_, _, items)| items.iter().map(|d| d.name)),
        )
        .breadcrumb(&route, &history)
        .scroll_top(should_scroll_top)
        .footer({
            let lang_ref = &lang_cell;
            move |footer_ui| {
                Self::show_app_footer(footer_ui, lang_ref.get());
            }
        })
        .show(ui, |content_ui| {
            let cur_lang = lang_cell.get();
            self.render_component(content_ui, cur_lang);
        });
        persistent.language = lang_cell.get();
        self.sidebar_collapsed = collapsed_val;
        self.persistent = persistent;
        if let Some(target) = pending_nav.get() {
            match target {
                PendingNav::Overview => self.navigate_to(None),
                PendingNav::Component(id) => self.navigate_to(Some(id)),
            }
            ctx.request_repaint();
        }
        if prev_persistent != self.persistent {
            persist_value("functora_egui_demo_persistent", &self.persistent);
        }
        if needs_reset.get() {
            self.reset_to_home(&ctx);
        }
        if needs_search.get() {
            self.dialogs.command_open = true;
            self.command_search.clear();
        }
        if let Some(action) = breadcrumb_action {
            match action {
                functora_egui::NavAction::Back => {
                    let _ = self.router.go_back();
                    ctx.request_repaint();
                }
                functora_egui::NavAction::Forward => {
                    let _ = self.router.go_forward();
                    ctx.request_repaint();
                }
                functora_egui::NavAction::Route(nav_route) => {
                    self.navigate_to(nav_route.component());
                    ctx.request_repaint();
                }
            }
            self.sync_from_router();
        }
        self.render_overlays(&ctx);
        self.toast.show(&ctx);
    }
}
