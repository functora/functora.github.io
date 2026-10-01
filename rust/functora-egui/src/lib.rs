//! functora-egui: shadcn/ui-inspired widgets for egui.
//!
//! Feature flags (see `Cargo.toml`):
//! - `platform`: cross-platform bridge (web/wasm, Android JNI, desktop)
//! - `clipboard`: clipboard read/write plus paste/clear input widgets
//! - `files`: file picking, downloads, blob previews, QR scanner widget
//! - `runtime`: async task spawning (`spawn_async`, worker)
//! - `storage`: persistent key-value storage
//! - `crypto`, `zip`, `package`: re-exported functora-core crypto/archive APIs
//! - `html-markdown`, `thumbnail`, `qr`: re-exported functora-core APIs
//! - `web`, `android`, `build`: platform runners, templates and codegen
//! - `images`: image loaders and bundled sample fixtures
//! - `camera`: camera capture APIs (module itself is gated on `platform`)
//! - `markdown`: markdown rendering (`markdown_view`, data-URL loader)

#[cfg(any(feature = "android", feature = "build"))]
pub mod android;
#[cfg(feature = "platform")]
pub mod camera;
#[cfg(feature = "platform")]
pub use camera::{
    FrameData, begin_capture_session, capture_frame, check_camera, sleep, start_camera,
    stop_camera, stop_capture_worker,
};
#[cfg(feature = "clipboard")]
pub mod clipboard;
#[cfg(any(feature = "web", feature = "android", feature = "build"))]
pub mod config;
pub mod deep_link;
#[cfg(feature = "files")]
pub mod download;
pub mod error;
#[cfg(feature = "files")]
pub mod files;

#[cfg(feature = "files")]
pub use files::BlobMemo;
#[cfg(feature = "files")]
pub use files::CancelToken;
#[cfg(feature = "files")]
pub use files::PickResult;
#[cfg(feature = "files")]
pub use files::cancel;
#[cfg(feature = "files")]
pub use files::data_url_mime;
#[cfg(feature = "files")]
pub use files::is_cancelled;
#[cfg(feature = "files")]
pub use files::mime_for_name;
#[cfg(feature = "files")]
pub use files::new_cancel_token;
#[cfg(feature = "files")]
pub use files::pick_files;
#[cfg(feature = "files")]
pub use files::pick_files_with_cancel;
#[cfg(feature = "files")]
pub use files::pick_files_with_progress;
#[cfg(feature = "files")]
pub use files::pick_files_with_shared_progress;
#[cfg(feature = "files")]
pub use files::preview_blob;
#[cfg(feature = "files")]
pub use files::revoke_blob_url;
#[cfg(all(feature = "files", feature = "thumbnail"))]
pub use files::video_thumbnail;

pub mod icons;
#[cfg(feature = "images")]
pub mod image_samples;
pub mod layout;
pub mod nav;
pub mod paint;
pub mod platform;
pub mod progress;
pub mod pwa;
pub use pwa::{InstallHint, PwaInstallOutcome, install_hint, trigger_pwa_install};
pub mod responsive;
pub mod route;
#[cfg(feature = "clipboard")]
pub mod share;
#[cfg(feature = "storage")]
pub mod storage;
#[cfg(feature = "storage")]
pub use storage::Persistent;
#[cfg(feature = "storage")]
pub use storage::{files_dir, load_state, persist_value};
pub mod theme;
pub mod theme_extra;
pub mod tokens;
pub mod utils;
#[cfg(feature = "runtime")]
pub use utils::spawn_async;
#[cfg(any(feature = "web", feature = "build"))]
pub mod web;
pub mod widgets;
pub mod worker;

#[cfg(feature = "crypto")]
pub mod crypto {
    pub use functora_core::crypto::*;
}
pub mod encoding {
    pub use functora_core::encoding::*;
}
pub mod i18n {
    pub use functora_core::i18n::*;
}
#[cfg(feature = "html-markdown")]
pub mod markdown {
    pub use functora_core::markdown::*;
}
#[cfg(feature = "markdown")]
pub mod markdown_loader;
#[cfg(feature = "markdown")]
pub mod markdown_view;
#[cfg(feature = "markdown")]
pub use egui_commonmark::{CommonMarkCache, CommonMarkViewer};
#[cfg(feature = "markdown")]
pub use markdown_loader::{WasmSafeDataUrlLoader, install_data_url_loader};
pub mod messages {
    pub use functora_core::messages::*;
}
#[cfg(feature = "thumbnail")]
pub mod thumbnail {
    pub use functora_core::thumbnail::*;
}
pub mod white_label {
    pub use functora_core::white_label::*;
}
#[cfg(feature = "package")]
pub mod package;
#[cfg(feature = "zip")]
pub mod zip;
#[cfg(feature = "qr")]
pub mod qr {
    pub use functora_core::qr::*;
}
pub mod in_flight;
pub mod state;

pub use responsive::breakpoint::Breakpoint;
pub use responsive::responsive_ext::ResponsiveExt;
pub use responsive::spacing::Spacing;

pub use layout::shell::Shell;
pub use layout::shell::initial_sidebar_collapsed;
pub use layout::shell::sidebar_effective_width;
pub use platform::android_back::BackOutcome;
pub use platform::android_back::{handle_system_back, is_back_pressed};
pub use widgets::navbar::widget::Navbar;

pub use egui_flex::FlexAlign;
pub use egui_flex::FlexItem;
pub use egui_flex::FlexJustify;
pub use icons::lucide_icon::LucideIcon;
pub use icons::paint_icon::paint_icon;
pub use icons::paint_icon::paint_icon_svg;
#[cfg(feature = "images")]
pub use image_samples::{ImageBytes, ImageSamples, ImageType, image_samples};
pub use in_flight::{InFlight, InFlightGuard};
pub use layout::center::center;
pub use layout::flex::Flex;
pub use layout::flex_instance::FlexInst;
pub use nav::NavHistory;
pub use progress::JobGuard;
pub use progress::{claim_job, clear_progress, report, report_progress, report_progress_named};
pub use route::AppRouter;
pub use route::BreadcrumbPosition;
pub use route::BreadcrumbSegment;
pub use route::COMPONENT_PARAM;
pub use route::ROOT_PATH;
pub use route::Routable;
pub use route::RouteKind;
pub use route::RouteMetadata;
pub use route::SCREEN_PARAM;
pub use route::breadcrumbs_for;
pub use route::{history_push, history_replace};
pub use state::PersistentState;
pub use theme::setup_fonts::setup_fonts;
pub use theme::setup_image_loaders::setup_image_loaders;
pub use theme::shadcn_theme::ShadcnTheme;
pub use theme::shadcn_theme_ext::ShadcnThemeExt;
pub use theme_extra::{default_theme, detect_system_theme, detect_system_theme_wasm};
pub use tokens::alert_variant::AlertVariant;
pub use tokens::badge_variant::BadgeVariant;
pub use tokens::button_variant::ButtonVariant;
pub use tokens::component_size::ComponentSize;
pub use tokens::item_variant::ItemVariant;
pub use tokens::sheet_side::SheetSide;
pub use tokens::toast_variant::ToastVariant;
pub use tokens::toggle_variant::ToggleVariant;
pub use tokens::typography_variant::TypographyVariant;
pub use widgets::accordion::widget::Accordion;
pub use widgets::alert::widget::Alert;
pub use widgets::alert_dialog::alert_dialog_show::AlertDialogResult;
pub use widgets::alert_dialog::widget::AlertDialog;
pub use widgets::area_chart::widget::AreaChart;
pub use widgets::area_chart::widget::AreaSeries;
pub use widgets::aspect_ratio::widget::AspectRatio;
pub use widgets::avatar::widget::Avatar;
pub use widgets::badge::widget::Badge;
pub use widgets::blocking_overlay::widget::BlockingOverlay;
pub use widgets::breadcrumb::{Breadcrumb, NavAction};
pub use widgets::button::widget::Button;
pub use widgets::button_group::widget::ButtonGroup;
pub use widgets::calendar::widget::Calendar;
#[cfg(feature = "platform")]
pub use widgets::camera_view::camera_view_state::CameraViewState;
#[cfg(feature = "platform")]
pub use widgets::camera_view::widget::CameraView;
pub use widgets::card::widget::Card;
pub use widgets::carousel::widget::Carousel;
pub use widgets::checkbox::widget::Checkbox;
pub use widgets::code_snippet::{snippet, snippet_break_long_words};
pub use widgets::collapsible::widget::Collapsible;
pub use widgets::color_swatch::widget::ColorSwatch;
pub use widgets::combobox::widget::Combobox;
pub use widgets::combobox::widget::ComboboxValue;
pub use widgets::command::widget::{Command, CommandItem, CommandValue};
pub use widgets::context_menu::widget::ContextMenu;
pub use widgets::date_picker::date_picker_state::DatePickerState;
pub use widgets::date_picker::widget::DatePicker;
pub use widgets::dialog::widget::Dialog;
pub use widgets::dropdown_menu::widget::DropdownMenu;
pub use widgets::dropdown_menu::widget::MenuItem;
pub use widgets::empty::widget::Empty;
pub use widgets::field::field_description::FieldDescription;
pub use widgets::field::field_group::FieldGroup;
pub use widgets::field::field_legend::FieldLegend;
pub use widgets::field::field_set::FieldSet;
pub use widgets::footer::widget::Footer;
pub use widgets::hover_card::widget::HoverCard;
pub use widgets::hyperlink::widget::Hyperlink;
pub use widgets::hypertext::widget::Hypertext;
pub use widgets::hypertext::widget::Segment;
pub use widgets::input::widget::Input;
pub use widgets::input_group::widget::InputGroup;
pub use widgets::input_otp::widget::InputOtp;
#[cfg(feature = "clipboard")]
pub use widgets::input_paste_clear::input_paste_clear_show::PasteClearResponse as InputPasteClearResponse;
#[cfg(feature = "clipboard")]
pub use widgets::input_paste_clear::widget::InputPasteClear;
pub use widgets::item::widget::Item;
pub use widgets::kbd::widget::Kbd;
pub use widgets::label::widget::Label;
pub use widgets::menubar::widget::Menubar;
pub use widgets::navigation_menu::widget::NavigationMenu;
pub use widgets::navigation_menu::widget::NavigationMenuValue;
pub use widgets::number_input::widget::NumberInput;
pub use widgets::pagination::widget::Pagination;
pub use widgets::popover::widget::Popover;
pub use widgets::progress::widget::Progress;
pub use widgets::property_grid::property_row::PropertyRow;
pub use widgets::property_grid::widget::PropertyGrid;
pub use widgets::qr_image::widget::QrImage;
#[cfg(feature = "files")]
pub use widgets::qr_scanner::qr_scanner_state::QrScannerState;
#[cfg(feature = "files")]
pub use widgets::qr_scanner::widget::QrScanner;
pub use widgets::radio::widget::Radio;
pub use widgets::radio_group::widget::RadioGroup;
pub use widgets::radio_group::widget::RadioGroupLabeled;
pub use widgets::resizable::widget::Resizable;
pub use widgets::scroll_area::widget::ScrollArea;
pub use widgets::select::widget::Select;
pub use widgets::select::widget::SelectLabeled;
pub use widgets::select::widget::SelectValue;
pub use widgets::select::widget::SelectValueLabeled;
pub use widgets::separator::widget::Separator;
pub use widgets::sheet::widget::Sheet;
pub use widgets::sidebar::widget::Sidebar;
pub use widgets::skeleton::widget::Skeleton;
pub use widgets::slider::widget::Slider;
pub use widgets::spinner::widget::Spinner;
pub use widgets::status_bar::widget::StatusBar;
pub use widgets::switch::widget::Switch;
pub use widgets::table::widget::Table;
pub use widgets::tabs::widget::IconTabs;
pub use widgets::tabs::widget::IconTabsValue;
pub use widgets::tabs::widget::TabEntry;
pub use widgets::tabs::widget::Tabs;
pub use widgets::tabs::widget::TabsValue;
pub use widgets::textarea::widget::Textarea;
#[cfg(feature = "clipboard")]
pub use widgets::textarea_paste_clear::textarea_paste_clear_show::PasteClearResponse as TextareaPasteClearResponse;
#[cfg(feature = "clipboard")]
pub use widgets::textarea_paste_clear::widget::TextareaPasteClear;
pub use widgets::toast::toast_entry::ToastEntry;
pub use widgets::toast::toast_state::ToastState;
pub use widgets::toggle::widget::Toggle;
pub use widgets::toggle_group::widget::ToggleGroup;
pub use widgets::toggle_group::widget::ToggleGroupValue;
pub use widgets::toolbar::widget::Toolbar;
pub use widgets::tooltip::widget::Tooltip;
pub use widgets::typography::widget::Typography;
pub use worker::Reporter;
pub use worker::run as worker_run;

pub use functora_core::{FUNCTORA_CORE_DATE, FUNCTORA_CORE_YEAR};
pub use theme_extra::{Theme, current_theme, set_theme};
