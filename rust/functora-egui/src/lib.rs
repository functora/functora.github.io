//! functora-egui: shadcn/ui-inspired widgets for egui.

#[cfg(any(feature = "android", feature = "build"))]
pub mod android;
pub mod camera;
pub mod clipboard;
#[cfg(any(feature = "web", feature = "android", feature = "build"))]
pub mod config;
pub mod deep_link;
pub mod download;
pub mod error;
pub mod files;

pub use files::CancelToken;
pub use files::PickResult;
pub use files::cancel;
pub use files::is_cancelled;
pub use files::new_cancel_token;

pub mod icons;
pub mod layout;
pub mod nav;
pub mod paint;
pub mod platform;
pub mod progress;
pub mod pwa;
pub mod responsive;
pub mod route;
pub mod share;
pub mod storage;
pub mod theme;
pub mod theme_extra;
pub mod tokens;
pub mod utils;
pub use utils::spawn_async;
#[cfg(any(feature = "web", feature = "build"))]
pub mod web;
pub mod widgets;
pub mod worker;

pub mod crypto {
    pub use functora_core::crypto::*;
}
pub mod encoding {
    pub use functora_core::encoding::*;
}
pub mod i18n {
    pub use functora_core::i18n::*;
}
pub mod markdown {
    pub use functora_core::markdown::*;
}
pub mod messages {
    pub use functora_core::messages::*;
}
pub mod thumbnail {
    pub use functora_core::thumbnail::*;
}
pub mod white_label {
    pub use functora_core::white_label::*;
}
pub mod package;
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

pub use egui_flex::FlexAlign;
pub use egui_flex::FlexItem;
pub use egui_flex::FlexJustify;
pub use icons::lucide_icon::LucideIcon;
pub use icons::paint_icon::paint_icon;
pub use icons::paint_icon::paint_icon_svg;
pub use layout::center::center;
pub use layout::flex::Flex;
pub use layout::flex_instance::FlexInst;
pub use nav::NavHistory;
pub use route::BreadcrumbSegment;
pub use route::Routable;
pub use route::RouteKind;
pub use route::RouteMetadata;
pub use route::breadcrumbs_for;
pub use theme::setup_fonts::setup_fonts;
pub use theme::shadcn_theme::ShadcnTheme;
pub use theme::shadcn_theme_ext::ShadcnThemeExt;
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
pub use widgets::camera_view::camera_view_state::CameraViewState;
pub use widgets::camera_view::widget::CameraView;
pub use widgets::card::widget::Card;
pub use widgets::carousel::widget::Carousel;
pub use widgets::checkbox::widget::Checkbox;
pub use widgets::code_snippet::{snippet, snippet_break_long_words};
pub use widgets::collapsible::widget::Collapsible;
pub use widgets::color_swatch::widget::ColorSwatch;
pub use widgets::combobox::widget::Combobox;
pub use widgets::command::widget::{Command, CommandItem};
pub use widgets::context_menu::widget::ContextMenu;
pub use widgets::date_picker::date_picker_state::DatePickerState;
pub use widgets::date_picker::widget::DatePicker;
pub use widgets::dialog::widget::Dialog;
pub use widgets::drawer::widget::Drawer;
pub use widgets::dropdown_menu::widget::DropdownMenu;
pub use widgets::dropdown_menu::widget::MenuItem;
pub use widgets::empty::widget::Empty;
pub use widgets::field::field_description::FieldDescription;
pub use widgets::field::field_group::FieldGroup;
pub use widgets::field::field_legend::FieldLegend;
pub use widgets::field::field_set::FieldSet;
pub use widgets::hover_card::widget::HoverCard;
pub use widgets::input::widget::Input;
pub use widgets::input_group::widget::InputGroup;
pub use widgets::input_otp::widget::InputOtp;
pub use widgets::item::widget::Item;
pub use widgets::kbd::widget::Kbd;
pub use widgets::label::widget::Label;
pub use widgets::menubar::widget::Menubar;
pub use widgets::navigation_menu::widget::NavigationMenu;
pub use widgets::number_input::widget::NumberInput;
pub use widgets::pagination::widget::Pagination;
pub use widgets::popover::widget::Popover;
pub use widgets::progress::widget::Progress;
pub use widgets::property_grid::property_row::PropertyRow;
pub use widgets::property_grid::widget::PropertyGrid;
pub use widgets::qr_scanner::qr_scanner_state::QrScannerState;
pub use widgets::qr_scanner::widget::QrScanner;
pub use widgets::radio::widget::Radio;
pub use widgets::radio_group::widget::RadioGroup;
pub use widgets::resizable::widget::Resizable;
pub use widgets::scroll_area::widget::ScrollArea;
pub use widgets::select::widget::Select;
pub use widgets::select::widget::SelectValue;
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
pub use widgets::tabs::widget::TabEntry;
pub use widgets::tabs::widget::Tabs;
pub use widgets::textarea::widget::Textarea;
pub use widgets::toast::toast_entry::ToastEntry;
pub use widgets::toast::toast_state::ToastState;
pub use widgets::toggle::widget::Toggle;
pub use widgets::toggle_group::widget::ToggleGroup;
pub use widgets::toolbar::widget::Toolbar;
pub use widgets::tooltip::widget::Tooltip;
pub use widgets::typography::widget::Typography;

pub use functora_core::{FUNCTORA_CORE_DATE, FUNCTORA_CORE_YEAR};
pub use theme_extra::{Theme, current_theme, set_theme};
