use crate::catalog::{
    Align, BlendMode, ComponentId, Framework, Fruit, Month, NavSection, ProfileTab, RadioOption,
    SettingsTab, Swatch, Tool, Year,
};
use crate::route::AppRoute;
use functora_egui::ToastState;
use functora_egui::i18n::Language;
use functora_egui::state::PersistentState;

pub(crate) type PickReceiver =
    std::sync::mpsc::Receiver<Result<Vec<(String, Vec<u8>)>, functora_egui::error::Error>>;
/// Ready thumbnail from `make_thumbnail`: image URI plus jpeg bytes.
pub type ThumbnailResult = Result<(String, Vec<u8>), String>;

/// Which crypto operation a pending `crypto_rx` task performs, so the shared
/// poll arm can label the result without guessing from the payload.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CryptoOp {
    Encrypt,
    Decrypt,
}

pub struct PlatformState {
    pub storage_key: String,
    pub storage_value: String,
    pub storage_persistent_text: String,
    pub clipboard_write: String,
    pub clipboard_read: String,
    pub clipboard_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub clipboard_write_rx: Option<std::sync::mpsc::Receiver<Result<(), String>>>,
    pub share_title: String,
    pub share_text: String,
    pub share_url: String,
    pub share_rx: Option<std::sync::mpsc::Receiver<Result<(), String>>>,
    pub deep_link_input: String,
    pub deep_link_current: String,
    pub picked: Vec<(String, Vec<u8>)>,
    pub pick_rx: Option<PickReceiver>,
    pub pick_cancel: Option<functora_egui::files::CancelToken>,
    pub pick_overlay_open: bool,
    pub pick_job: Option<functora_egui::progress::Job<functora_egui::progress::Stage>>,
    pub pick_progress: Option<
        std::sync::Arc<
            std::sync::Mutex<Option<functora_egui::progress::Job<functora_egui::progress::Stage>>>,
        >,
    >,
    pub download_name: String,
    pub download_text: String,
    pub download_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub progress_job: Option<functora_egui::progress::Job<functora_egui::progress::Stage>>,
    pub progress_running: bool,
    pub pwa_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub encode_input: String,
    pub encode_output: String,
    pub in_flight: functora_egui::in_flight::InFlight,
    pub camera_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub camera_view_state: functora_egui::CameraViewState,
    pub qr_state: functora_egui::QrScannerState,
    pub qr_continuous: bool,
    pub qr_input: String,
    pub qr_image_input: String,
    pub qr_last_scan: String,
    pub qr_error_notified: Option<String>,
    pub thumbnail_input: String,
    pub thumbnail_rx: Option<std::sync::mpsc::Receiver<ThumbnailResult>>,
    pub thumbnail_image: Option<(String, Vec<u8>)>,
    pub zip_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub crypto_input: String,
    pub crypto_password: String,
    pub crypto_output: String,
    pub crypto_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub crypto_op: Option<CryptoOp>,
    pub worker_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub platform_info: String,
    pub md_source: String,
    pub md_cache: functora_egui::CommonMarkCache,
}

impl Default for PlatformState {
    fn default() -> Self {
        Self {
            storage_key: "demo_key".to_owned(),
            storage_value: "hello".to_owned(),
            storage_persistent_text: functora_egui::storage::load_state::<String>(
                "demo_persistent",
            )
            .unwrap_or_else(|| "persistent hello".to_owned()),
            clipboard_write: "Hello from functora-egui!".to_owned(),
            clipboard_read: String::new(),
            clipboard_rx: None,
            clipboard_write_rx: None,
            share_title: "functora-egui".to_owned(),
            share_text: "Check out functora-egui".to_owned(),
            share_url: "https://functora.github.io".to_owned(),
            share_rx: None,
            deep_link_input: "https://functora.github.io/apps/demo/?page=about&lang=en".to_owned(),
            deep_link_current: String::new(),
            picked: Vec::new(),
            pick_rx: None,
            pick_cancel: None,
            pick_overlay_open: false,
            pick_job: None,
            pick_progress: None,
            download_name: "hello.txt".to_owned(),
            download_text: "Hello from functora-egui download!".to_owned(),
            download_rx: None,
            progress_job: None,
            progress_running: false,
            pwa_rx: None,
            encode_input: "hello world".to_owned(),
            encode_output: String::new(),
            in_flight: functora_egui::in_flight::InFlight::new(),
            camera_rx: None,
            camera_view_state: functora_egui::CameraViewState::new(),
            qr_state: functora_egui::QrScannerState::new(),
            qr_continuous: false,
            qr_input: "https://functora.github.io".to_owned(),
            qr_image_input: "https://functora.github.io".to_owned(),
            qr_last_scan: String::new(),
            qr_error_notified: None,
            thumbnail_input: "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEASABIAAD".to_owned(),
            thumbnail_rx: None,
            thumbnail_image: None,
            zip_rx: None,
            crypto_input: "hello world".to_owned(),
            crypto_password: "s3cret".to_owned(),
            crypto_output: String::new(),
            crypto_rx: None,
            crypto_op: None,
            worker_rx: None,
            platform_info: String::new(),
            md_source: "# Showcase\n\nCommonMark **rendering** with *emphasis*, ~~strikethrough~~, `inline code` and [links](https://example.com).\n\n## Lists\n\n- Unordered item one\n- Unordered item two\n\n1. Ordered item one\n2. Ordered item two\n\n- [x] Completed task\n- [ ] Open task\n\n> Blockquote with **nested** emphasis.\n\n```rust\nfn main() {\n    println!(\"Hello\");\n}\n```\n\n| Name | Value |\n| ---- | ----- |\n| Alpha | 1 |\n| Beta | 2 |\n\n![Red dot](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAFklEQVR4nGO4o6ZGEmIY1TCqYfhqAAATqigQ9JeO5gAAAABJRU5ErkJggg==)\n\nHere is a footnote[^1].\n\n---\n\n[^1]: Footnote text."
                .to_owned(),
            md_cache: functora_egui::CommonMarkCache::default(),
        }
    }
}

#[derive(Default)]
pub struct DialogState {
    pub command_open: bool,
    pub dialog_open: bool,
    pub alert_dialog_open: bool,
}

#[derive(Default)]
pub struct SheetState {
    pub sheet_open: bool,
    pub sheet_side: functora_egui::SheetSide,
}

pub struct CheckState {
    pub checkbox_val: bool,
    pub switch_val: bool,
    pub collapsible_open: bool,
}

impl Default for CheckState {
    fn default() -> Self {
        Self {
            checkbox_val: false,
            switch_val: false,
            collapsible_open: true,
        }
    }
}

pub struct RadioState {
    pub radio_a: bool,
    pub radio_b: bool,
    pub radio_c: bool,
}

impl Default for RadioState {
    fn default() -> Self {
        Self {
            radio_a: true,
            radio_b: false,
            radio_c: false,
        }
    }
}

#[derive(Default)]
pub struct TextStyleState {
    pub toggle_bold: bool,
    pub toggle_italic: bool,
    pub toggle_underline: bool,
}

pub struct ToolbarState {
    pub toolbar_tool: Tool,
    pub toolbar_snap: bool,
}

impl Default for ToolbarState {
    fn default() -> Self {
        Self {
            toolbar_tool: Tool::Pen,
            toolbar_snap: true,
        }
    }
}

pub struct FormState {
    pub form_name: String,
    pub form_card: String,
    pub form_cvv: String,
    pub form_month: Option<Month>,
    pub form_year: Option<Year>,
    pub form_comments: String,
    pub form_billing: bool,
}

impl Default for FormState {
    fn default() -> Self {
        Self {
            form_name: String::new(),
            form_card: String::new(),
            form_cvv: String::new(),
            form_month: None,
            form_year: None,
            form_comments: String::new(),
            form_billing: true,
        }
    }
}

#[derive(Default)]
pub struct DemoState {
    pub button_selected: bool,
    pub button_group_selected: usize,
    pub navbar_collapsed: bool,
    pub blocking_overlay_open: bool,
}

/// All state for the showcase demos, one field per interactive demo.
pub struct ShowcaseApp {
    pub persistent: PersistentState<()>,
    pub sidebar_collapsed: bool,
    pub selected: Option<ComponentId>,
    pub prev_selected: Option<ComponentId>,
    pub router: functora_egui::route::AppRouter<AppRoute>,
    pub dialogs: DialogState,
    pub command_search: String,
    pub toast: ToastState,
    pub sheet_state: SheetState,
    // inputs
    pub checks: CheckState,
    pub radios: RadioState,
    pub radio_group_val: RadioOption,
    pub text_style: TextStyleState,
    pub toggle_group_align: Align,
    pub slider_val: f64,
    pub slider_price: f64,
    pub touch_slider_val: f64,
    pub input_text: String,
    pub number_f64: f64,
    pub number_f32: f32,
    pub number_i32: i32,
    pub input_group_url: String,
    pub input_group_search: String,
    pub input_paste_clear_text: String,
    pub input_paste_clear_password: String,
    pub input_paste_clear_password_custom: String,
    pub input_paste_clear_custom_default: String,
    pub input_paste_clear_custom_icons: String,
    pub input_paste_clear_copy: String,
    pub input_paste_clear_copy_custom: String,
    pub textarea_text: String,
    pub textarea_compact: String,
    pub textarea_paste_clear_text: String,
    pub textarea_paste_clear_custom: String,
    pub textarea_paste_clear_copy: String,
    pub textarea_paste_clear_copy_custom: String,
    pub select_val: Option<Fruit>,
    pub select_blend: BlendMode,
    pub property_blend: BlendMode,
    pub combobox_framework: Option<Framework>,
    pub combobox_search: String,
    pub otp_value: String,
    pub date_picker: functora_egui::DatePickerState,
    pub color_swatch: Swatch,
    // layout
    pub accordion_open: Vec<usize>,
    pub settings_tab: SettingsTab,
    pub profile_tab: ProfileTab,
    pub nav_section: NavSection,
    pub input_password: String,
    pub pagination_page: usize,
    pub resizable_fraction: f32,
    pub flex_input: String,
    pub flex_first: String,
    pub flex_last: String,
    pub flex_email: String,
    pub flex_phone: String,
    pub label_email: String,
    pub field_set_email: String,
    pub field_description_password: String,
    pub toolbar: ToolbarState,
    pub navbar_language: std::cell::Cell<Language>,
    pub blocking_overlay_cancel: std::sync::Arc<std::sync::atomic::AtomicBool>,
    pub blocking_overlay_job: Option<functora_egui::progress::Job<functora_egui::progress::Stage>>,
    // feedback
    pub progress_val: f32,
    // data
    pub carousel_idx: usize,
    pub calendar_year: i32,
    pub calendar_month: u32,
    pub calendar_day: u32,
    // display
    pub icon_search: String,
    // forms
    pub prop_x: f64,
    pub prop_y: f64,
    pub prop_width: f64,
    pub prop_height: f64,
    pub prop_rotation: f64,
    pub prop_opacity: f64,
    pub form: FormState,
    pub platform: PlatformState,
    pub demo: DemoState,
}

impl Default for ShowcaseApp {
    fn default() -> Self {
        Self {
            persistent: PersistentState::default(),
            sidebar_collapsed: true,
            selected: None,
            prev_selected: None,
            router: functora_egui::route::AppRouter::new(&AppRoute::default()),
            dialogs: DialogState::default(),
            command_search: String::new(),
            toast: ToastState::new(),
            sheet_state: SheetState::default(),
            checks: CheckState::default(),
            radios: RadioState::default(),
            radio_group_val: RadioOption::default(),
            text_style: TextStyleState::default(),
            toggle_group_align: Align::default(),
            slider_val: 50.0,
            slider_price: 200.0,
            touch_slider_val: 50.0,
            input_text: String::new(),
            number_f64: 42.0,
            number_f32: std::f32::consts::PI,
            number_i32: 10,
            input_group_url: String::new(),
            input_group_search: String::new(),
            input_paste_clear_text: String::new(),
            input_paste_clear_password: String::new(),
            input_paste_clear_password_custom: String::new(),
            input_paste_clear_custom_default: "default value".to_owned(),
            input_paste_clear_custom_icons: String::new(),
            input_paste_clear_copy: String::new(),
            input_paste_clear_copy_custom: String::new(),
            textarea_text: String::new(),
            textarea_compact: String::new(),
            textarea_paste_clear_text: String::new(),
            textarea_paste_clear_custom: String::new(),
            textarea_paste_clear_copy: String::new(),
            textarea_paste_clear_copy_custom: String::new(),
            select_val: None,
            select_blend: BlendMode::default(),
            property_blend: BlendMode::default(),
            combobox_framework: None,
            combobox_search: String::new(),
            otp_value: String::new(),
            date_picker: functora_egui::DatePickerState::default(),
            color_swatch: Swatch::default(),
            accordion_open: vec![0],
            settings_tab: SettingsTab::default(),
            profile_tab: ProfileTab::default(),
            nav_section: NavSection::default(),
            input_password: String::new(),
            pagination_page: 0,
            resizable_fraction: 0.5,
            flex_input: String::new(),
            flex_first: String::new(),
            flex_last: String::new(),
            flex_email: String::new(),
            flex_phone: String::new(),
            label_email: String::new(),
            field_set_email: String::new(),
            field_description_password: String::new(),
            toolbar: ToolbarState::default(),
            navbar_language: std::cell::Cell::new(Language::default()),
            blocking_overlay_cancel: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
            blocking_overlay_job: None,
            progress_val: 0.66,
            carousel_idx: 0,
            calendar_year: 2026,
            calendar_month: 8,
            calendar_day: 20,
            icon_search: String::new(),
            prop_x: 124.0,
            prop_y: 88.0,
            prop_width: 320.0,
            prop_height: 180.0,
            prop_rotation: -8.0,
            prop_opacity: 92.0,
            form: FormState::default(),
            platform: PlatformState::default(),
            demo: DemoState::default(),
        }
    }
}
