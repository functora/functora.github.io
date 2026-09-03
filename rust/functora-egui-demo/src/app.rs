//! Showcase app: an interactive catalog of every functora-egui widget,
//! layout, and feature, with light/dark theming and responsive behavior.

use functora_egui::i18n::{I18N, Language};
use functora_egui::state::PersistentState;
use functora_egui::storage::persist_value;
use functora_egui::{
    AlertDialog, AlertDialogResult, Button, ButtonVariant, Command, CommandItem, Dialog, Drawer,
    FieldDescription, Flex, Footer, Hypertext, Item, Label, LucideIcon, ResponsiveExt, Separator,
    Sheet, Shell, ToastState, ToastVariant, Typography,
};

pub use functora_egui::PickResult;
pub type PickReceiver = std::sync::mpsc::Receiver<PickResult>;

use crate::route::AppRoute;

pub struct PlatformState {
    pub storage_key: String,
    pub storage_value: String,
    pub storage_status: String,
    pub storage_persistent_text: String,
    pub clipboard_write: String,
    pub clipboard_read: String,
    pub clipboard_status: String,
    pub clipboard_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub clipboard_write_rx: Option<std::sync::mpsc::Receiver<Result<(), String>>>,
    pub share_title: String,
    pub share_text: String,
    pub share_url: String,
    pub share_status: String,
    pub share_rx: Option<std::sync::mpsc::Receiver<Result<(), String>>>,
    pub deep_link_input: String,
    pub deep_link_output: String,
    pub deep_link_current: String,
    pub picked: Vec<(String, Vec<u8>)>,
    pub pick_status: String,
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
    pub download_status: String,
    pub download_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub nav_input: String,
    pub progress_job: Option<functora_egui::progress::Job<functora_egui::progress::Stage>>,
    pub progress_running: bool,
    pub pwa_status: String,
    pub pwa_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub encode_input: String,
    pub encode_output: String,
    pub in_flight: functora_egui::in_flight::InFlight,
    pub in_flight_status: String,
    pub camera_status: String,
    pub camera_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub qr_state: functora_egui::QrScannerState,
    pub qr_continuous: bool,
    pub qr_input: String,
    pub qr_status: String,
    pub qr_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub qr_texture: Option<egui::TextureHandle>,
    pub thumbnail_input: String,
    pub thumbnail_status: String,
    pub thumbnail_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub thumbnail_texture: Option<egui::TextureHandle>,
    pub zip_status: String,
    pub zip_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub zip_picked: Vec<(String, Vec<u8>)>,
    pub crypto_input: String,
    pub crypto_password: String,
    pub crypto_output: String,
    pub crypto_status: String,
    pub worker_status: String,
    pub worker_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pub platform_info: String,
}

impl Default for PlatformState {
    fn default() -> Self {
        Self {
            storage_key: "demo_key".to_owned(),
            storage_value: "hello".to_owned(),
            storage_status: String::new(),
            storage_persistent_text: functora_egui::storage::load_state::<String>(
                "demo_persistent",
            )
            .unwrap_or_else(|| "persistent hello".to_owned()),
            clipboard_write: "Hello from functora-egui!".to_owned(),
            clipboard_read: String::new(),
            clipboard_status: String::new(),
            clipboard_rx: None,
            clipboard_write_rx: None,
            share_title: "functora-egui".to_owned(),
            share_text: "Check out functora-egui".to_owned(),
            share_url: "https://functora.github.io".to_owned(),
            share_status: String::new(),
            share_rx: None,
            deep_link_input: "https://functora.github.io/apps/demo/?page=about&lang=en".to_owned(),
            deep_link_output: String::new(),
            deep_link_current: String::new(),
            picked: Vec::new(),
            pick_status: String::new(),
            pick_rx: None,
            pick_cancel: None,
            pick_overlay_open: false,
            pick_job: None,
            pick_progress: None,
            download_name: "hello.txt".to_owned(),
            download_text: "Hello from functora-egui download!".to_owned(),
            download_status: String::new(),
            download_rx: None,
            nav_input: "/about".to_owned(),
            progress_job: None,
            progress_running: false,
            pwa_status: String::new(),
            pwa_rx: None,
            encode_input: "hello world".to_owned(),
            encode_output: String::new(),
            in_flight: functora_egui::in_flight::InFlight::new(),
            in_flight_status: String::new(),
            camera_status: String::new(),
            camera_rx: None,
            qr_state: functora_egui::QrScannerState::new(),
            qr_continuous: false,
            qr_input: "https://functora.github.io".to_owned(),
            qr_status: String::new(),
            qr_rx: None,
            qr_texture: None,
            thumbnail_input: "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEASABIAAD".to_owned(),
            thumbnail_status: String::new(),
            thumbnail_rx: None,
            thumbnail_texture: None,
            zip_status: String::new(),
            zip_rx: None,
            zip_picked: Vec::new(),
            crypto_input: "hello world".to_owned(),
            crypto_password: "s3cret".to_owned(),
            crypto_output: String::new(),
            crypto_status: String::new(),
            worker_status: String::new(),
            worker_rx: None,
            platform_info: String::new(),
        }
    }
}

/// A single showcase entry: one component or feature with a nav icon.
pub struct ComponentDef {
    pub name: &'static str,
    pub icon: LucideIcon,
}

impl ComponentDef {
    const fn new(name: &'static str, icon: LucideIcon) -> Self {
        Self { name, icon }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CategoryId {
    Overview,
    Inputs,
    Layout,
    Overlays,
    Feedback,
    Data,
    Display,
    Forms,
    Responsive,
    Platform,
}

impl CategoryId {
    #[must_use]
    pub fn label(self) -> &'static str {
        match self {
            Self::Overview => "Overview",
            Self::Inputs => "Inputs",
            Self::Layout => "Layout",
            Self::Overlays => "Overlays",
            Self::Feedback => "Feedback",
            Self::Data => "Data",
            Self::Display => "Display",
            Self::Forms => "Forms",
            Self::Responsive => "Responsive",
            Self::Platform => "Platform",
        }
    }

    #[must_use]
    pub fn icon(self) -> LucideIcon {
        match self {
            Self::Overview => LucideIcon::Sparkles,
            Self::Inputs => LucideIcon::Keyboard,
            Self::Layout => LucideIcon::LayoutGrid,
            Self::Overlays => LucideIcon::Layers,
            Self::Feedback => LucideIcon::BellRing,
            Self::Data => LucideIcon::Database,
            Self::Display => LucideIcon::Type,
            Self::Forms => LucideIcon::ListChecks,
            Self::Responsive => LucideIcon::MonitorSmartphone,
            Self::Platform => LucideIcon::Smartphone,
        }
    }
}

impl I18N for CategoryId {
    fn render_eng(&self) -> String {
        self.label().into()
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Overview => "Vista general",
            Self::Inputs => "Entradas",
            Self::Layout => "Diseño",
            Self::Overlays => "Superposiciones",
            Self::Feedback => "Comentarios",
            Self::Data => "Datos",
            Self::Display => "Visualización",
            Self::Forms => "Formularios",
            Self::Responsive => "Responsivo",
            Self::Platform => "Plataforma",
        }
        .into()
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Overview => "Обзор",
            Self::Inputs => "Ввод",
            Self::Layout => "Макет",
            Self::Overlays => "Наложения",
            Self::Feedback => "Обратная связь",
            Self::Data => "Данные",
            Self::Display => "Отображение",
            Self::Forms => "Формы",
            Self::Responsive => "Адаптивность",
            Self::Platform => "Платформа",
        }
        .into()
    }
}

pub struct OverviewBody;
impl I18N for OverviewBody {
    fn render_eng(&self) -> String {
        "Interactive showcase of 60+ shadcn/ui-inspired widgets for egui with light/dark themes and 1600+ Lucide icons. Browse via the sidebar or press Ctrl+K.".into()
    }

    fn render_spa(&self) -> String {
        "Presentación interactiva de más de 60 widgets para egui inspirados en shadcn/ui con temas claro/oscuro e iconos Lucide 1600+. Navega por la barra lateral o presiona Ctrl+K.".into()
    }

    fn render_rus(&self) -> String {
        "Интерактивная демонстрация 60+ виджетов для egui в стиле shadcn/ui с темами светлая/тёмная и 1600+ иконок Lucide. Откройте боковую панель или нажмите Ctrl+K.".into()
    }
}

pub struct SearchLabel;
impl I18N for SearchLabel {
    fn render_eng(&self) -> String {
        "Search".into()
    }

    fn render_spa(&self) -> String {
        "Buscar".into()
    }

    fn render_rus(&self) -> String {
        "Поиск".into()
    }
}

pub struct SearchCommandPlaceholder;
impl I18N for SearchCommandPlaceholder {
    fn render_eng(&self) -> String {
        "Search components...".into()
    }

    fn render_spa(&self) -> String {
        "Buscar componentes...".into()
    }

    fn render_rus(&self) -> String {
        "Поиск компонентов...".into()
    }
}

pub struct FooterSuffix;
impl I18N for FooterSuffix {
    fn render_eng(&self) -> String {
        ". All rights reserved.".into()
    }

    fn render_spa(&self) -> String {
        ". Todos los derechos reservados.".into()
    }

    fn render_rus(&self) -> String {
        ". Все права защищены.".into()
    }
}

/// Catalog of every component grouped by category.
pub const CATEGORIES: &[(CategoryId, LucideIcon, &[ComponentDef])] = &[
    (
        CategoryId::Overview,
        LucideIcon::Sparkles,
        &[ComponentDef::new("Overview", LucideIcon::Sparkles)],
    ),
    (
        CategoryId::Inputs,
        LucideIcon::Keyboard,
        &[
            ComponentDef::new("Button", LucideIcon::MousePointer2),
            ComponentDef::new("Checkbox", LucideIcon::SquareCheckBig),
            ComponentDef::new("Switch", LucideIcon::ToggleRight),
            ComponentDef::new("Radio", LucideIcon::CircleDot),
            ComponentDef::new("RadioGroup", LucideIcon::CircleCheckBig),
            ComponentDef::new("Toggle", LucideIcon::Bold),
            ComponentDef::new("ToggleGroup", LucideIcon::AlignCenterHorizontal),
            ComponentDef::new("Slider", LucideIcon::SlidersHorizontal),
            ComponentDef::new("Input", LucideIcon::SquarePen),
            ComponentDef::new("NumberInput", LucideIcon::Hash),
            ComponentDef::new("InputGroup", LucideIcon::Combine),
            ComponentDef::new("InputPasteClear", LucideIcon::ClipboardPaste),
            ComponentDef::new("TextareaPasteClear", LucideIcon::ClipboardPaste),
            ComponentDef::new("Textarea", LucideIcon::TextCursorInput),
            ComponentDef::new("Select", LucideIcon::ListFilter),
            ComponentDef::new("SelectValue", LucideIcon::ListEnd),
            ComponentDef::new("Combobox", LucideIcon::SquareChartGantt),
            ComponentDef::new("InputOtp", LucideIcon::CircleDashed),
            ComponentDef::new("DatePicker", LucideIcon::Calendar),
            ComponentDef::new("ColorSwatch", LucideIcon::Paintbrush),
        ],
    ),
    (
        CategoryId::Layout,
        LucideIcon::LayoutGrid,
        &[
            ComponentDef::new("Flex", LucideIcon::GripHorizontal),
            ComponentDef::new("AspectRatio", LucideIcon::Ratio),
            ComponentDef::new("Card", LucideIcon::SquareStack),
            ComponentDef::new("Collapsible", LucideIcon::ChevronsDownUp),
            ComponentDef::new("Resizable", LucideIcon::MoveHorizontal),
            ComponentDef::new("ScrollArea", LucideIcon::PanelsTopLeft),
            ComponentDef::new("Separator", LucideIcon::Minus),
            ComponentDef::new("StatusBar", LucideIcon::PanelBottom),
            ComponentDef::new("Tabs", LucideIcon::SquareMenu),
            ComponentDef::new("IconTabs", LucideIcon::AppWindow),
            ComponentDef::new("Toolbar", LucideIcon::Wrench),
            ComponentDef::new("Accordion", LucideIcon::ChevronsUpDown),
        ],
    ),
    (
        CategoryId::Overlays,
        LucideIcon::Layers,
        &[
            ComponentDef::new("Dialog", LucideIcon::AppWindow),
            ComponentDef::new("AlertDialog", LucideIcon::TriangleAlert),
            ComponentDef::new("Sheet", LucideIcon::PanelRight),
            ComponentDef::new("Drawer", LucideIcon::PanelBottomOpen),
            ComponentDef::new("Popover", LucideIcon::PanelTopOpen),
            ComponentDef::new("HoverCard", LucideIcon::SquareMousePointer),
            ComponentDef::new("Tooltip", LucideIcon::MousePointerClick),
            ComponentDef::new("ContextMenu", LucideIcon::List),
            ComponentDef::new("DropdownMenu", LucideIcon::Menu),
            ComponentDef::new("Command", LucideIcon::Command),
            ComponentDef::new("Menubar", LucideIcon::SquareMenu),
            ComponentDef::new("NavigationMenu", LucideIcon::Navigation),
        ],
    ),
    (
        CategoryId::Feedback,
        LucideIcon::BellRing,
        &[
            ComponentDef::new("Alert", LucideIcon::CircleAlert),
            ComponentDef::new("Badge", LucideIcon::BadgeCheck),
            ComponentDef::new("Progress", LucideIcon::Gauge),
            ComponentDef::new("Skeleton", LucideIcon::RectangleHorizontal),
            ComponentDef::new("Spinner", LucideIcon::LoaderCircle),
            ComponentDef::new("Toast", LucideIcon::BellRing),
            ComponentDef::new("Empty", LucideIcon::Inbox),
        ],
    ),
    (
        CategoryId::Data,
        LucideIcon::Database,
        &[
            ComponentDef::new("Avatar", LucideIcon::CircleUser),
            ComponentDef::new("Breadcrumb", LucideIcon::ChevronsRight),
            ComponentDef::new("Calendar", LucideIcon::Calendar),
            ComponentDef::new("Carousel", LucideIcon::Images),
            ComponentDef::new("Pagination", LucideIcon::ChevronsLeft),
            ComponentDef::new("Sidebar", LucideIcon::PanelLeft),
            ComponentDef::new("Table", LucideIcon::Table2),
            ComponentDef::new("AreaChart", LucideIcon::ChartArea),
        ],
    ),
    (
        CategoryId::Display,
        LucideIcon::Type,
        &[
            ComponentDef::new("Typography", LucideIcon::Type),
            ComponentDef::new("Label", LucideIcon::Tag),
            ComponentDef::new("Kbd", LucideIcon::Keyboard),
            ComponentDef::new("Item", LucideIcon::Rows3),
            ComponentDef::new("Icons", LucideIcon::Component),
        ],
    ),
    (
        CategoryId::Forms,
        LucideIcon::ListChecks,
        &[
            ComponentDef::new("FieldGroup", LucideIcon::Boxes),
            ComponentDef::new("FieldSet", LucideIcon::Box),
            ComponentDef::new("FieldLegend", LucideIcon::List),
            ComponentDef::new("FieldDescription", LucideIcon::FileText),
            ComponentDef::new("PropertyGrid", LucideIcon::SlidersVertical),
            ComponentDef::new("PropertyRow", LucideIcon::Rows3),
        ],
    ),
    (
        CategoryId::Responsive,
        LucideIcon::MonitorSmartphone,
        &[
            ComponentDef::new("Breakpoint", LucideIcon::MonitorSmartphone),
            ComponentDef::new("Spacing", LucideIcon::Ruler),
            ComponentDef::new("FlexWrap", LucideIcon::GripHorizontal),
            ComponentDef::new("TouchTarget", LucideIcon::Hand),
            ComponentDef::new("MobileDialog", LucideIcon::Smartphone),
            ComponentDef::new("MobileSidebar", LucideIcon::PanelLeftOpen),
        ],
    ),
    (
        CategoryId::Platform,
        LucideIcon::Smartphone,
        &[
            ComponentDef::new("Storage", LucideIcon::Database),
            ComponentDef::new("Clipboard", LucideIcon::Clipboard),
            ComponentDef::new("Share", LucideIcon::Share2),
            ComponentDef::new("DeepLink", LucideIcon::Link),
            ComponentDef::new("Files", LucideIcon::Files),
            ComponentDef::new("Download", LucideIcon::Download),
            ComponentDef::new("Nav", LucideIcon::Navigation),
            ComponentDef::new("ProgressWorker", LucideIcon::LoaderCircle),
            ComponentDef::new("PWA", LucideIcon::Globe),
            ComponentDef::new("Encoding", LucideIcon::Code),
            ComponentDef::new("InFlight", LucideIcon::ShieldCheck),
            ComponentDef::new("Camera", LucideIcon::Camera),
            ComponentDef::new("QrScanner", LucideIcon::ScanQrCode),
            ComponentDef::new("Thumbnail", LucideIcon::Image),
            ComponentDef::new("Zip", LucideIcon::FileArchive),
            ComponentDef::new("Crypto", LucideIcon::Lock),
            ComponentDef::new("Worker", LucideIcon::Cog),
            ComponentDef::new("PlatformInfo", LucideIcon::Info),
            ComponentDef::new("Messages", LucideIcon::Languages),
            ComponentDef::new("Markdown", LucideIcon::FileText),
            ComponentDef::new("Package", LucideIcon::Package),
            ComponentDef::new("WhiteLabel", LucideIcon::Tag),
        ],
    ),
];

/// Flat index of a component across all categories.
#[must_use]
pub fn flat_index(cat: usize, item: usize) -> usize {
    CATEGORIES
        .iter()
        .take(cat)
        .map(|(_, _, items)| items.len())
        .sum::<usize>()
        + item
}

/// Total number of components.
#[must_use]
pub fn component_count() -> usize {
    CATEGORIES
        .iter()
        .map(|(_, _, items)| items.len())
        .sum::<usize>()
}

/// Name of the component at a flat index.
#[must_use]
pub fn component_name(flat: usize) -> &'static str {
    let mut remaining = flat;
    for (_, _, items) in CATEGORIES {
        if remaining < items.len() {
            return items[remaining].name;
        }
        remaining -= items.len();
    }
    ""
}

/// Flat index of the component with the given name.
#[must_use]
pub fn component_index(name: &str) -> Option<usize> {
    let needle = name.trim().to_ascii_lowercase();
    CATEGORIES
        .iter()
        .flat_map(|(_, _, items)| items.iter())
        .position(|def| def.name.to_ascii_lowercase() == needle)
}

/// Single source of truth for section buttons – used by sidebar, palette and overview.
/// `Default` size, `Ghost`/`Default` variant with icon and selected state.
pub fn section_button(def: &ComponentDef, selected: bool) -> Button<'static> {
    let variant = if selected {
        ButtonVariant::Default
    } else {
        ButtonVariant::Ghost
    };
    Button::new(def.name)
        .icon(def.icon)
        .variant(variant)
        .selected(selected)
}

/// Group header with icon – used by sidebar, palette and overview.
pub fn category_header(ui: &mut egui::Ui, id: CategoryId, lang: Language) {
    let _ = Separator::horizontal()
        .text(id.render(lang))
        .icon(id.icon())
        .show(ui);
}
#[cfg(target_arch = "wasm32")]
fn initial_selected() -> usize {
    let search = web_sys::window()
        .and_then(|window| window.location().search().ok())
        .unwrap_or_default();
    search
        .strip_prefix('?')
        .unwrap_or(&search)
        .split('&')
        .find_map(|pair| {
            let (key, value) = pair.split_once('=')?;
            (key == "component")
                .then(|| component_index(value))
                .flatten()
        })
        .unwrap_or(0)
}

#[cfg(not(target_arch = "wasm32"))]
fn initial_selected() -> usize {
    0
}

#[derive(Default)]
pub struct NavState {
    pub sidebar_demo_collapsed: bool,
}

#[derive(Default)]
pub struct DialogState {
    pub command_open: bool,
    pub dialog_open: bool,
    pub alert_dialog_open: bool,
}

#[derive(Default)]
pub struct DrawerState {
    pub sheet_open: bool,
    pub drawer_open: bool,
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
    pub toolbar_tool_idx: usize,
    pub toolbar_snap: bool,
}

impl Default for ToolbarState {
    fn default() -> Self {
        Self {
            toolbar_tool_idx: 1,
            toolbar_snap: true,
        }
    }
}

pub struct FormState {
    pub form_name: String,
    pub form_card: String,
    pub form_cvv: String,
    pub form_month: Option<String>,
    pub form_year: Option<String>,
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

/// All state for the showcase demos, one field per interactive demo.
pub struct ShowcaseApp {
    pub nav: NavState,
    pub persistent: PersistentState<()>,
    pub sidebar_collapsed: bool,
    pub sidebar_init_done: bool,
    pub selected: usize,
    pub prev_selected: usize,
    pub router: functora_egui::route::AppRouter<AppRoute, ()>,
    pub dialogs: DialogState,
    pub command_search: String,
    pub toast: ToastState,
    pub drawers: DrawerState,
    // inputs
    pub checks: CheckState,
    pub radios: RadioState,
    pub radio_group_val: String,
    pub text_style: TextStyleState,
    pub toggle_group_idx: usize,
    pub slider_val: f64,
    pub slider_price: f64,
    pub input_text: String,
    pub number_f64: f64,
    pub number_f32: f32,
    pub number_i32: i32,
    pub input_group_text: String,
    pub input_paste_clear_text: String,
    pub input_paste_clear_password: String,
    pub input_paste_clear_custom_default: String,
    pub input_paste_clear_custom_icons: String,
    pub textarea_text: String,
    pub textarea_paste_clear_text: String,
    pub textarea_paste_clear_status: String,
    pub textarea_paste_clear_custom: String,
    pub select_val: Option<String>,
    pub select_blend: String,
    pub combobox_selected: Option<usize>,
    pub combobox_search: String,
    pub otp_value: String,
    pub date_picker: functora_egui::DatePickerState,
    pub color_swatch_idx: usize,
    // layout
    pub accordion_open: Vec<usize>,
    pub tabs_idx: usize,
    pub icon_tabs_idx: usize,
    pub pagination_page: usize,
    pub resizable_fraction: f32,
    pub flex_input: String,
    pub flex_first: String,
    pub flex_last: String,
    pub flex_email: String,
    pub flex_phone: String,
    pub toolbar: ToolbarState,
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
}

impl Default for ShowcaseApp {
    fn default() -> Self {
        Self {
            nav: NavState::default(),
            persistent: PersistentState::default(),
            sidebar_collapsed: true,
            sidebar_init_done: false,
            selected: 0,
            prev_selected: 0,
            router: functora_egui::route::AppRouter::new(&mut (), AppRoute::default()),
            dialogs: DialogState::default(),
            command_search: String::new(),
            toast: ToastState::new(),
            drawers: DrawerState::default(),
            checks: CheckState::default(),
            radios: RadioState::default(),
            radio_group_val: "Option A".to_owned(),
            text_style: TextStyleState::default(),
            toggle_group_idx: 0,
            slider_val: 50.0,
            slider_price: 200.0,
            input_text: String::new(),
            number_f64: 42.0,
            number_f32: std::f32::consts::PI,
            number_i32: 10,
            input_group_text: String::new(),
            input_paste_clear_text: String::new(),
            input_paste_clear_password: String::new(),
            input_paste_clear_custom_default: "default value".to_owned(),
            input_paste_clear_custom_icons: String::new(),
            textarea_text: String::new(),
            textarea_paste_clear_text: String::new(),
            textarea_paste_clear_status: String::new(),
            textarea_paste_clear_custom: String::new(),
            select_val: None,
            select_blend: "Normal".to_owned(),
            combobox_selected: None,
            combobox_search: String::new(),
            otp_value: String::new(),
            date_picker: functora_egui::DatePickerState::default(),
            color_swatch_idx: 0,
            accordion_open: vec![0],
            tabs_idx: 0,
            icon_tabs_idx: 0,
            pagination_page: 0,
            resizable_fraction: 0.5,
            flex_input: String::new(),
            flex_first: String::new(),
            flex_last: String::new(),
            flex_email: String::new(),
            flex_phone: String::new(),
            toolbar: ToolbarState::default(),
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
        }
    }
}

impl ShowcaseApp {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        functora_egui::setup_fonts(&cc.egui_ctx);
        let persistent =
            PersistentState::load_or_default(&cc.egui_ctx, "functora_egui_demo_persistent", ());
        functora_egui::theme_extra::set_theme(&cc.egui_ctx, persistent.theme);
        let startup_width = {
            #[cfg(target_arch = "wasm32")]
            {
                functora_egui::web::startup::startup_width(cc)
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                functora_egui::web::startup::startup_width(&cc.egui_ctx)
            }
        };
        let initial_collapsed = if startup_width == 0.0 {
            true
        } else {
            startup_width < functora_egui::Breakpoint::MOBILE_MAX_WIDTH
        };
        let mut this = Self {
            persistent,
            sidebar_collapsed: initial_collapsed,
            ..Default::default()
        };
        this.sidebar_init_done = false;
        this.apply_theme(&cc.egui_ctx);
        #[cfg(target_arch = "wasm32")]
        {
            let mut tmp = ();
            let router = functora_egui::route::AppRouter::new(&mut tmp, AppRoute::default());
            let current = router.current().clone();
            let initial_flat = current.to_flat().unwrap_or_else(initial_selected);
            this.router = router;
            this.selected = initial_flat;
            this.prev_selected = this.selected;
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            this.selected = initial_selected();
            this.prev_selected = this.selected;
            this.router =
                functora_egui::route::AppRouter::new(&mut (), AppRoute::from_flat(this.selected));
        }
        this
    }

    fn apply_theme(&self, ctx: &egui::Context) {
        functora_egui::theme_extra::set_theme(ctx, self.persistent.theme);
    }

    fn reset_to_home(&mut self, ctx: &egui::Context) {
        let prev_persistent = self.persistent.clone();
        *self = Self::default();
        self.persistent = PersistentState::with_system_defaults(ctx, ());
        if self.persistent != prev_persistent {
            persist_value("functora_egui_demo_persistent", &self.persistent);
        }
        self.sidebar_collapsed = ctx.on_mobile();
        self.sidebar_init_done = true;
        self.prev_selected = usize::MAX;
        self.router.reset(&mut (), AppRoute::default());
        self.selected = 0;
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

    pub(crate) fn navigate_to(&mut self, idx: usize) {
        let route = match idx {
            0 => AppRoute::Overview,
            _ => AppRoute::Component(idx),
        };
        self.selected = idx;
        self.router.navigate(&mut (), route);
    }

    fn sync_from_router(&mut self) {
        if let Some(idx) = self.router.current().to_flat()
            && idx != self.selected
        {
            self.selected = idx;
        }
    }

    #[allow(dead_code)]
    fn render_sidebar(&mut self, ui: &mut egui::Ui) -> bool {
        let mut close = false;
        let lang = self.persistent.language;
        for (cat_idx, (cat_id, _, items)) in CATEGORIES.iter().enumerate() {
            let is_overview = *cat_id == CategoryId::Overview;
            if is_overview {
                ui.add_space(8.0);
            } else {
                category_header(ui, *cat_id, lang);
                ui.add_space(8.0);
            }
            for (item_idx, def) in items.iter().enumerate() {
                let flat = flat_index(cat_idx, item_idx);
                let selected = flat == self.selected;
                if ui.add(section_button(def, selected).full_width()).clicked() {
                    self.navigate_to(flat);
                    close |= ui.on_mobile();
                    ui.ctx().request_repaint();
                }
            }
            ui.add_space(8.0);
        }
        close
    }

    fn render_overlays(&mut self, ctx: &egui::Context) {
        if self.dialogs.dialog_open {
            let mut close = false;
            Dialog::new()
                .title("Edit Profile")
                .description("Make changes to your profile here.")
                .show(ctx, &mut self.dialogs.dialog_open, |ui| {
                    _ = Label::new("Full name").show(ui);
                    _ = ui.add(
                        functora_egui::Input::new(&mut self.form.form_name)
                            .placeholder("Ada Lovelace"),
                    );
                    ui.add_space(8.0);
                    _ = Label::new("Bio").show(ui);
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
                        .response
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

        if self.drawers.sheet_open {
            Sheet::new()
                .title("Sheet Panel")
                .description("A side sheet that slides in from the edge.")
                .side(functora_egui::SheetSide::Right)
                .show(ctx, &mut self.drawers.sheet_open, |ui| {
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

        if self.drawers.drawer_open {
            Drawer::new()
                .title("Drawer")
                .description("A bottom drawer panel.")
                .show(ctx, &mut self.drawers.drawer_open, |ui| {
                    FieldDescription::show(
                        ui,
                        "On mobile viewports drawers slide up from the bottom edge.",
                    );
                    ui.add_space(8.0);
                    _ = Flex::row().justify_end().gap(8.0).show(ui, |f| {
                        _ = f.add(
                            Button::new("Close")
                                .variant(ButtonVariant::Outline)
                                .size(functora_egui::ComponentSize::Sm),
                        );
                    });
                });
        }

        if self.dialogs.command_open {
            let lang = self.persistent.language;
            let placeholder = SearchCommandPlaceholder.render(lang);
            let items: Vec<CommandItem> = CATEGORIES
                .iter()
                .flat_map(|(cat, _, defs)| {
                    defs.iter().map(|def| CommandItem {
                        group: (*cat).render(lang),
                        group_icon: cat.icon(),
                        label: def.name.to_owned(),
                        icon: def.icon,
                    })
                })
                .collect();
            if let Some(idx) = Command::with_items(items).placeholder(placeholder).show(
                ctx,
                &mut self.dialogs.command_open,
                &mut self.command_search,
            ) {
                self.navigate_to(idx);
                ctx.request_repaint();
            }
        }
    }

    #[allow(dead_code)]
    fn category_of(flat: usize) -> Option<(CategoryId, usize)> {
        let mut remaining = flat;
        for (cat_idx, (cat_id, _, items)) in CATEGORIES.iter().enumerate() {
            if remaining < items.len() {
                return Some((*cat_id, cat_idx));
            }
            remaining -= items.len();
        }
        None
    }

    fn render_component(&mut self, ui: &mut egui::Ui, lang: Language) {
        let name = component_name(self.selected);
        _ = Typography::h3(name).show(ui);
        ui.add_space(4.0);
        match name {
            "Overview" => self.demo_overview(ui, lang),
            "Button" => self.demo_button(ui),
            "Checkbox" => self.demo_checkbox(ui),
            "Switch" => self.demo_switch(ui),
            "Radio" => self.demo_radio(ui),
            "RadioGroup" => self.demo_radio_group(ui),
            "Toggle" => self.demo_toggle(ui),
            "ToggleGroup" => self.demo_toggle_group(ui),
            "Slider" => self.demo_slider(ui),
            "Input" => self.demo_input(ui),
            "NumberInput" => self.demo_number_input(ui),
            "InputGroup" => self.demo_input_group(ui),
            "InputPasteClear" => self.demo_input_paste_clear(ui),
            "TextareaPasteClear" => self.demo_textarea_paste_clear(ui),
            "Textarea" => self.demo_textarea(ui),
            "Select" => self.demo_select(ui),
            "SelectValue" => self.demo_select_value(ui),
            "Combobox" => self.demo_combobox(ui),
            "InputOtp" => self.demo_input_otp(ui),
            "DatePicker" => self.demo_date_picker(ui),
            "ColorSwatch" => self.demo_color_swatch(ui),
            "Flex" => self.demo_flex(ui),
            "AspectRatio" => Self::demo_aspect_ratio(ui),
            "Card" => Self::demo_card(ui),
            "Collapsible" => self.demo_collapsible(ui),
            "Resizable" => self.demo_resizable(ui),
            "ScrollArea" => Self::demo_scroll_area(ui),
            "Separator" => Self::demo_separator(ui),
            "StatusBar" => Self::demo_status_bar(ui),
            "Tabs" => self.demo_tabs(ui),
            "IconTabs" => self.demo_icon_tabs(ui),
            "Toolbar" => self.demo_toolbar(ui),
            "Accordion" => self.demo_accordion(ui),
            "Dialog" => self.demo_dialog(ui),
            "AlertDialog" => self.demo_alert_dialog(ui),
            "Sheet" => self.demo_sheet(ui),
            "Drawer" => self.demo_drawer(ui),
            "Popover" => Self::demo_popover(ui),
            "HoverCard" => Self::demo_hover_card(ui),
            "Tooltip" => Self::demo_tooltip(ui),
            "ContextMenu" => self.demo_context_menu(ui),
            "DropdownMenu" => self.demo_dropdown_menu(ui),
            "Command" => self.demo_command(ui),
            "Menubar" => self.demo_menubar(ui),
            "NavigationMenu" => self.demo_navigation_menu(ui),
            "Alert" => Self::demo_alert(ui),
            "Badge" => Self::demo_badge(ui),
            "Progress" => self.demo_progress(ui),
            "Skeleton" => Self::demo_skeleton(ui),
            "Spinner" => Self::demo_spinner(ui),
            "Toast" => self.demo_toast(ui),
            "Empty" => Self::demo_empty(ui),
            "Avatar" => Self::demo_avatar(ui),
            "Breadcrumb" => self.demo_breadcrumb(ui),
            "Calendar" => self.demo_calendar(ui),
            "Carousel" => self.demo_carousel(ui),
            "Pagination" => self.demo_pagination(ui),
            "Sidebar" => self.demo_sidebar(ui),
            "Table" => Self::demo_table(ui),
            "AreaChart" => Self::demo_area_chart(ui),
            "Typography" => Self::demo_typography(ui),
            "Label" => self.demo_label(ui),
            "Kbd" => Self::demo_kbd(ui),
            "Item" => self.demo_item(ui),
            "Icons" => self.demo_icons(ui),
            "FieldGroup" => self.demo_field_group(ui),
            "FieldSet" => self.demo_field_set(ui),
            "FieldLegend" => Self::demo_field_legend(ui),
            "FieldDescription" => self.demo_field_description(ui),
            "PropertyGrid" => self.demo_property_grid(ui),
            "PropertyRow" => self.demo_property_row(ui),
            "Breakpoint" => Self::demo_breakpoint(ui),
            "Spacing" => Self::demo_spacing(ui),
            "FlexWrap" => Self::demo_flex_wrap(ui),
            "TouchTarget" => self.demo_touch_target(ui),
            "MobileDialog" => self.demo_mobile_dialog(ui),
            "MobileSidebar" => self.demo_mobile_sidebar(ui),
            "Storage" => self.demo_storage(ui),
            "Clipboard" => self.demo_clipboard(ui),
            "Share" => self.demo_share(ui),
            "DeepLink" => self.demo_deep_link(ui),
            "Files" => self.demo_files(ui),
            "Download" => self.demo_download(ui),
            "Nav" => self.demo_nav(ui),
            "ProgressWorker" => self.demo_progress_worker(ui),
            "PWA" => self.demo_pwa(ui),
            "Encoding" => self.demo_encoding(ui),
            "InFlight" => self.demo_in_flight(ui),
            "Camera" => self.demo_camera(ui),
            "QrScanner" => self.demo_qr_scanner(ui),
            "Thumbnail" => self.demo_thumbnail(ui),
            "Zip" => self.demo_zip(ui),
            "Crypto" => self.demo_crypto(ui),
            "Worker" => self.demo_worker(ui),
            "PlatformInfo" => self.demo_platform_info(ui),
            "Messages" => Self::demo_messages(ui),
            "Markdown" => Self::demo_markdown(ui),
            "Package" => Self::demo_package(ui),
            "WhiteLabel" => Self::demo_white_label(ui),
            _ => {
                _ = Typography::muted("No demo available.").show(ui);
            }
        }
    }
}

impl eframe::App for ShowcaseApp {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        let ctx = ui.ctx().clone();
        if !self.sidebar_init_done {
            let width = ctx.input(|i| i.viewport_rect().width());
            if width != 0.0 {
                self.sidebar_collapsed = ctx.on_mobile();
                self.sidebar_init_done = true;
            }
        }
        self.apply_theme(&ctx);
        self.handle_shortcuts(&ctx);
        self.poll_platform_promises(&ctx);
        self.router.ui(ui, &mut ());
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
        let selected_ptr: *mut usize = &raw mut self.selected;
        let router_ptr: *mut functora_egui::route::AppRouter<AppRoute, ()> = &raw mut self.router;
        let lang_cell = std::cell::Cell::new(persistent.language);
        let breadcrumb_action = Shell::new("functora-egui", &mut collapsed_val, {
            let lang_ref = &lang_cell;
            move |side_ui| {
                let cur_lang = lang_ref.get();
                let selected_ref = unsafe { &mut *selected_ptr };
                let router_ref = unsafe { &mut *router_ptr };
                let mut close = false;
                for (cat_idx, (cat_id, _, items)) in CATEGORIES.iter().enumerate() {
                    let is_overview = *cat_id == CategoryId::Overview;
                    if is_overview {
                        side_ui.add_space(8.0);
                    } else {
                        category_header(side_ui, *cat_id, cur_lang);
                        side_ui.add_space(8.0);
                    }
                    for (item_idx, def) in items.iter().enumerate() {
                        let flat = flat_index(cat_idx, item_idx);
                        let is_selected = flat == *selected_ref;
                        if side_ui
                            .add(section_button(def, is_selected).full_width())
                            .clicked()
                        {
                            let next_route = match flat {
                                0 => AppRoute::Overview,
                                _ => AppRoute::Component(flat),
                            };
                            *selected_ref = flat;
                            router_ref.navigate(&mut (), next_route);
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
                let cur_lang = lang_ref.get();
                let suffix = FooterSuffix.render(cur_lang);
                let _ = Footer::new().show(footer_ui, |inner| {
                    let _ = Hypertext::new()
                        .text(format!("© {} ", functora_egui::FUNCTORA_CORE_YEAR))
                        .link("Functora", "https://functora.github.io/")
                        .text(suffix)
                        .centered()
                        .show(inner);
                });
            }
        })
        .show(ui, |content_ui| {
            let cur_lang = lang_cell.get();
            self.render_component(content_ui, cur_lang);
        });
        persistent.language = lang_cell.get();
        self.sidebar_collapsed = collapsed_val;
        self.persistent = persistent;
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
                    let _ = self.router.go_back(&mut ());
                    ctx.request_repaint();
                }
                functora_egui::NavAction::Forward => {
                    let _ = self.router.go_forward(&mut ());
                    ctx.request_repaint();
                }
                functora_egui::NavAction::Route(nav_route) => {
                    if let Some(idx) = nav_route.to_flat() {
                        self.navigate_to(idx);
                    }
                    ctx.request_repaint();
                }
            }
            self.sync_from_router();
        }
        self.render_overlays(&ctx);
        self.toast.show(&ctx);
    }
}
