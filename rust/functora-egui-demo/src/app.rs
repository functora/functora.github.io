//! Showcase app: an interactive catalog of every functora-egui widget,
//! layout, and feature, with light/dark theming and responsive behavior.

use functora_egui::theme::shadcn_theme_dark::dark;
use functora_egui::theme::shadcn_theme_light::light;
use functora_egui::{
    AlertDialog, AlertDialogResult, Button, ButtonVariant, Command, Dialog, Drawer,
    FieldDescription, Flex, Item, Label, LucideIcon, ResponsiveExt, ShadcnThemeExt, Sheet, Sidebar,
    ToastState, ToastVariant, Typography, TypographyVariant,
};

pub use functora_egui::PickResult;
pub type PickReceiver = std::sync::mpsc::Receiver<PickResult>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum DemoRoute {
    #[default]
    Home,
    Profile,
    Settings,
    About,
}

impl std::fmt::Display for DemoRoute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Home => write!(f, "Home"),
            Self::Profile => write!(f, "Profile"),
            Self::Settings => write!(f, "Settings"),
            Self::About => write!(f, "About"),
        }
    }
}

impl std::str::FromStr for DemoRoute {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "/" | "/home" | "home" => Ok(Self::Home),
            "/profile" => Ok(Self::Profile),
            "/settings" => Ok(Self::Settings),
            "/about" => Ok(Self::About),
            _ => Err("unknown route".into()),
        }
    }
}

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
    pub print_status: String,
    pub print_rx: Option<std::sync::mpsc::Receiver<Result<(), String>>>,
    pub nav: functora_egui::nav::NavStack<DemoRoute>,
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
            print_status: String::new(),
            print_rx: None,
            nav: functora_egui::nav::NavStack::new(),
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

/// Catalog of every component grouped by category.
pub const CATEGORIES: &[(&str, LucideIcon, &[ComponentDef])] = &[
    (
        "Overview",
        LucideIcon::Sparkles,
        &[ComponentDef::new("Overview", LucideIcon::Sparkles)],
    ),
    (
        "Inputs",
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
        "Layout",
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
        "Overlays",
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
        "Feedback",
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
        "Data",
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
        "Display",
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
        "Forms",
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
        "Responsive",
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
        "Platform",
        LucideIcon::Smartphone,
        &[
            ComponentDef::new("Storage", LucideIcon::Database),
            ComponentDef::new("Clipboard", LucideIcon::Clipboard),
            ComponentDef::new("Share", LucideIcon::Share2),
            ComponentDef::new("DeepLink", LucideIcon::Link),
            ComponentDef::new("Files", LucideIcon::Files),
            ComponentDef::new("Download", LucideIcon::Download),
            ComponentDef::new("Print", LucideIcon::Printer),
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

/// The selected component read from the `?component=` query on the web.
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

pub struct NavState {
    pub dark: bool,
    pub sidebar_collapsed: bool,
    pub sidebar_demo_collapsed: bool,
}

impl Default for NavState {
    fn default() -> Self {
        Self {
            dark: true,
            sidebar_collapsed: true,
            sidebar_demo_collapsed: false,
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
    pub sidebar_init_done: bool,
    pub selected: usize,
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
    pub textarea_text: String,
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
            sidebar_init_done: false,
            selected: 0,
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
            textarea_text: String::new(),
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
        let theme = dark();
        ShadcnThemeExt::set_shadcn_theme(&cc.egui_ctx, theme);
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
        let mut this = Self::default();
        this.nav.sidebar_collapsed = initial_collapsed;
        this.sidebar_init_done = false;
        this.selected = initial_selected();
        this
    }

    fn toggle_theme(&mut self, ctx: &egui::Context) {
        self.nav.dark = !self.nav.dark;
        self.apply_theme(ctx);
    }

    fn apply_theme(&self, ctx: &egui::Context) {
        let theme = if self.nav.dark { dark() } else { light() };
        ShadcnThemeExt::set_shadcn_theme(ctx, theme);
    }

    fn reset_to_home(&mut self, ctx: &egui::Context) {
        *self = Self::default();
        self.nav.sidebar_collapsed = ctx.on_mobile();
        self.sidebar_init_done = true;
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

    fn sidebar_effective_width(ctx: &egui::Context) -> f32 {
        let spacing = ctx.responsive_spacing();
        let max_text = CATEGORIES
            .iter()
            .flat_map(|(_, _, items)| items.iter())
            .map(|def| {
                let font_id = egui::FontId::proportional(14.0);
                ctx.fonts_mut(|fonts| {
                    fonts
                        .layout_no_wrap(def.name.to_owned(), font_id, egui::Color32::WHITE)
                        .rect
                        .width()
                })
            })
            .fold(0.0, f32::max);
        let icon = spacing.touch_height * 0.5;
        max_text + icon + spacing.gap + spacing.touch_padding * 2.0 + spacing.gap
    }

    fn render_top_bar(&mut self, ui: &mut egui::Ui) {
        let pad_x: i8 = 8;
        let _panel = egui::Frame::NONE
            .inner_margin(egui::Margin {
                left: pad_x,
                right: pad_x,
                top: 6,
                bottom: 6,
            })
            .show(ui, |ui2| {
                _ = Flex::row()
                    .gap(4.0)
                    .justify_between()
                    .align_center()
                    .w_full()
                    .show(ui2, |f| {
                        _ = f.ui(|ui_left| {
                            _ = ui_left.horizontal(|ui_inner| {
                                let ctx = ui_inner.ctx().clone();
                                let theme = ShadcnThemeExt::shadcn_theme(&ctx);
                                let resp = ui_inner
                                    .add(
                                        egui::Label::new(
                                            egui::RichText::new("functora-egui")
                                                .size(20.0)
                                                .strong()
                                                .color(theme.foreground),
                                        )
                                        .selectable(false)
                                        .sense(egui::Sense::click()),
                                    )
                                    .on_hover_cursor(egui::CursorIcon::PointingHand);
                                if resp.clicked() {
                                    self.reset_to_home(&ctx);
                                }
                            });
                        });
                        _ = f.ui(|ui_right| {
                            _ = ui_right.horizontal(|ui_inner| {
                                let search = if ui_inner.on_mobile() {
                                    Button::icon_only(LucideIcon::Search)
                                        .variant(ButtonVariant::Outline)
                                        .size(functora_egui::ComponentSize::Sm)
                                } else {
                                    Button::new("Search")
                                        .icon(LucideIcon::Search)
                                        .variant(ButtonVariant::Outline)
                                        .size(functora_egui::ComponentSize::Sm)
                                        .shortcut_text("Ctrl K")
                                };
                                if ui_inner.add(search).clicked() {
                                    self.dialogs.command_open = true;
                                    self.command_search.clear();
                                }
                                ui_inner.add_space(4.0);
                                let theme_icon = if self.nav.dark {
                                    LucideIcon::Moon
                                } else {
                                    LucideIcon::Sun
                                };
                                if ui_inner
                                    .add(
                                        Button::icon_only(theme_icon)
                                            .variant(ButtonVariant::Outline)
                                            .size(functora_egui::ComponentSize::Sm),
                                    )
                                    .on_hover_text(if self.nav.dark {
                                        "Light theme"
                                    } else {
                                        "Dark theme"
                                    })
                                    .clicked()
                                {
                                    self.toggle_theme(ui_inner.ctx());
                                }
                                ui_inner.add_space(4.0);
                                _ = Sidebar::toggle_button(
                                    ui_inner,
                                    &mut self.nav.sidebar_collapsed,
                                );
                            });
                        });
                    });
            });
    }

    fn render_sidebar(&mut self, ui: &mut egui::Ui) -> bool {
        let mut close = false;
        for (cat_idx, (cat_name, _cat_icon, items)) in CATEGORIES.iter().enumerate() {
            ui.add_space(6.0);
            _ = Typography::small(*cat_name)
                .variant(TypographyVariant::Muted)
                .show(ui);
            ui.add_space(2.0);
            for (item_idx, def) in items.iter().enumerate() {
                let flat = flat_index(cat_idx, item_idx);
                let selected = flat == self.selected;
                let variant = if selected {
                    ButtonVariant::Default
                } else {
                    ButtonVariant::Ghost
                };
                if ui
                    .add(
                        Button::new(def.name)
                            .icon(def.icon)
                            .variant(variant)
                            .full_width()
                            .selected(selected),
                    )
                    .clicked()
                {
                    self.selected = flat;
                    close |= ui.on_mobile();
                    ui.ctx().request_repaint();
                }
            }
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
            let items: Vec<(String, String)> = CATEGORIES
                .iter()
                .flat_map(|(cat, _, defs)| {
                    defs.iter()
                        .map(|def| ((*cat).to_owned(), def.name.to_owned()))
                })
                .collect();
            if let Some(idx) = Command::new(items)
                .placeholder("Search components...")
                .show(
                    ctx,
                    &mut self.dialogs.command_open,
                    &mut self.command_search,
                )
            {
                self.selected = idx;
                ctx.request_repaint();
            }
        }
    }

    fn render_component(&mut self, ui: &mut egui::Ui) {
        let name = component_name(self.selected);
        _ = Typography::h3(name).show(ui);
        ui.add_space(4.0);
        match name {
            "Overview" => self.demo_overview(ui),
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
            "Print" => self.demo_print(ui),
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
                self.nav.sidebar_collapsed = ctx.on_mobile();
                self.sidebar_init_done = true;
            }
        }
        self.apply_theme(&ctx);
        self.handle_shortcuts(&ctx);
        self.poll_platform_promises(&ctx);
        #[cfg(target_os = "android")]
        crate::android::poll_ime(&ctx);

        let theme = ShadcnThemeExt::shadcn_theme(&ctx);
        let top = egui::Panel::top("top_bar")
            .frame(egui::Frame::NONE.fill(theme.card))
            .show_separator_line(false)
            .show(ui, |ui7| {
                self.render_top_bar(ui7);
            });
        _ = ui.painter().hline(
            top.response.rect.x_range(),
            top.response.rect.max.y - 0.5,
            egui::Stroke::new(1.0, theme.border),
        );

        if !ctx.on_mobile() {
            let is_rail = self.nav.sidebar_collapsed;
            let spacing = ctx.responsive_spacing();
            let screen_width = ctx.input(|i| i.viewport_rect().width());
            let max_allowed_outer = (screen_width - spacing.page_padding * 2.0).max(0.0);
            let effective = if is_rail {
                spacing.touch_height
            } else {
                Self::sidebar_effective_width(&ctx).min((max_allowed_outer - 16.0).max(0.0))
            };
            let panel_outer = effective + 16.0;
            let panel_fill = if is_rail {
                theme.background
            } else {
                theme.card
            };
            _ = egui::Panel::right("sidebar_panel")
                .exact_size(panel_outer)
                .frame(egui::Frame::NONE.fill(panel_fill))
                .resizable(false)
                .show_separator_line(false)
                .show(ui, |ui8| {
                    let close = std::cell::Cell::new(false);
                    let mut collapsed = self.nav.sidebar_collapsed;
                    _ = Sidebar::new().width(effective).collapsible().show(
                        ui8,
                        &mut collapsed,
                        |ui9| {
                            close.set(self.render_sidebar(ui9));
                        },
                    );
                    if close.get() {
                        collapsed = true;
                    }
                    self.nav.sidebar_collapsed = collapsed;
                });
        }

        _ = egui::CentralPanel::default()
            .frame(egui::Frame::NONE.fill(theme.background))
            .show(ui, |ui10| {
                if ui10.on_mobile() {
                    let spacing = ui10.responsive_spacing();
                    let screen_width = ui10.ctx().input(|i| i.viewport_rect().width());
                    let max_allowed_outer = (screen_width - spacing.page_padding * 2.0).max(0.0);
                    let effective = Self::sidebar_effective_width(ui10.ctx())
                        .min((max_allowed_outer - 16.0).max(0.0));
                    let close = std::cell::Cell::new(false);
                    let mut collapsed = self.nav.sidebar_collapsed;
                    _ = Sidebar::new().width(effective).collapsible().show(
                        ui10,
                        &mut collapsed,
                        |ui11| {
                            close.set(self.render_sidebar(ui11));
                        },
                    );
                    if close.get() {
                        collapsed = true;
                    }
                    self.nav.sidebar_collapsed = collapsed;
                }
                let spacing = ui10.responsive_spacing();
                let available = ui10.available_width();
                let content_width = available.min(spacing.content_max_width);
                let margin = ((available - content_width) * 0.5).max(0.0);
                let inner_width = (content_width - 2.0 * spacing.page_padding).max(0.0);
                _ = egui::ScrollArea::vertical()
                    .auto_shrink([false; 2])
                    .show(ui10, |ui12| {
                        ui12.add_space(spacing.page_padding);
                        _ = ui12.horizontal(|ui13| {
                            ui13.add_space(margin);
                            ui13.add_space(spacing.page_padding);
                            _ = ui13.vertical(|ui14| {
                                ui14.set_max_width(inner_width);
                                self.render_component(ui14);
                                ui14.add_space(48.0);
                            });
                            ui13.add_space(spacing.page_padding);
                            ui13.add_space(margin);
                        });
                    });
            });

        self.render_overlays(&ctx);
        self.toast.show(&ctx);
    }
}
