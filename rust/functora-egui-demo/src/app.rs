//! Showcase app: an interactive catalog of every functora-egui widget,
//! layout, and feature, with light/dark theming and responsive behavior.

use functora_egui::i18n::{I18N, Language};
use functora_egui::state::PersistentState;
use functora_egui::storage::persist_value;
use functora_egui::{
    AlertDialog, AlertDialogResult, Button, ButtonVariant, CommandItem, CommandValue, Dialog,
    FieldDescription, Flex, Footer, Hypertext, Item, Label, LucideIcon, ResponsiveExt, Separator,
    Sheet, Shell, ToastState, ToastVariant, Typography,
};

pub(crate) type PickReceiver =
    std::sync::mpsc::Receiver<Result<Vec<(String, Vec<u8>)>, functora_egui::error::Error>>;
/// Ready thumbnail from `make_thumbnail`: image URI plus jpeg bytes.
pub type ThumbnailResult = Result<(String, Vec<u8>), String>;

use crate::route::AppRoute;

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

/// A single showcase entry: one component or feature with a nav icon.
pub struct ComponentDef {
    pub name: &'static str,
    pub icon: LucideIcon,
    pub id: Option<ComponentId>,
}

impl ComponentDef {
    const fn new(name: &'static str, icon: LucideIcon, id: Option<ComponentId>) -> Self {
        Self { name, icon, id }
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

/// Selection values for the typed showcase demos: widgets bind these
/// enum variants directly instead of blind `usize` indexes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Align {
    #[default]
    Left,
    Center,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Framework {
    #[default]
    React,
    Vue,
    Angular,
    Svelte,
    Solid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SettingsTab {
    #[default]
    Account,
    Password,
    Settings,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ProfileTab {
    #[default]
    Home,
    Settings,
    Profile,
    Notifications,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum NavSection {
    #[default]
    Overview,
    Integrations,
    Settings,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RadioOption {
    #[default]
    A,
    B,
    C,
}

impl RadioOption {
    pub const ALL: [(Self, &'static str); 3] = [
        (Self::A, "Option A"),
        (Self::B, "Option B"),
        (Self::C, "Option C"),
    ];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Fruit {
    #[default]
    Apple,
    Banana,
    Cherry,
    Grape,
    Mango,
}

impl Fruit {
    pub const ALL: [(Self, &'static str); 5] = [
        (Self::Apple, "Apple"),
        (Self::Banana, "Banana"),
        (Self::Cherry, "Cherry"),
        (Self::Grape, "Grape"),
        (Self::Mango, "Mango"),
    ];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BlendMode {
    #[default]
    Normal,
    Multiply,
    Screen,
    Overlay,
}

impl BlendMode {
    pub const ALL: [(Self, &'static str); 4] = [
        (Self::Normal, "Normal"),
        (Self::Multiply, "Multiply"),
        (Self::Screen, "Screen"),
        (Self::Overlay, "Overlay"),
    ];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Month(u8);

impl Month {
    pub const ALL: [Self; 12] = [
        Self(1),
        Self(2),
        Self(3),
        Self(4),
        Self(5),
        Self(6),
        Self(7),
        Self(8),
        Self(9),
        Self(10),
        Self(11),
        Self(12),
    ];

    #[must_use]
    pub fn label(self) -> String {
        format!("{:02}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Year(i32);

impl Year {
    pub const ALL: [Self; 5] = [Self(2026), Self(2027), Self(2028), Self(2029), Self(2030)];

    #[must_use]
    pub fn label(self) -> String {
        self.0.to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tool {
    Select,
    Pen,
    Spline,
    Frame,
    Text,
}

impl Tool {
    pub const ALL: [(Self, LucideIcon); 5] = [
        (Self::Select, LucideIcon::MousePointer2),
        (Self::Pen, LucideIcon::PenTool),
        (Self::Spline, LucideIcon::Spline),
        (Self::Frame, LucideIcon::Frame),
        (Self::Text, LucideIcon::Type),
    ];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Swatch {
    #[default]
    Signal,
    Mint,
    Amber,
    Rose,
    Ink,
}

impl Swatch {
    pub const ALL: [(Self, &'static str, egui::Color32); 5] = [
        (
            Self::Signal,
            "Signal",
            egui::Color32::from_rgb(25, 113, 194),
        ),
        (Self::Mint, "Mint", egui::Color32::from_rgb(18, 184, 134)),
        (Self::Amber, "Amber", egui::Color32::from_rgb(245, 159, 0)),
        (Self::Rose, "Rose", egui::Color32::from_rgb(224, 49, 49)),
        (Self::Ink, "Ink", egui::Color32::from_rgb(33, 37, 41)),
    ];
}

/// Identity of a showcase component. Widgets bind these variants directly
/// instead of blind `usize` indexes or matched name strings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComponentId {
    Button,
    Checkbox,
    Switch,
    Radio,
    RadioGroup,
    Toggle,
    ToggleGroup,
    Slider,
    Input,
    NumberInput,
    InputGroup,
    InputPasteClear,
    TextareaPasteClear,
    Textarea,
    Select,
    SelectValue,
    Combobox,
    InputOtp,
    DatePicker,
    ColorSwatch,
    Flex,
    AspectRatio,
    Card,
    Collapsible,
    Resizable,
    ScrollArea,
    Separator,
    StatusBar,
    Tabs,
    IconTabs,
    Toolbar,
    Accordion,
    Dialog,
    AlertDialog,
    Sheet,
    Popover,
    HoverCard,
    Tooltip,
    ContextMenu,
    DropdownMenu,
    Command,
    Menubar,
    NavigationMenu,
    Alert,
    Badge,
    Progress,
    Skeleton,
    Spinner,
    Toast,
    Empty,
    Avatar,
    Breadcrumb,
    Calendar,
    Carousel,
    Pagination,
    Sidebar,
    Table,
    AreaChart,
    Typography,
    Label,
    Kbd,
    Item,
    Icons,
    Image,
    FieldGroup,
    FieldSet,
    FieldLegend,
    FieldDescription,
    PropertyGrid,
    PropertyRow,
    Breakpoint,
    Spacing,
    FlexWrap,
    TouchTarget,
    Storage,
    Clipboard,
    Share,
    DeepLink,
    Files,
    Download,
    Nav,
    ProgressWorker,
    Pwa,
    Encoding,
    InFlight,
    Camera,
    QrScanner,
    QrImage,
    Thumbnail,
    Zip,
    Crypto,
    Worker,
    PlatformInfo,
    Messages,
    Markdown,
    Package,
    WhiteLabel,
}

impl ComponentId {
    pub const ALL: [Self; 97] = [
        Self::Button,
        Self::Checkbox,
        Self::Switch,
        Self::Radio,
        Self::RadioGroup,
        Self::Toggle,
        Self::ToggleGroup,
        Self::Slider,
        Self::Input,
        Self::NumberInput,
        Self::InputGroup,
        Self::InputPasteClear,
        Self::TextareaPasteClear,
        Self::Textarea,
        Self::Select,
        Self::SelectValue,
        Self::Combobox,
        Self::InputOtp,
        Self::DatePicker,
        Self::ColorSwatch,
        Self::Flex,
        Self::AspectRatio,
        Self::Card,
        Self::Collapsible,
        Self::Resizable,
        Self::ScrollArea,
        Self::Separator,
        Self::StatusBar,
        Self::Tabs,
        Self::IconTabs,
        Self::Toolbar,
        Self::Accordion,
        Self::Dialog,
        Self::AlertDialog,
        Self::Sheet,
        Self::Popover,
        Self::HoverCard,
        Self::Tooltip,
        Self::ContextMenu,
        Self::DropdownMenu,
        Self::Command,
        Self::Menubar,
        Self::NavigationMenu,
        Self::Alert,
        Self::Badge,
        Self::Progress,
        Self::Skeleton,
        Self::Spinner,
        Self::Toast,
        Self::Empty,
        Self::Avatar,
        Self::Breadcrumb,
        Self::Calendar,
        Self::Carousel,
        Self::Pagination,
        Self::Sidebar,
        Self::Table,
        Self::AreaChart,
        Self::Typography,
        Self::Label,
        Self::Kbd,
        Self::Item,
        Self::Icons,
        Self::Image,
        Self::FieldGroup,
        Self::FieldSet,
        Self::FieldLegend,
        Self::FieldDescription,
        Self::PropertyGrid,
        Self::PropertyRow,
        Self::Breakpoint,
        Self::Spacing,
        Self::FlexWrap,
        Self::TouchTarget,
        Self::Storage,
        Self::Clipboard,
        Self::Share,
        Self::DeepLink,
        Self::Files,
        Self::Download,
        Self::Nav,
        Self::ProgressWorker,
        Self::Pwa,
        Self::Encoding,
        Self::InFlight,
        Self::Camera,
        Self::QrScanner,
        Self::QrImage,
        Self::Thumbnail,
        Self::Zip,
        Self::Crypto,
        Self::Worker,
        Self::PlatformInfo,
        Self::Messages,
        Self::Markdown,
        Self::Package,
        Self::WhiteLabel,
    ];

    /// Display name shared with the catalog entry.
    #[must_use]
    pub fn name(self) -> &'static str {
        match self {
            Self::Button => "Button",
            Self::Checkbox => "Checkbox",
            Self::Switch => "Switch",
            Self::Radio => "Radio",
            Self::RadioGroup => "RadioGroup",
            Self::Toggle => "Toggle",
            Self::ToggleGroup => "ToggleGroup",
            Self::Slider => "Slider",
            Self::Input => "Input",
            Self::NumberInput => "NumberInput",
            Self::InputGroup => "InputGroup",
            Self::InputPasteClear => "InputPasteClear",
            Self::TextareaPasteClear => "TextareaPasteClear",
            Self::Textarea => "Textarea",
            Self::Select => "Select",
            Self::SelectValue => "SelectValue",
            Self::Combobox => "Combobox",
            Self::InputOtp => "InputOtp",
            Self::DatePicker => "DatePicker",
            Self::ColorSwatch => "ColorSwatch",
            Self::Flex => "Flex",
            Self::AspectRatio => "AspectRatio",
            Self::Card => "Card",
            Self::Collapsible => "Collapsible",
            Self::Resizable => "Resizable",
            Self::ScrollArea => "ScrollArea",
            Self::Separator => "Separator",
            Self::StatusBar => "StatusBar",
            Self::Tabs => "Tabs",
            Self::IconTabs => "IconTabs",
            Self::Toolbar => "Toolbar",
            Self::Accordion => "Accordion",
            Self::Dialog => "Dialog",
            Self::AlertDialog => "AlertDialog",
            Self::Sheet => "Sheet",
            Self::Popover => "Popover",
            Self::HoverCard => "HoverCard",
            Self::Tooltip => "Tooltip",
            Self::ContextMenu => "ContextMenu",
            Self::DropdownMenu => "DropdownMenu",
            Self::Command => "Command",
            Self::Menubar => "Menubar",
            Self::NavigationMenu => "NavigationMenu",
            Self::Alert => "Alert",
            Self::Badge => "Badge",
            Self::Progress => "Progress",
            Self::Skeleton => "Skeleton",
            Self::Spinner => "Spinner",
            Self::Toast => "Toast",
            Self::Empty => "Empty",
            Self::Avatar => "Avatar",
            Self::Breadcrumb => "Breadcrumb",
            Self::Calendar => "Calendar",
            Self::Carousel => "Carousel",
            Self::Pagination => "Pagination",
            Self::Sidebar => "Sidebar",
            Self::Table => "Table",
            Self::AreaChart => "AreaChart",
            Self::Typography => "Typography",
            Self::Label => "Label",
            Self::Kbd => "Kbd",
            Self::Item => "Item",
            Self::Icons => "Icons",
            Self::Image => "Image",
            Self::FieldGroup => "FieldGroup",
            Self::FieldSet => "FieldSet",
            Self::FieldLegend => "FieldLegend",
            Self::FieldDescription => "FieldDescription",
            Self::PropertyGrid => "PropertyGrid",
            Self::PropertyRow => "PropertyRow",
            Self::Breakpoint => "Breakpoint",
            Self::Spacing => "Spacing",
            Self::FlexWrap => "FlexWrap",
            Self::TouchTarget => "TouchTarget",
            Self::Storage => "Storage",
            Self::Clipboard => "Clipboard",
            Self::Share => "Share",
            Self::DeepLink => "DeepLink",
            Self::Files => "Files",
            Self::Download => "Download",
            Self::Nav => "Nav",
            Self::ProgressWorker => "ProgressWorker",
            Self::Pwa => "PWA",
            Self::Encoding => "Encoding",
            Self::InFlight => "InFlight",
            Self::Camera => "Camera",
            Self::QrScanner => "QrScanner",
            Self::QrImage => "QrImage",
            Self::Thumbnail => "Thumbnail",
            Self::Zip => "Zip",
            Self::Crypto => "Crypto",
            Self::Worker => "Worker",
            Self::PlatformInfo => "PlatformInfo",
            Self::Messages => "Messages",
            Self::Markdown => "Markdown",
            Self::Package => "Package",
            Self::WhiteLabel => "WhiteLabel",
        }
    }

    /// URL slug for routes and deep links.
    #[must_use]
    pub fn slug(self) -> String {
        self.name().to_ascii_lowercase()
    }

    /// Parses a slug back, tolerating surrounding whitespace and case.
    #[must_use]
    pub fn from_slug(slug: &str) -> Option<Self> {
        match slug.trim().to_ascii_lowercase().as_str() {
            "button" => Some(Self::Button),
            "checkbox" => Some(Self::Checkbox),
            "switch" => Some(Self::Switch),
            "radio" => Some(Self::Radio),
            "radiogroup" => Some(Self::RadioGroup),
            "toggle" => Some(Self::Toggle),
            "togglegroup" => Some(Self::ToggleGroup),
            "slider" => Some(Self::Slider),
            "input" => Some(Self::Input),
            "numberinput" => Some(Self::NumberInput),
            "inputgroup" => Some(Self::InputGroup),
            "inputpasteclear" => Some(Self::InputPasteClear),
            "textareapasteclear" => Some(Self::TextareaPasteClear),
            "textarea" => Some(Self::Textarea),
            "select" => Some(Self::Select),
            "selectvalue" => Some(Self::SelectValue),
            "combobox" => Some(Self::Combobox),
            "inputotp" => Some(Self::InputOtp),
            "datepicker" => Some(Self::DatePicker),
            "colorswatch" => Some(Self::ColorSwatch),
            "flex" => Some(Self::Flex),
            "aspectratio" => Some(Self::AspectRatio),
            "card" => Some(Self::Card),
            "collapsible" => Some(Self::Collapsible),
            "resizable" => Some(Self::Resizable),
            "scrollarea" => Some(Self::ScrollArea),
            "separator" => Some(Self::Separator),
            "statusbar" => Some(Self::StatusBar),
            "tabs" => Some(Self::Tabs),
            "icontabs" => Some(Self::IconTabs),
            "toolbar" => Some(Self::Toolbar),
            "accordion" => Some(Self::Accordion),
            "dialog" => Some(Self::Dialog),
            "alertdialog" => Some(Self::AlertDialog),
            "sheet" => Some(Self::Sheet),
            "popover" => Some(Self::Popover),
            "hovercard" => Some(Self::HoverCard),
            "tooltip" => Some(Self::Tooltip),
            "contextmenu" => Some(Self::ContextMenu),
            "dropdownmenu" => Some(Self::DropdownMenu),
            "command" => Some(Self::Command),
            "menubar" => Some(Self::Menubar),
            "navigationmenu" => Some(Self::NavigationMenu),
            "alert" => Some(Self::Alert),
            "badge" => Some(Self::Badge),
            "progress" => Some(Self::Progress),
            "skeleton" => Some(Self::Skeleton),
            "spinner" => Some(Self::Spinner),
            "toast" => Some(Self::Toast),
            "empty" => Some(Self::Empty),
            "avatar" => Some(Self::Avatar),
            "breadcrumb" => Some(Self::Breadcrumb),
            "calendar" => Some(Self::Calendar),
            "carousel" => Some(Self::Carousel),
            "pagination" => Some(Self::Pagination),
            "sidebar" => Some(Self::Sidebar),
            "table" => Some(Self::Table),
            "areachart" => Some(Self::AreaChart),
            "typography" => Some(Self::Typography),
            "label" => Some(Self::Label),
            "kbd" => Some(Self::Kbd),
            "item" => Some(Self::Item),
            "icons" => Some(Self::Icons),
            "image" => Some(Self::Image),
            "fieldgroup" => Some(Self::FieldGroup),
            "fieldset" => Some(Self::FieldSet),
            "fieldlegend" => Some(Self::FieldLegend),
            "fielddescription" => Some(Self::FieldDescription),
            "propertygrid" => Some(Self::PropertyGrid),
            "propertyrow" => Some(Self::PropertyRow),
            "breakpoint" => Some(Self::Breakpoint),
            "spacing" => Some(Self::Spacing),
            "flexwrap" => Some(Self::FlexWrap),
            "touchtarget" => Some(Self::TouchTarget),
            "storage" => Some(Self::Storage),
            "clipboard" => Some(Self::Clipboard),
            "share" => Some(Self::Share),
            "deeplink" => Some(Self::DeepLink),
            "files" => Some(Self::Files),
            "download" => Some(Self::Download),
            "nav" => Some(Self::Nav),
            "progressworker" => Some(Self::ProgressWorker),
            "pwa" => Some(Self::Pwa),
            "encoding" => Some(Self::Encoding),
            "inflight" => Some(Self::InFlight),
            "camera" => Some(Self::Camera),
            "qrscanner" => Some(Self::QrScanner),
            "qrimage" => Some(Self::QrImage),
            "thumbnail" => Some(Self::Thumbnail),
            "zip" => Some(Self::Zip),
            "crypto" => Some(Self::Crypto),
            "worker" => Some(Self::Worker),
            "platforminfo" => Some(Self::PlatformInfo),
            "messages" => Some(Self::Messages),
            "markdown" => Some(Self::Markdown),
            "package" => Some(Self::Package),
            "whitelabel" => Some(Self::WhiteLabel),
            _ => None,
        }
    }
}

pub struct OverviewBody;
impl I18N for OverviewBody {
    fn render_eng(&self) -> String {
        format!(
            "Interactive showcase of {} shadcn/ui-inspired widgets for egui with light/dark themes and 1600+ Lucide icons. Browse via the sidebar or press Ctrl+K.",
            component_count()
        )
    }

    fn render_spa(&self) -> String {
        format!(
            "Presentación interactiva de {} widgets para egui inspirados en shadcn/ui con temas claro/oscuro e iconos Lucide 1600+. Navega por la barra lateral o presiona Ctrl+K.",
            component_count()
        )
    }

    fn render_rus(&self) -> String {
        format!(
            "Интерактивная демонстрация {} виджетов для egui в стиле shadcn/ui с темами светлая/тёмная и 1600+ иконок Lucide. Откройте боковую панель или нажмите Ctrl+K.",
            component_count()
        )
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
        &[ComponentDef::new("Overview", LucideIcon::Sparkles, None)],
    ),
    (
        CategoryId::Inputs,
        LucideIcon::Keyboard,
        &[
            ComponentDef::new(
                "Button",
                LucideIcon::MousePointer2,
                Some(ComponentId::Button),
            ),
            ComponentDef::new(
                "Checkbox",
                LucideIcon::SquareCheckBig,
                Some(ComponentId::Checkbox),
            ),
            ComponentDef::new("Switch", LucideIcon::ToggleRight, Some(ComponentId::Switch)),
            ComponentDef::new("Radio", LucideIcon::CircleDot, Some(ComponentId::Radio)),
            ComponentDef::new(
                "RadioGroup",
                LucideIcon::CircleCheckBig,
                Some(ComponentId::RadioGroup),
            ),
            ComponentDef::new("Toggle", LucideIcon::Bold, Some(ComponentId::Toggle)),
            ComponentDef::new(
                "ToggleGroup",
                LucideIcon::AlignCenterHorizontal,
                Some(ComponentId::ToggleGroup),
            ),
            ComponentDef::new(
                "Slider",
                LucideIcon::SlidersHorizontal,
                Some(ComponentId::Slider),
            ),
            ComponentDef::new("Input", LucideIcon::SquarePen, Some(ComponentId::Input)),
            ComponentDef::new(
                "NumberInput",
                LucideIcon::Hash,
                Some(ComponentId::NumberInput),
            ),
            ComponentDef::new(
                "InputGroup",
                LucideIcon::Combine,
                Some(ComponentId::InputGroup),
            ),
            ComponentDef::new(
                "InputPasteClear",
                LucideIcon::ClipboardPaste,
                Some(ComponentId::InputPasteClear),
            ),
            ComponentDef::new(
                "TextareaPasteClear",
                LucideIcon::ClipboardPaste,
                Some(ComponentId::TextareaPasteClear),
            ),
            ComponentDef::new(
                "Textarea",
                LucideIcon::TextCursorInput,
                Some(ComponentId::Textarea),
            ),
            ComponentDef::new("Select", LucideIcon::ListFilter, Some(ComponentId::Select)),
            ComponentDef::new(
                "SelectValue",
                LucideIcon::ListEnd,
                Some(ComponentId::SelectValue),
            ),
            ComponentDef::new(
                "Combobox",
                LucideIcon::SquareChartGantt,
                Some(ComponentId::Combobox),
            ),
            ComponentDef::new(
                "InputOtp",
                LucideIcon::CircleDashed,
                Some(ComponentId::InputOtp),
            ),
            ComponentDef::new(
                "DatePicker",
                LucideIcon::Calendar,
                Some(ComponentId::DatePicker),
            ),
            ComponentDef::new(
                "ColorSwatch",
                LucideIcon::Paintbrush,
                Some(ComponentId::ColorSwatch),
            ),
        ],
    ),
    (
        CategoryId::Layout,
        LucideIcon::LayoutGrid,
        &[
            ComponentDef::new("Flex", LucideIcon::GripHorizontal, Some(ComponentId::Flex)),
            ComponentDef::new(
                "AspectRatio",
                LucideIcon::Ratio,
                Some(ComponentId::AspectRatio),
            ),
            ComponentDef::new("Card", LucideIcon::SquareStack, Some(ComponentId::Card)),
            ComponentDef::new(
                "Collapsible",
                LucideIcon::ChevronsDownUp,
                Some(ComponentId::Collapsible),
            ),
            ComponentDef::new(
                "Resizable",
                LucideIcon::MoveHorizontal,
                Some(ComponentId::Resizable),
            ),
            ComponentDef::new(
                "ScrollArea",
                LucideIcon::PanelsTopLeft,
                Some(ComponentId::ScrollArea),
            ),
            ComponentDef::new("Separator", LucideIcon::Minus, Some(ComponentId::Separator)),
            ComponentDef::new(
                "StatusBar",
                LucideIcon::PanelBottom,
                Some(ComponentId::StatusBar),
            ),
            ComponentDef::new("Tabs", LucideIcon::SquareMenu, Some(ComponentId::Tabs)),
            ComponentDef::new(
                "IconTabs",
                LucideIcon::AppWindow,
                Some(ComponentId::IconTabs),
            ),
            ComponentDef::new("Toolbar", LucideIcon::Wrench, Some(ComponentId::Toolbar)),
            ComponentDef::new(
                "Accordion",
                LucideIcon::ChevronsUpDown,
                Some(ComponentId::Accordion),
            ),
        ],
    ),
    (
        CategoryId::Overlays,
        LucideIcon::Layers,
        &[
            ComponentDef::new("Dialog", LucideIcon::AppWindow, Some(ComponentId::Dialog)),
            ComponentDef::new(
                "AlertDialog",
                LucideIcon::TriangleAlert,
                Some(ComponentId::AlertDialog),
            ),
            ComponentDef::new("Sheet", LucideIcon::PanelRight, Some(ComponentId::Sheet)),
            ComponentDef::new(
                "Popover",
                LucideIcon::PanelTopOpen,
                Some(ComponentId::Popover),
            ),
            ComponentDef::new(
                "HoverCard",
                LucideIcon::SquareMousePointer,
                Some(ComponentId::HoverCard),
            ),
            ComponentDef::new(
                "Tooltip",
                LucideIcon::MousePointerClick,
                Some(ComponentId::Tooltip),
            ),
            ComponentDef::new(
                "ContextMenu",
                LucideIcon::List,
                Some(ComponentId::ContextMenu),
            ),
            ComponentDef::new(
                "DropdownMenu",
                LucideIcon::Menu,
                Some(ComponentId::DropdownMenu),
            ),
            ComponentDef::new("Command", LucideIcon::Command, Some(ComponentId::Command)),
            ComponentDef::new(
                "Menubar",
                LucideIcon::SquareMenu,
                Some(ComponentId::Menubar),
            ),
            ComponentDef::new(
                "NavigationMenu",
                LucideIcon::Navigation,
                Some(ComponentId::NavigationMenu),
            ),
        ],
    ),
    (
        CategoryId::Feedback,
        LucideIcon::BellRing,
        &[
            ComponentDef::new("Alert", LucideIcon::CircleAlert, Some(ComponentId::Alert)),
            ComponentDef::new("Badge", LucideIcon::BadgeCheck, Some(ComponentId::Badge)),
            ComponentDef::new("Progress", LucideIcon::Gauge, Some(ComponentId::Progress)),
            ComponentDef::new(
                "Skeleton",
                LucideIcon::RectangleHorizontal,
                Some(ComponentId::Skeleton),
            ),
            ComponentDef::new(
                "Spinner",
                LucideIcon::LoaderCircle,
                Some(ComponentId::Spinner),
            ),
            ComponentDef::new("Toast", LucideIcon::BellRing, Some(ComponentId::Toast)),
            ComponentDef::new("Empty", LucideIcon::Inbox, Some(ComponentId::Empty)),
        ],
    ),
    (
        CategoryId::Data,
        LucideIcon::Database,
        &[
            ComponentDef::new("Avatar", LucideIcon::CircleUser, Some(ComponentId::Avatar)),
            ComponentDef::new(
                "Breadcrumb",
                LucideIcon::ChevronsRight,
                Some(ComponentId::Breadcrumb),
            ),
            ComponentDef::new(
                "Calendar",
                LucideIcon::Calendar,
                Some(ComponentId::Calendar),
            ),
            ComponentDef::new("Carousel", LucideIcon::Images, Some(ComponentId::Carousel)),
            ComponentDef::new(
                "Pagination",
                LucideIcon::ChevronsLeft,
                Some(ComponentId::Pagination),
            ),
            ComponentDef::new("Sidebar", LucideIcon::PanelLeft, Some(ComponentId::Sidebar)),
            ComponentDef::new("Table", LucideIcon::Table2, Some(ComponentId::Table)),
            ComponentDef::new(
                "AreaChart",
                LucideIcon::ChartArea,
                Some(ComponentId::AreaChart),
            ),
        ],
    ),
    (
        CategoryId::Display,
        LucideIcon::Type,
        &[
            ComponentDef::new(
                "Typography",
                LucideIcon::Type,
                Some(ComponentId::Typography),
            ),
            ComponentDef::new("Label", LucideIcon::Tag, Some(ComponentId::Label)),
            ComponentDef::new("Kbd", LucideIcon::Keyboard, Some(ComponentId::Kbd)),
            ComponentDef::new("Item", LucideIcon::Rows3, Some(ComponentId::Item)),
            ComponentDef::new("Icons", LucideIcon::Component, Some(ComponentId::Icons)),
            ComponentDef::new("Image", LucideIcon::Image, Some(ComponentId::Image)),
        ],
    ),
    (
        CategoryId::Forms,
        LucideIcon::ListChecks,
        &[
            ComponentDef::new(
                "FieldGroup",
                LucideIcon::Boxes,
                Some(ComponentId::FieldGroup),
            ),
            ComponentDef::new("FieldSet", LucideIcon::Box, Some(ComponentId::FieldSet)),
            ComponentDef::new(
                "FieldLegend",
                LucideIcon::List,
                Some(ComponentId::FieldLegend),
            ),
            ComponentDef::new(
                "FieldDescription",
                LucideIcon::FileText,
                Some(ComponentId::FieldDescription),
            ),
            ComponentDef::new(
                "PropertyGrid",
                LucideIcon::SlidersVertical,
                Some(ComponentId::PropertyGrid),
            ),
            ComponentDef::new(
                "PropertyRow",
                LucideIcon::Rows3,
                Some(ComponentId::PropertyRow),
            ),
        ],
    ),
    (
        CategoryId::Responsive,
        LucideIcon::MonitorSmartphone,
        &[
            ComponentDef::new(
                "Breakpoint",
                LucideIcon::MonitorSmartphone,
                Some(ComponentId::Breakpoint),
            ),
            ComponentDef::new("Spacing", LucideIcon::Ruler, Some(ComponentId::Spacing)),
            ComponentDef::new(
                "FlexWrap",
                LucideIcon::GripHorizontal,
                Some(ComponentId::FlexWrap),
            ),
            ComponentDef::new(
                "TouchTarget",
                LucideIcon::Hand,
                Some(ComponentId::TouchTarget),
            ),
        ],
    ),
    (
        CategoryId::Platform,
        LucideIcon::Smartphone,
        &[
            ComponentDef::new("Storage", LucideIcon::Database, Some(ComponentId::Storage)),
            ComponentDef::new(
                "Clipboard",
                LucideIcon::Clipboard,
                Some(ComponentId::Clipboard),
            ),
            ComponentDef::new("Share", LucideIcon::Share2, Some(ComponentId::Share)),
            ComponentDef::new("DeepLink", LucideIcon::Link, Some(ComponentId::DeepLink)),
            ComponentDef::new("Files", LucideIcon::Files, Some(ComponentId::Files)),
            ComponentDef::new(
                "Download",
                LucideIcon::Download,
                Some(ComponentId::Download),
            ),
            ComponentDef::new("Nav", LucideIcon::Navigation, Some(ComponentId::Nav)),
            ComponentDef::new(
                "ProgressWorker",
                LucideIcon::LoaderCircle,
                Some(ComponentId::ProgressWorker),
            ),
            ComponentDef::new("PWA", LucideIcon::Globe, Some(ComponentId::Pwa)),
            ComponentDef::new("Encoding", LucideIcon::Code, Some(ComponentId::Encoding)),
            ComponentDef::new(
                "InFlight",
                LucideIcon::ShieldCheck,
                Some(ComponentId::InFlight),
            ),
            ComponentDef::new("Camera", LucideIcon::Camera, Some(ComponentId::Camera)),
            ComponentDef::new(
                "QrScanner",
                LucideIcon::ScanQrCode,
                Some(ComponentId::QrScanner),
            ),
            ComponentDef::new("QrImage", LucideIcon::QrCode, Some(ComponentId::QrImage)),
            ComponentDef::new("Thumbnail", LucideIcon::Image, Some(ComponentId::Thumbnail)),
            ComponentDef::new("Zip", LucideIcon::FileArchive, Some(ComponentId::Zip)),
            ComponentDef::new("Crypto", LucideIcon::Lock, Some(ComponentId::Crypto)),
            ComponentDef::new("Worker", LucideIcon::Cog, Some(ComponentId::Worker)),
            ComponentDef::new(
                "PlatformInfo",
                LucideIcon::Info,
                Some(ComponentId::PlatformInfo),
            ),
            ComponentDef::new(
                "Messages",
                LucideIcon::Languages,
                Some(ComponentId::Messages),
            ),
            ComponentDef::new(
                "Markdown",
                LucideIcon::FileText,
                Some(ComponentId::Markdown),
            ),
            ComponentDef::new("Package", LucideIcon::Package, Some(ComponentId::Package)),
            ComponentDef::new("WhiteLabel", LucideIcon::Tag, Some(ComponentId::WhiteLabel)),
        ],
    ),
];

/// Total number of components.
#[must_use]
pub fn component_count() -> usize {
    CATEGORIES
        .iter()
        .map(|(_, _, items)| items.len())
        .sum::<usize>()
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
#[derive(Default)]
pub struct DialogState {
    pub command_open: bool,
    pub dialog_open: bool,
    pub alert_dialog_open: bool,
}

#[derive(Default)]
pub struct SheetState {
    pub sheet_open: bool,
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

/// All state for the showcase demos, one field per interactive demo.
pub struct ShowcaseApp {
    pub persistent: PersistentState<()>,
    pub sidebar_collapsed: bool,
    pub selected: Option<ComponentId>,
    pub prev_selected: Option<ComponentId>,
    pub router: functora_egui::route::AppRouter<AppRoute, ()>,
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
    pub input_text: String,
    pub number_f64: f64,
    pub number_f32: f32,
    pub number_i32: i32,
    pub input_group_url: String,
    pub input_group_search: String,
    pub input_paste_clear_text: String,
    pub input_paste_clear_password: String,
    pub input_paste_clear_custom_default: String,
    pub input_paste_clear_custom_icons: String,
    pub input_paste_clear_copy: String,
    pub input_paste_clear_copy_custom: String,
    pub textarea_text: String,
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
    pub button_selected: bool,
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
            persistent: PersistentState::default(),
            sidebar_collapsed: true,
            selected: None,
            prev_selected: None,
            router: functora_egui::route::AppRouter::new(&mut (), AppRoute::default()),
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
            input_text: String::new(),
            number_f64: 42.0,
            number_f32: std::f32::consts::PI,
            number_i32: 10,
            input_group_url: String::new(),
            input_group_search: String::new(),
            input_paste_clear_text: String::new(),
            input_paste_clear_password: String::new(),
            input_paste_clear_custom_default: "default value".to_owned(),
            input_paste_clear_custom_icons: String::new(),
            input_paste_clear_copy: String::new(),
            input_paste_clear_copy_custom: String::new(),
            textarea_text: String::new(),
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
            button_selected: false,
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
        self.router.reset(&mut (), AppRoute::default());
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
        self.router.navigate(&mut (), route);
    }

    fn sync_from_router(&mut self) {
        let current = self.router.current().component();
        if current != self.selected {
            self.selected = current;
        }
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
                .side(functora_egui::SheetSide::Right)
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
            let entries: Vec<(ComponentId, CommandItem)> = CATEGORIES
                .iter()
                .flat_map(|(cat, _, defs)| {
                    defs.iter().filter_map(|def| {
                        def.id.map(|id| {
                            (
                                id,
                                CommandItem {
                                    group: (*cat).render(lang),
                                    group_icon: cat.icon(),
                                    label: def.name.to_owned(),
                                    icon: def.icon,
                                },
                            )
                        })
                    })
                })
                .collect();
            if let Some(id) = CommandValue::new(entries).placeholder(placeholder).show(
                ctx,
                &mut self.dialogs.command_open,
                &mut self.command_search,
            ) {
                self.navigate_to(Some(id));
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
        let pending_nav = std::cell::Cell::new(None::<ComponentId>);
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
                        let Some(id) = def.id else {
                            continue;
                        };
                        let is_selected = Some(id) == selected_snapshot
                            && pending_ref.get().is_none_or(|next| next == id);
                        if side_ui
                            .add(section_button(def, is_selected).full_width())
                            .clicked()
                        {
                            pending_ref.set(Some(id));
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
        if let Some(id) = pending_nav.get() {
            self.navigate_to(Some(id));
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
                    let _ = self.router.go_back(&mut ());
                    ctx.request_repaint();
                }
                functora_egui::NavAction::Forward => {
                    let _ = self.router.go_forward(&mut ());
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
