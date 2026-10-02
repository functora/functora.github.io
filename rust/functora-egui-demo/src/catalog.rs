use functora_egui::i18n::{I18N, Language};
use functora_egui::{Button, ButtonVariant, CommandItem, LucideIcon, Separator};

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
    ButtonGroup,
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
    Navbar,
    Footer,
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
    BlockingOverlay,
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
    Hyperlink,
    Hypertext,
    CodeSnippet,
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
    pub const ALL: [Self; 104] = [
        Self::Button,
        Self::ButtonGroup,
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
        Self::Navbar,
        Self::Footer,
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
        Self::BlockingOverlay,
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
        Self::Hyperlink,
        Self::Hypertext,
        Self::CodeSnippet,
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
            Self::ButtonGroup => "ButtonGroup",
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
            Self::Navbar => "Navbar",
            Self::Footer => "Footer",
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
            Self::BlockingOverlay => "BlockingOverlay",
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
            Self::Hyperlink => "Hyperlink",
            Self::Hypertext => "Hypertext",
            Self::CodeSnippet => "CodeSnippet",
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
            "buttongroup" => Some(Self::ButtonGroup),
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
            "navbar" => Some(Self::Navbar),
            "footer" => Some(Self::Footer),
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
            "blockingoverlay" => Some(Self::BlockingOverlay),
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
            "hyperlink" => Some(Self::Hyperlink),
            "hypertext" => Some(Self::Hypertext),
            "codesnippet" => Some(Self::CodeSnippet),
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
                "ButtonGroup",
                LucideIcon::Columns2,
                Some(ComponentId::ButtonGroup),
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
                LucideIcon::ClipboardX,
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
            ComponentDef::new("Tabs", LucideIcon::NotebookTabs, Some(ComponentId::Tabs)),
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
            ComponentDef::new("Navbar", LucideIcon::PanelTop, Some(ComponentId::Navbar)),
            ComponentDef::new("Footer", LucideIcon::Dock, Some(ComponentId::Footer)),
        ],
    ),
    (
        CategoryId::Overlays,
        LucideIcon::Layers,
        &[
            ComponentDef::new(
                "Dialog",
                LucideIcon::MessageSquare,
                Some(ComponentId::Dialog),
            ),
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
            ComponentDef::new(
                "BlockingOverlay",
                LucideIcon::Hourglass,
                Some(ComponentId::BlockingOverlay),
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
            ComponentDef::new("Toast", LucideIcon::Bell, Some(ComponentId::Toast)),
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
                LucideIcon::CalendarDays,
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
                LucideIcon::Heading1,
                Some(ComponentId::Typography),
            ),
            ComponentDef::new("Label", LucideIcon::Tag, Some(ComponentId::Label)),
            ComponentDef::new("Kbd", LucideIcon::Keyboard, Some(ComponentId::Kbd)),
            ComponentDef::new("Item", LucideIcon::Rows3, Some(ComponentId::Item)),
            ComponentDef::new("Icons", LucideIcon::Component, Some(ComponentId::Icons)),
            ComponentDef::new("Image", LucideIcon::Image, Some(ComponentId::Image)),
            ComponentDef::new("Hyperlink", LucideIcon::Link, Some(ComponentId::Hyperlink)),
            ComponentDef::new(
                "Hypertext",
                LucideIcon::Pilcrow,
                Some(ComponentId::Hypertext),
            ),
            ComponentDef::new(
                "CodeSnippet",
                LucideIcon::SquareCode,
                Some(ComponentId::CodeSnippet),
            ),
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
                LucideIcon::ListOrdered,
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
                LucideIcon::Rows2,
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
                LucideIcon::TabletSmartphone,
                Some(ComponentId::Breakpoint),
            ),
            ComponentDef::new("Spacing", LucideIcon::Ruler, Some(ComponentId::Spacing)),
            ComponentDef::new(
                "FlexWrap",
                LucideIcon::FoldHorizontal,
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
            ComponentDef::new("Storage", LucideIcon::HardDrive, Some(ComponentId::Storage)),
            ComponentDef::new(
                "Clipboard",
                LucideIcon::Clipboard,
                Some(ComponentId::Clipboard),
            ),
            ComponentDef::new("Share", LucideIcon::Share2, Some(ComponentId::Share)),
            ComponentDef::new("DeepLink", LucideIcon::Link2, Some(ComponentId::DeepLink)),
            ComponentDef::new("Files", LucideIcon::Files, Some(ComponentId::Files)),
            ComponentDef::new(
                "Download",
                LucideIcon::Download,
                Some(ComponentId::Download),
            ),
            ComponentDef::new("Nav", LucideIcon::Compass, Some(ComponentId::Nav)),
            ComponentDef::new(
                "ProgressWorker",
                LucideIcon::LoaderPinwheel,
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
            ComponentDef::new(
                "Thumbnail",
                LucideIcon::FileImage,
                Some(ComponentId::Thumbnail),
            ),
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
                LucideIcon::FileCode,
                Some(ComponentId::Markdown),
            ),
            ComponentDef::new("Package", LucideIcon::Package, Some(ComponentId::Package)),
            ComponentDef::new(
                "WhiteLabel",
                LucideIcon::Tags,
                Some(ComponentId::WhiteLabel),
            ),
        ],
    ),
];

/// Total number of components.
#[must_use]
pub fn component_count() -> usize {
    CATEGORIES
        .iter()
        .flat_map(|(_, _, items)| items.iter())
        .filter(|def| def.id.is_some())
        .count()
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

/// Search palette entries with an ungrouped Overview button first, then
/// every component in catalog order grouped by category. Single source of
/// truth for the command palette so the sidebar, palette, and tests share
/// one ordering.
#[must_use]
pub fn palette_entries(lang: Language) -> Vec<(Option<ComponentId>, CommandItem)> {
    [(
        None,
        CommandItem {
            group: String::new(),
            group_icon: LucideIcon::Sparkles,
            label: "Overview".to_owned(),
            icon: LucideIcon::Sparkles,
        },
    )]
    .into_iter()
    .chain(CATEGORIES.iter().flat_map(|(cat, _, defs)| {
        defs.iter().filter_map(|def| {
            def.id.map(|id| {
                (
                    Some(id),
                    CommandItem {
                        group: (*cat).render(lang),
                        group_icon: cat.icon(),
                        label: def.name.to_owned(),
                        icon: def.icon,
                    },
                )
            })
        })
    }))
    .collect()
}
