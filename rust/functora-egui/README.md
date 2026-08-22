# functora-egui

[shadcn/ui](https://ui.shadcn.com)-inspired widget library for [egui](https://github.com/emilk/egui).

60+ beautifully styled, ready-to-use components with built-in light and dark theming. Drop-in replacements for native egui widgets plus higher-level components like dialogs, date pickers, sidebars, and editor-ready controls.

**[Live Demo](https://functora.github.io/apps/functora-egui-demo/)** (runs in your browser via WebAssembly)

## Quick start

```toml
[dependencies]
functora-egui = "0.1"
```

```rust
// Set up the theme (e.g. in your eframe CreationContext)
functora_egui::setup_fonts(&cc.egui_ctx);
let theme = functora_egui::theme::shadcn_theme_light::light();
functora_egui::ShadcnThemeExt::set_shadcn_theme(&cc.egui_ctx, theme);

// Use components
functora_egui::Button::new("Click me").show(ui);
ui.add(functora_egui::Switch::new(&mut value).label("Dark mode"));
ui.add(functora_egui::Input::new(&mut text).placeholder("Type here..."));
ui.add(functora_egui::Select::new(&mut selected, &options).placeholder("Pick one..."));
```

## Components

| Category | Widgets |
|----------|---------|
| **Inputs** | Button, Checkbox, ColorSwatch, Input, InputOtp, Radio, RadioGroup, Select, Slider, Switch, Textarea, Toggle, ToggleGroup, Combobox, DatePicker |
| **Layout** | Accordion, AspectRatio, Card, Collapsible, Resizable, ScrollArea, Separator, StatusBar, Tabs, Toolbar, Flex |
| **Overlay** | AlertDialog, Command, ContextMenu, Dialog, Drawer, DropdownMenu, HoverCard, Menubar, NavigationMenu, Popover, Sheet, Tooltip |
| **Feedback** | Alert, Badge, Progress, Skeleton, Spinner, Toast |
| **Data** | Avatar, Breadcrumb, Calendar, Carousel, Pagination, Sidebar, Table |
| **Typography** | Typography, Label, Kbd |
| **Grouping** | ButtonGroup, InputGroup, FieldGroup, FieldSet, FieldLegend, FieldDescription, PropertyGrid, PropertyRow |
| **Icons** | 1600+ Lucide icons via `LucideIcon` |

## Theming

Built-in light and dark themes:

```rust
let light = functora_egui::theme::shadcn_theme_light::light();
let dark = functora_egui::theme::shadcn_theme_dark::dark();
functora_egui::ShadcnThemeExt::set_shadcn_theme(ctx, dark);
```

## Responsive design

Mobile-first adaptive UI out of the box, mirroring the conventions of
[functora-css](https://github.com/functora/functora-css): below 800px the
layout switches to a touch-optimized mobile scale, and content is capped at
1440px on wide screens.

```rust
use functora_egui::{Breakpoint, ResponsiveExt, Spacing};

let breakpoint = ui.breakpoint(); // Breakpoint::Mobile below 800px
let spacing = ui.responsive_spacing();
// Spacing::mobile():  touch height 44, page padding 16
// Spacing::desktop(): touch height 32, page padding 32
if ui.on_mobile() {
    // touch-first behavior
}
```

Widgets pick this up automatically:

- Buttons, inputs, selects, date pickers, comboboxes, navigation menus,
  pagination, and tabs use the touch height (44) and padding (14) on mobile.
- `Dialog` and `AlertDialog` become bottom sheets on mobile, with top-rounded
  corners and page padding, and stay centered windows on desktop.
- `Sheet` clamps its size so the panel always stays inside the screen.
- `Sidebar` renders a slide-in overlay drawer with a backdrop on mobile and
  an inline collapsible panel (or icon rail) on desktop. Pair it with
  `Sidebar::toggle_button(ui, &mut collapsed)`. Opt out of the drawer with
  `Sidebar::new().static_()` to always keep the inline panel.
- `Flex::row()` wraps onto multiple lines below 800px, so toolbars and card
  grids stay on screen on phones and keep a single row on desktop:

```rust
functora_egui::Flex::row()
    .gap(spacing.gap)
    .show(ui, |ui| { /* cards, forms, toolbars */ });
```

  Force a single line with `Flex::row().no_wrap_on_mobile()`; `Flex::column()`
  never wraps.

## Examples

Run locally:

```sh
cargo run --example demo
cargo run --example shadcn_demo
cargo run --example component_dashboard
```

The `functora-egui-demo` crate in this repository is a full interactive
showcase of every layout, component, widget, and feature, with light/dark
theming and responsive mobile behavior:

```sh
cargo run -p functora-egui-demo
```

Or try the [live web demo](https://functora.github.io/apps/functora-egui-demo/).

## License

MIT
