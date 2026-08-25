//! Dark theme migrated from functora-css — lifted #222/#333.

/// Creates the dark theme with colors derived from functora-css variables.
#[must_use]
pub fn dark() -> super::shadcn_theme::ShadcnTheme {
    super::shadcn_theme::ShadcnTheme {
        background: egui::Color32::from_rgb(34, 34, 34), // --bg-color #222
        foreground: egui::Color32::from_rgb(199, 206, 209), // --black hsl(200,10%,80%) #c7ced1
        card: egui::Color32::from_rgb(51, 51, 51),       // --surface #333
        card_foreground: egui::Color32::from_rgb(199, 206, 209),
        popover: egui::Color32::from_rgb(51, 51, 51),
        popover_foreground: egui::Color32::from_rgb(199, 206, 209),
        primary: egui::Color32::from_rgb(136, 153, 187), // --primary #89b #8899bb
        primary_foreground: egui::Color32::from_rgb(46, 53, 56), // dark on light slate for contrast
        secondary: egui::Color32::from_rgb(46, 53, 56),  // muted slate hsl(200,10%,20%) #2e3538
        secondary_foreground: egui::Color32::from_rgb(227, 230, 232), // --grey-ultradark hsl(200,10%,90%) #e3e6e8
        muted: egui::Color32::from_rgb(23, 26, 28), // --grey-ultralight hsl(200,10%,10%) #171a1c
        muted_foreground: egui::Color32::from_rgb(143, 156, 163), // --grey-dark hsl(200,10%,60%) #8f9ca3
        accent: egui::Color32::from_rgb(69, 79, 84), // --grey-light hsl(200,10%,30%) #454f54
        accent_foreground: egui::Color32::from_rgb(227, 230, 232), // --grey-ultradark hsl(200,10%,90%) #e3e6e8
        destructive: egui::Color32::from_rgb(255, 99, 105), // shadcn oklch(0.704 0.191 22.216) spirit
        destructive_foreground: egui::Color32::from_rgb(255, 255, 255),
        success: egui::Color32::from_rgb(34, 197, 94), // extended: #22c55e
        success_foreground: egui::Color32::from_rgb(46, 53, 56),
        warning: egui::Color32::from_rgb(234, 179, 8), // extended: #eab308
        warning_foreground: egui::Color32::from_rgb(46, 53, 56),
        info: egui::Color32::from_rgb(96, 165, 250), // extended: #60a5fa lightened for dark
        info_foreground: egui::Color32::from_rgb(46, 53, 56),
        chart_1: egui::Color32::from_rgb(136, 153, 187), // primary
        chart_2: egui::Color32::from_rgb(34, 197, 94),   // success
        chart_3: egui::Color32::from_rgb(234, 179, 8),   // warning
        chart_4: egui::Color32::from_rgb(96, 165, 250),  // info
        chart_5: egui::Color32::from_rgb(167, 139, 250), // violet #a78bfa
        border: egui::Color32::from_rgb(69, 79, 84),     // --grey-light hsl(200,10%,30%) opaque
        input: egui::Color32::from_rgb(115, 132, 140),   // --grey-mid hsl(200,10%,50%) #73848c
        ring: egui::Color32::from_rgb(136, 153, 187),    // --primary focus
        radius: 10.0,
    }
}
