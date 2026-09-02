//! Light theme migrated from functora-css — hsl(200,10%) cool slate.

/// Creates the light theme with colors derived from functora-css variables.
#[must_use]
pub fn light() -> super::shadcn_theme::ShadcnTheme {
    super::shadcn_theme::ShadcnTheme {
        background: egui::Color32::from_rgb(255, 255, 255), // --bg-color #fff
        foreground: egui::Color32::from_rgb(46, 53, 56),    // --black hsl(200,10%,20%) #2e3538
        card: egui::Color32::from_rgb(255, 255, 255),       // --surface #fff
        card_foreground: egui::Color32::from_rgb(46, 53, 56), // --black
        popover: egui::Color32::from_rgb(255, 255, 255),
        popover_foreground: egui::Color32::from_rgb(46, 53, 56),
        primary: egui::Color32::from_rgb(102, 119, 153), // --primary #679 #667799
        primary_foreground: egui::Color32::from_rgb(255, 255, 255), // --primary-contrast #fff
        secondary: egui::Color32::from_rgb(233, 235, 237), // muted slate hsl(200,10%,92%) #e9ebed
        secondary_foreground: egui::Color32::from_rgb(46, 53, 56), // --black hsl(200,10%,20%) #2e3538
        muted: egui::Color32::from_rgb(244, 245, 246), // --grey-ultralight hsl(200,10%,96%)
        muted_foreground: egui::Color32::from_rgb(69, 79, 84), // --grey-ultradark hsl(200,10%,30%) #454f54
        accent: egui::Color32::from_rgb(199, 206, 209), // --grey-light hsl(200,10%,80%) #c7ced1
        accent_foreground: egui::Color32::from_rgb(46, 53, 56),
        destructive: egui::Color32::from_rgb(229, 72, 77), // shadcn oklch(0.577 0.245 27.325) spirit
        destructive_foreground: egui::Color32::from_rgb(255, 255, 255),
        success: egui::Color32::from_rgb(39, 174, 96), // extended: muted green #27ae60
        success_foreground: egui::Color32::from_rgb(46, 53, 56),
        warning: egui::Color32::from_rgb(234, 179, 8), // extended: amber #eab308
        warning_foreground: egui::Color32::from_rgb(46, 53, 56),
        info: egui::Color32::from_rgb(59, 130, 246), // extended: blue #3b82f6
        info_foreground: egui::Color32::from_rgb(255, 255, 255),
        chart_1: egui::Color32::from_rgb(102, 119, 153), // primary
        chart_2: egui::Color32::from_rgb(39, 174, 96),   // success
        chart_3: egui::Color32::from_rgb(234, 179, 8),   // warning
        chart_4: egui::Color32::from_rgb(59, 130, 246),  // info
        chart_5: egui::Color32::from_rgb(139, 92, 246),  // violet #8b5cf6
        border: egui::Color32::from_rgb(199, 206, 209),  // --grey-light hsl(200,10%,80%)
        input: egui::Color32::from_rgb(171, 181, 186),   // --grey-mid hsl(200,10%,70%) #abb5ba
        ring: egui::Color32::from_rgb(102, 119, 153),    // --primary focus
        radius: 10.0,
    }
}
