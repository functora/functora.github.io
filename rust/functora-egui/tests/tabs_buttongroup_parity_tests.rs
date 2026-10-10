//! Tabs must paint exactly like `ButtonGroup` + Outline `Button`s:
//! selected tab uses the accent pair, idle tabs use the background,
//! and the muted pill container is gone.

use egui::{Color32, Context, Event, Pos2, RawInput, Rect, Shape, Vec2};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Fruit {
    Apple,
    Banana,
    Cherry,
}

fn entries() -> [(Fruit, String); 3] {
    [
        (Fruit::Apple, "Apple".to_owned()),
        (Fruit::Banana, "Banana".to_owned()),
        (Fruit::Cherry, "Cherry".to_owned()),
    ]
}

struct Harness {
    ctx: Context,
    frame: u32,
}

impl Harness {
    fn new() -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
        }
    }

    fn step(
        &mut self,
        events: Vec<Event>,
        body: &mut dyn FnMut(&mut egui::Ui),
    ) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        });
        out.textures_delta.clear();
        out
    }

    fn rect_fills(out: &egui::FullOutput) -> Vec<(Rect, Color32)> {
        out.shapes
            .iter()
            .filter_map(|clipped| match &clipped.shape {
                Shape::Rect(rect_shape) => Some((rect_shape.rect, rect_shape.fill)),
                _ => None,
            })
            .collect()
    }
}

fn tab_button_fills(out: &egui::FullOutput) -> Vec<Color32> {
    Harness::rect_fills(out)
        .into_iter()
        .filter(|(rect, fill)| {
            (rect.height() - 32.0).abs() < 1.0
                && rect.width() > 20.0
                && *fill != Color32::TRANSPARENT
        })
        .map(|(_, fill)| fill)
        .collect()
}

fn has_fill(out: &egui::FullOutput, target: Color32) -> bool {
    Harness::rect_fills(out)
        .into_iter()
        .any(|(_, fill)| fill == target)
}

#[test]
fn tabs_value_matches_buttongroup_colors() {
    let all = entries();
    let mut selected = Fruit::Banana;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TabsValue::new(&all).show(ui, &mut selected, |_, _| {});
    };
    let out = app.step(vec![], &mut body);
    let theme = functora_egui::ShadcnTheme::default();
    let mut fills = tab_button_fills(&out);
    fills.sort_by_key(egui::Color32::to_srgba_unmultiplied);
    assert_eq!(
        fills.len(),
        3,
        "tab bar must paint three opaque tab rects, got {fills:?}"
    );
    assert_eq!(
        fills.iter().filter(|fill| **fill == theme.accent).count(),
        1,
        "exactly one tab must use the ButtonGroup selected (accent) fill, got {fills:?}"
    );
    assert_eq!(
        fills
            .iter()
            .filter(|fill| **fill == theme.background)
            .count(),
        2,
        "idle tabs must use the ButtonGroup Outline (background) fill, got {fills:?}"
    );
    assert!(
        !has_fill(&out, theme.muted),
        "tab bar must not paint the old muted pill container"
    );
}

#[test]
fn icon_tabs_value_matches_buttongroup_colors() {
    let all = [
        (
            Fruit::Apple,
            functora_egui::TabEntry::Icon {
                icon: functora_egui::LucideIcon::House,
                tooltip: "Home".to_owned(),
            },
        ),
        (
            Fruit::Banana,
            functora_egui::TabEntry::Icon {
                icon: functora_egui::LucideIcon::Settings,
                tooltip: "Settings".to_owned(),
            },
        ),
        (
            Fruit::Cherry,
            functora_egui::TabEntry::Icon {
                icon: functora_egui::LucideIcon::Bell,
                tooltip: "Notifications".to_owned(),
            },
        ),
    ];
    let mut selected = Fruit::Apple;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::IconTabsValue::new(&all).show(ui, &mut selected, |_, _| {});
    };
    let out = app.step(vec![], &mut body);
    let theme = functora_egui::ShadcnTheme::default();
    let mut fills = tab_button_fills(&out);
    fills.sort_by_key(egui::Color32::to_srgba_unmultiplied);
    assert_eq!(
        fills.len(),
        3,
        "icon tab bar must paint three opaque tab rects, got {fills:?}"
    );
    assert_eq!(
        fills.iter().filter(|fill| **fill == theme.accent).count(),
        1,
        "exactly one icon tab must use the ButtonGroup selected (accent) fill, got {fills:?}"
    );
    assert_eq!(
        fills
            .iter()
            .filter(|fill| **fill == theme.background)
            .count(),
        2,
        "idle icon tabs must use the ButtonGroup Outline (background) fill, got {fills:?}"
    );
    assert!(
        !has_fill(&out, theme.muted),
        "icon tab bar must not paint the old muted pill container"
    );
}
