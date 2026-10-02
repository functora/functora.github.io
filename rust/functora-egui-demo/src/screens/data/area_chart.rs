use functora_egui::snippet;
use functora_egui::{AreaChart, AreaSeries, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_area_chart(ui: &mut egui::Ui) {
        _ = Typography::muted("A stacked area chart with smooth curves.").show(ui);
        ui.add_space(12.0);
        let theme = functora_egui::ShadcnThemeExt::shadcn_theme(ui.ctx());
        _ = AreaChart::new(vec![
            "Jan".to_owned(),
            "Feb".to_owned(),
            "Mar".to_owned(),
            "Apr".to_owned(),
            "May".to_owned(),
            "Jun".to_owned(),
        ])
        .series(AreaSeries {
            values: vec![186.0, 305.0, 237.0, 73.0, 209.0, 214.0],
            color: theme.chart_1,
        })
        .series(AreaSeries {
            values: vec![80.0, 200.0, 120.0, 190.0, 130.0, 140.0],
            color: theme.chart_2,
        })
        .stacked()
        .height(260.0)
        .show(ui);

        snippet(
            ui,
            "// AreaChart: stacked area chart with smooth curves\nuse functora_egui::{AreaChart, AreaSeries};\n\nlet theme = ShadcnThemeExt::shadcn_theme(ui.ctx());\n\nlet months = [\"Jan\", \"Feb\", \"Mar\", \"Apr\", \"May\", \"Jun\"].map(str::to_owned);\nAreaChart::new(months.to_vec())\n    .series(AreaSeries {\n        values: vec![186.0, 305.0, 237.0, 73.0, 209.0, 214.0],\n        color: theme.chart_1,\n    })\n    .series(AreaSeries {\n        values: vec![80.0, 200.0, 120.0, 190.0, 130.0, 140.0],\n        color: theme.chart_2,\n    })\n    .stacked()\n    .height(260.0)\n    .show(ui);",
        );
    }
}
