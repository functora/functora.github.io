use functora_egui::Table;
use functora_egui::snippet;

impl crate::state::ShowcaseApp {
    pub fn demo_table(ui: &mut egui::Ui) {
        crate::screens::intro(
            ui,
            "A styled table with headers, rows, and optional striping.",
        );
        let headers = vec!["Name".to_owned(), "Status".to_owned(), "Role".to_owned()];
        let rows = vec![
            vec![
                "Ada Lovelace".to_owned(),
                "Active".to_owned(),
                "Admin".to_owned(),
            ],
            vec![
                "Alan Turing".to_owned(),
                "Active".to_owned(),
                "Editor".to_owned(),
            ],
            vec![
                "Grace Hopper".to_owned(),
                "Inactive".to_owned(),
                "Viewer".to_owned(),
            ],
            vec![
                "Edsger Dijkstra".to_owned(),
                "Active".to_owned(),
                "Editor".to_owned(),
            ],
        ];
        _ = Table::new(headers).rows(rows).striped().show(ui);
        ui.add_space(12.0);
        crate::screens::section(ui, "With custom column weights");
        _ = Table::new(vec![
            "Name".to_owned(),
            "Email".to_owned(),
            "Role".to_owned(),
        ])
        .rows(vec![
            vec![
                "Ada Lovelace".to_owned(),
                "ada@example.com".to_owned(),
                "Admin".to_owned(),
            ],
            vec![
                "Alan Turing".to_owned(),
                "alan@example.com".to_owned(),
                "Editor".to_owned(),
            ],
        ])
        .col_weights(vec![0.4, 0.4, 0.2])
        .show(ui);

        snippet(
            ui,
            "// Table: styled table with headers, rows, striping, column weights\nuse functora_egui::Table;\n\nlet headers = vec![\"Name\".to_owned(), \"Status\".to_owned(), \"Role\".to_owned()];\nlet rows = vec![\n    vec![\"Ada Lovelace\".to_owned(), \"Active\".to_owned(), \"Admin\".to_owned()],\n    vec![\"Alan Turing\".to_owned(), \"Active\".to_owned(), \"Editor\".to_owned()],\n    vec![\"Grace Hopper\".to_owned(), \"Inactive\".to_owned(), \"Viewer\".to_owned()],\n    vec![\"Edsger Dijkstra\".to_owned(), \"Active\".to_owned(), \"Editor\".to_owned()],\n];\n\n// Basic with striping\nTable::new(headers.clone()).rows(rows.clone()).striped().show(ui);\n\n// Custom column weights\nTable::new(vec![\"Name\".to_owned(), \"Email\".to_owned(), \"Role\".to_owned()])\n    .rows(vec![\n        vec![\"Ada Lovelace\".to_owned(), \"ada@example.com\".to_owned(), \"Admin\".to_owned()],\n        vec![\"Alan Turing\".to_owned(), \"alan@example.com\".to_owned(), \"Editor\".to_owned()],\n    ])\n    .col_weights(vec![0.4, 0.4, 0.2])\n    .show(ui);",
        );
    }
}
