//! Feedback: alerts, badges, progress, skeletons, spinners, toasts, empty states.

use egui_shadcn::{
    Alert, AlertVariant, Badge, BadgeVariant, Button, ButtonVariant, Card, Empty, Flex, LucideIcon,
    Progress, Skeleton, Spinner, Typography,
};

impl crate::app::ShowcaseApp {
    pub(crate) fn demo_alert(ui: &mut egui::Ui) {
        _ = Typography::muted("A status message container with variants.").show(ui);
        ui.add_space(12.0);
        _ = Alert::new()
            .title("Heads up!")
            .variant(AlertVariant::Default)
            .show(ui, |ui23| {
                _ = ui23.label("You can add components to your app using the CLI.");
            });
        ui.add_space(8.0);
        _ = Alert::new()
            .title("Error")
            .variant(AlertVariant::Destructive)
            .show(ui, |ui24| {
                _ = ui24.label("Your session has expired. Please log in again.");
            });
    }

    pub(crate) fn demo_badge(ui: &mut egui::Ui) {
        _ = Typography::muted("Small labels for counts, states, and statuses.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small("Variants").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            _ = f.add(Badge::new("Default"));
            _ = f.add(Badge::new("Secondary").variant(BadgeVariant::Secondary));
            _ = f.add(Badge::new("Outline").variant(BadgeVariant::Outline));
            _ = f.add(Badge::new("Destructive").variant(BadgeVariant::Destructive));
        });
        ui.add_space(12.0);
    }

    pub(crate) fn demo_progress(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A progress indicator with a value 0.0..=1.0.").show(ui);
        ui.add_space(12.0);
        _ = Progress::new(self.progress_val).show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("{:.0}%", self.progress_val * 100.0)).show(ui);
    }

    pub(crate) fn demo_skeleton(ui: &mut egui::Ui) {
        _ = Typography::muted("Placeholder shimmering blocks while content loads.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).align_center().show(ui, |f| {
            _ = f.add(Skeleton::new(48.0, 48.0).circle());
            _ = f.grow_nested(1.0, Flex::column().gap(4.0), |f2| {
                _ = f2.add(Skeleton::new(200.0, 16.0));
                _ = f2.add(Skeleton::new(180.0, 16.0));
                _ = f2.add(Skeleton::new(120.0, 16.0));
            });
        });
    }

    pub(crate) fn demo_spinner(ui: &mut egui::Ui) {
        _ = Typography::muted("An animated loading indicator with sizes.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(16.0).align_center().show(ui, |f| {
            _ = f.ui(|ui59| {
                _ = Spinner::new().size(16.0).show(ui59);
            });
            _ = f.ui(|ui60| {
                _ = Spinner::new().size(24.0).show(ui60);
            });
            _ = f.ui(|ui61| {
                _ = Spinner::new().size(32.0).show(ui61);
            });
            _ = f.ui(|ui62| {
                _ = Spinner::new().size(48.0).show(ui62);
            });
        });
        ui.add_space(12.0);
        _ = Typography::small("Inside a button").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(
                Button::new("Loading")
                    .icon(LucideIcon::LoaderCircle)
                    .enabled(false),
            );
        });
    }

    pub(crate) fn demo_toast(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Transient notifications with variants and descriptions.").show(ui);
        ui.add_space(12.0);
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Default").variant(ButtonVariant::Outline))
                .response
                .clicked()
            {
                self.toast.add(
                    "Default toast",
                    egui_shadcn::ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            }
            if f.add(Button::new("Success").variant(ButtonVariant::Outline))
                .response
                .clicked()
            {
                self.toast.add(
                    "Success toast",
                    egui_shadcn::ToastVariant::Success,
                    ctx.input(|i| i.time),
                );
            }
            if f.add(Button::new("Destructive").variant(ButtonVariant::Destructive))
                .response
                .clicked()
            {
                self.toast.add(
                    "Destructive toast",
                    egui_shadcn::ToastVariant::Error,
                    ctx.input(|i| i.time),
                );
            }
        });
        ui.add_space(12.0);
        if Button::new("Toast with description")
            .variant(ButtonVariant::Outline)
            .show(ui)
            .clicked()
        {
            self.toast.add_with_description(
                "Scheduled: Catch up",
                "Friday, February 10, 2026 at 5:57 PM",
                egui_shadcn::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
    }

    pub(crate) fn demo_empty(ui: &mut egui::Ui) {
        _ = Typography::muted("A centered empty state for lists and searches.").show(ui);
        ui.add_space(12.0);
        _ = Empty::show(ui, |ui25| {
            _ = Card::new().show(ui25, |ui26| {
                _ = Button::icon_only(LucideIcon::Inbox)
                    .variant(ButtonVariant::Ghost)
                    .size(egui_shadcn::ComponentSize::Lg)
                    .show(ui26);
                ui26.add_space(4.0);
                _ = Typography::h4("No results found").show(ui26);
                ui26.add_space(4.0);
                _ = Typography::small("Try adjusting your search to find what you're looking for.")
                    .show(ui26);
                ui26.add_space(8.0);
                _ = Button::new("Reset Search")
                    .variant(ButtonVariant::Outline)
                    .size(egui_shadcn::ComponentSize::Sm)
                    .show(ui26);
            });
        });
    }
}
