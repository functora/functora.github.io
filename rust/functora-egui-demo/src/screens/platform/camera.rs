use functora_egui::snippet;
use functora_egui::{
    Badge, Button, ButtonVariant, Card, Flex, ShadcnThemeExt, Typography, spawn_async,
};

impl crate::state::ShowcaseApp {
    pub fn demo_camera(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Camera: check_camera/start_camera/capture_frame/stop_camera + begin/stop session. Web via getUserMedia/canvas, Android via Camera2 (stub), desktop via file-picker fallback.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            let busy = self.platform.camera_rx.is_some();
            if f.add(
                Button::new("Check")
                    .icon(functora_egui::LucideIcon::Camera)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                self.platform.camera_rx = Some(spawn_async(async move {
                    functora_egui::camera::check_camera()
                        .await
                        .map(|()| "Camera available".to_string())
                        .map_err(|e| e.to_string())
                }));
            }
            if f.add(
                Button::new("Start")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                self.platform.camera_rx = Some(spawn_async(async move {
                    functora_egui::camera::start_camera()
                        .await
                        .map(|()| "Camera started".to_string())
                        .map_err(|e| e.to_string())
                }));
            }
            if f.add(
                Button::new("Capture")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                self.platform.camera_rx = Some(spawn_async(async move {
                    let frame = functora_egui::camera::capture_frame()
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(format!(
                        "Frame {}x{} luma {} bytes",
                        frame.width,
                        frame.height,
                        frame.data.len()
                    ))
                }));
            }
            if f.add(
                Button::new("Stop")
                    .variant(ButtonVariant::Outline)
                    .enabled(!busy),
            )
            .inner
            .clicked()
            {
                self.platform.camera_rx = Some(spawn_async(async move {
                    functora_egui::camera::stop_camera()
                        .await
                        .map(|()| "Camera stopped".to_string())
                        .map_err(|e| e.to_string())
                }));
            }
        });
        ui.add_space(8.0);
        _ = Typography::small("On desktop this will report 'not available – use file picker' (expected). On web, use QrScanner below for live preview.").show(ui);
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(
                "Live preview via CameraView + CameraViewState (15 fps, auto-start, Start/Stop controls).",
            )
            .show(ui2);
            ui2.add_space(4.0);
            self.platform.camera_view_state.ensure_default_handler();
            let _ = functora_egui::CameraView::new()
                .controls(true)
                .show(ui2, &mut self.platform.camera_view_state);
            if self.platform.camera_view_state.is_running() {
                ui2.add_space(8.0);
                _ = ui2.add(Badge::new("Live preview running"));
            }
            if let Some(err) = self.platform.camera_view_state.error() {
                ui2.add_space(8.0);
                _ = ui2.label(
                    egui::RichText::new(format!("Error: {err}"))
                        .color(ui2.ctx().shadcn_theme().destructive)
                        .size(12.0),
                );
            }
        });

        snippet(
            ui,
            "// Camera: check + start + capture + stop\nuse functora_egui::camera::{check_camera, start_camera, capture_frame, stop_camera};\n\n// Check if camera is available\ncheck_camera().await?;\n\n// Start camera session\nstart_camera().await?;\n\n// Capture a frame\nlet frame = capture_frame().await?;\n// frame: CameraFrame { width, height, data: Vec<u8> (RGBA) }\neprintln!(\"captured {}x{}\", frame.width, frame.height);\n\n// Stop camera\nstop_camera().await?;\n\n// Live preview: stateful CameraView (call every frame)\nuse functora_egui::{CameraView, CameraViewState};\n\nlet mut camera_view = CameraViewState::new();\ncamera_view.ensure_default_handler();\nCameraView::new().controls(true).show(ui, &mut camera_view);",
        );
    }
}
