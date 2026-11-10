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
            let lang = self.persistent.language;
            let _ = functora_egui::CameraView::new()
                .desired_size(egui::vec2(320.0, 240.0))
                .fps(30.0)
                .auto_start(true)
                .controls(true)
                .show(ui2, &mut self.platform.camera_view_state, lang);
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
            ui2.add_space(8.0);
            _ = Typography::small(format!(
                "interval_ms: {}, preview_size: {:?}, camera_epoch: {}",
                self.platform.camera_view_state.interval_ms(),
                self.platform.camera_view_state.preview_size(),
                functora_egui::widgets::camera_view::camera_view_state::camera_epoch()
            ))
            .show(ui2);
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Set 10 fps").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.camera_view_state.set_fps(10.0);
            }
            if f.add(Button::new("Clear error").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.camera_view_state.clear_error();
            }
            if f.add(Button::new("Stop capture worker").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                functora_egui::camera::stop_capture_worker();
            }
            if f.add(Button::new("Begin capture session").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                functora_egui::camera::begin_capture_session();
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "CameraStartStatus: {:?}, {:?}, {:?} (from code: {:?})",
            functora_egui::camera::CameraStartStatus::Running,
            functora_egui::camera::CameraStartStatus::Pending,
            functora_egui::camera::CameraStartStatus::Failed,
            functora_egui::camera::CameraStartStatus::from(-1)
        ))
        .show(ui);

        snippet(
            ui,
            "// Camera: check + start + capture + stop\nuse functora_egui::camera::{check_camera, start_camera, capture_frame, stop_camera};\n\n// Check if camera is available\ncheck_camera().await?;\n\n// Start camera session\nstart_camera().await?;\n\n// Capture a frame\nlet frame = capture_frame().await?;\n// frame: FrameData { width, height, data: Vec<u8> (luma), preview_rgba: Option<Vec<u8>> }\neprintln!(\"captured {}x{}\", frame.width, frame.height);\n\n// Rotate the frame upright (portrait phones capture rotated buffers)\nuse functora_egui::frame::FrameRotation;\nlet frame = frame.upright(FrameRotation::Cw90);\n\n// Stop camera\nstop_camera().await?;\n\n// The capture worker can be driven without the widget\nuse functora_egui::camera::{begin_capture_session, stop_capture_worker};\n\nbegin_capture_session();\n// ... capture frames ...\nstop_capture_worker();\n\n// The backend reports its start state as a code\nuse functora_egui::camera::CameraStartStatus;\n\nassert_eq!(CameraStartStatus::from(1), CameraStartStatus::Running);\n\n// Live preview: stateful CameraView (call every frame)\nuse functora_egui::{CameraView, CameraViewState};\n\nlet mut camera_view = CameraViewState::new();\ncamera_view.ensure_default_handler();\nCameraView::new()\n    .desired_size(egui::vec2(320.0, 240.0)) // fixed preview box\n    .fps(30.0)                       // preview capture rate\n    .auto_start(true)                // start on the first show\n    .controls(true)                  // Start/Stop buttons\n    .show(ui, &mut camera_view, lang);\n\n// The state exposes the capture interval and the last preview size\ncamera_view.set_fps(10.0);\neprintln!(\"interval: {} ms\", camera_view.interval_ms());\neprintln!(\"preview: {:?}\", camera_view.preview_size());\n\n// The feed state and its epoch are shared between the widget and the pump\nuse functora_egui::widgets::camera_view::camera_view_state::{CAMERA_EPOCH, FeedInner, FrameHandler};\n\neprintln!(\"epoch: {}\", CAMERA_EPOCH.load(std::sync::atomic::Ordering::SeqCst));\n\n// A custom frame handler replaces the default pass-through\ncamera_view.set_handler(Some(Box::new(|_ctx, _frame| {\n    std::ops::ControlFlow::Continue(())\n})));\n\n// with_fps sets the capture rate while building the state\nlet camera_view = CameraViewState::new().with_fps(15.0);",
        );
    }
}
