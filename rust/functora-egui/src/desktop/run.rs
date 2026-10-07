#[must_use]
pub fn launch_args() -> Vec<String> {
    std::env::args()
        .skip(1)
        .filter(|arg| arg != "--" && !arg.starts_with('-'))
        .collect()
}

#[must_use]
pub fn take_file_args() -> Vec<std::path::PathBuf> {
    launch_args()
        .into_iter()
        .map(std::path::PathBuf::from)
        .filter(|path| path.exists())
        .collect()
}

pub fn ingest_launch_args() {
    launch_args()
        .into_iter()
        .for_each(crate::deep_link::store_url);
}

fn viewport(app_id: &str) -> egui::ViewportBuilder {
    egui::ViewportBuilder::default()
        .with_inner_size([1100.0, 750.0])
        .with_min_inner_size([320.0, 480.0])
        .with_app_id(app_id)
}

fn options(app_id: &str) -> eframe::NativeOptions {
    eframe::NativeOptions {
        viewport: viewport(app_id),
        ..Default::default()
    }
}

fn options_with_icon(app_id: &str, icon: Option<egui::IconData>) -> eframe::NativeOptions {
    icon.map_or_else(
        || options(app_id),
        |data| eframe::NativeOptions {
            viewport: viewport(app_id).with_icon(data),
            ..Default::default()
        },
    )
}

#[cfg(feature = "images")]
#[must_use]
pub fn icon_from_png(bytes: &[u8]) -> Option<egui::IconData> {
    image::load_from_memory(bytes).ok().map(|decoded| {
        let rgba = decoded.to_rgba8();
        let (width, height) = (rgba.width(), rgba.height());
        egui::IconData {
            rgba: rgba.into_raw(),
            width,
            height,
        }
    })
}

pub fn run<F>(title: &str, app_id: &str, creator: F)
where
    F: FnOnce(
            &eframe::CreationContext<'_>,
        ) -> Result<Box<dyn eframe::App>, Box<dyn std::error::Error + Send + Sync>>
        + 'static,
{
    run_with_icon(title, app_id, None, creator);
}

pub fn run_with_icon<F>(title: &str, app_id: &str, icon: Option<egui::IconData>, creator: F)
where
    F: FnOnce(
            &eframe::CreationContext<'_>,
        ) -> Result<Box<dyn eframe::App>, Box<dyn std::error::Error + Send + Sync>>
        + 'static,
{
    ingest_launch_args();
    if let Err(error) =
        eframe::run_native(title, options_with_icon(app_id, icon), Box::new(creator))
    {
        eprintln!("eframe error: {error}");
    }
}
