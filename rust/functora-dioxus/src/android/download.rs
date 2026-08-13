use crate::Error;
use crate::android::save_to_downloads;
use crate::progress::Job;
use crate::worker;
use dioxus::prelude::Writable;

pub async fn download_package<P, S, D>(data: D, filename: &str, progress: P, stage: S) -> Result<String, Error>
where
    P: Writable<Target = Option<Job<S>>> + Copy + 'static,
    S: Clone + Send + 'static,
    D: AsRef<[u8]> + Send + 'static,
{
    let name = filename.to_string();
    worker::run(
        (data, name),
        progress,
        move |(bytes, file_name), mut report| async move {
            save_to_downloads(bytes.as_ref(), file_name.clone(), move |done, total| {
                report(Job {
                    stage: stage.clone(),
                    done,
                    total,
                    name: None,
                });
            })?;
            Ok(file_name)
        },
    )
    .await
}
