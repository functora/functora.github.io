pub use functora_core::package::{
    ArchiveSource, PackageStages, extract_package_report, package_report,
};

use crate::error::Error;
use crate::progress::Job;
use crate::worker;
use functora_core::crypto::CipherType;
use zeroize::Zeroizing;

pub async fn package_async<S, P>(
    entries: Vec<(String, Vec<u8>)>,
    password: &str,
    cipher: Option<CipherType>,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    progress: P,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
    P: FnMut(Option<Job<S>>) + Send + 'static,
{
    let password_owned = Zeroizing::new(password.to_string());
    worker::run(
        (entries, password_owned, cipher),
        progress,
        move |(entries_owned, password_clone, cipher_sel), mut report| async move {
            package_report(
                entries_owned,
                password_clone.as_str(),
                cipher_sel,
                prefix,
                stages,
                &mut report,
            )
            .await
        },
    )
    .await
}

pub async fn extract_package_async<S, P>(
    source: ArchiveSource,
    password: &str,
    prefix: &'static [u8],
    stages: PackageStages<S>,
    progress: P,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
    P: FnMut(Option<Job<S>>) + Send + 'static,
{
    let password_owned = Zeroizing::new(password.to_string());
    worker::run(
        source,
        progress,
        move |source_owned, mut report| async move {
            extract_package_report(
                source_owned,
                password_owned.as_str(),
                prefix,
                stages,
                &mut report,
            )
            .await
        },
    )
    .await
}
