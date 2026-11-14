use crate::Error;
use crate::progress::{Job, yield_to_paint};
use crate::worker::Reporter;
use std::io::{Cursor, Read, Write};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use zip::CompressionMethod;

pub const ATTACHMENTS_PREFIX: &str = "attachments/";
pub(crate) const ZIP_CHUNK: usize = 2 * 1024 * 1024;
pub const MAX_ARCHIVE_ENTRIES: usize = 4096;
pub const MAX_ARCHIVE_ENTRY_SIZE: u64 = 256 * 1024 * 1024;
pub const MAX_ARCHIVE_TOTAL_SIZE: u64 = 512 * 1024 * 1024;

pub type CancelToken = Arc<AtomicBool>;

pub(crate) fn check_cancel(cancel: Option<&CancelToken>) -> Result<(), Error> {
    if cancel.is_some_and(|c| c.load(Ordering::Relaxed)) {
        return Err(Error::Cancelled);
    }
    Ok(())
}

pub(crate) fn check_entry_count(count: usize) -> Result<(), Error> {
    (count <= MAX_ARCHIVE_ENTRIES)
        .then_some(())
        .ok_or(Error::TooManyEntries {
            count,
            limit: MAX_ARCHIVE_ENTRIES,
        })
}

pub(crate) fn check_entry_size(name: &str, size: u64) -> Result<(), Error> {
    (size <= MAX_ARCHIVE_ENTRY_SIZE)
        .then_some(())
        .ok_or_else(|| Error::FileTooLarge {
            name: name.to_string(),
            size,
            limit: MAX_ARCHIVE_ENTRY_SIZE,
        })
}

pub(crate) fn check_total_size(size: u64) -> Result<(), Error> {
    (size <= MAX_ARCHIVE_TOTAL_SIZE)
        .then_some(())
        .ok_or_else(|| Error::FileTooLarge {
            name: "archive".to_string(),
            size,
            limit: MAX_ARCHIVE_TOTAL_SIZE,
        })
}

fn opts() -> zip::write::FileOptions<'static, 'static, ()> {
    zip::write::FileOptions::default()
        .compression_method(CompressionMethod::Deflated)
        .compression_level(Some(1))
}

pub(crate) async fn zip_entries<S>(
    zip: &mut zip::ZipWriter<Cursor<&mut Vec<u8>>>,
    entries: &[(String, Arc<[u8]>)],
    stage: S,
    report: &mut Reporter<S>,
    done: &mut u64,
    total: u64,
    cancel: Option<&CancelToken>,
) -> Result<(), Error>
where
    S: Copy + Send + Sync + 'static,
{
    for (name, data) in entries {
        zip.start_file(name, opts())?;
        let display = name.strip_prefix(ATTACHMENTS_PREFIX).unwrap_or(name);
        for chunk in data.chunks(ZIP_CHUNK) {
            check_cancel(cancel)?;
            zip.write_all(chunk)?;
            *done += chunk.len() as u64;
            report(Job {
                stage,
                done: *done,
                total,
                name: Some(display.to_string()),
            });
            yield_to_paint().await;
        }
    }
    Ok(())
}

pub async fn create_zip_report<S>(
    entries: Vec<(String, Arc<[u8]>)>,
    stage: S,
    report: &mut Reporter<S>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let mut buf = Vec::new();
    let total = entries.iter().map(|(_, d)| d.len() as u64).sum::<u64>();
    let mut done = 0u64;
    {
        let mut zip = zip::ZipWriter::new(Cursor::new(&mut buf));
        zip_entries(&mut zip, &entries, stage, report, &mut done, total, cancel).await?;
        let _ = zip.finish()?;
    }
    Ok(buf)
}

pub async fn unzip_report<S>(
    inner: Vec<u8>,
    stage: S,
    report: &mut Reporter<S>,
    cancel: Option<&CancelToken>,
) -> Result<Vec<(String, Vec<u8>)>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let mut archive = zip::ZipArchive::new(Cursor::new(inner))?;
    check_entry_count(archive.len())?;
    let total = (0..archive.len())
        .map(|i| archive.by_index(i).map(|f| f.size()))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .sum::<u64>();
    check_total_size(total)?;
    let mut entries = Vec::new();
    let mut done = 0u64;
    let mut chunk = vec![0u8; ZIP_CHUNK];
    for i in 0..archive.len() {
        let (name, data) = {
            let mut file = archive.by_index(i)?;
            let name = file.name()?.into_owned();
            check_entry_size(&name, file.size())?;
            let capacity =
                usize::try_from(file.size().min(ZIP_CHUNK as u64)).map_err(|e| Error::Convert {
                    context: "zip entry capacity exceeds usize range",
                    source: e,
                })?;
            let mut data = Vec::with_capacity(capacity);
            let mut entry_read = 0u64;
            loop {
                check_cancel(cancel)?;
                let read = file.read(&mut chunk)?;
                if read == 0 {
                    break;
                }
                entry_read += read as u64;
                done += read as u64;
                check_entry_size(&name, entry_read)?;
                check_total_size(done)?;
                data.extend_from_slice(&chunk[..read]);
                report(Job {
                    stage,
                    done,
                    total,
                    name: Some(name.clone()),
                });
                #[cfg(target_arch = "wasm32")]
                yield_to_paint().await;
            }
            (name, data)
        };
        yield_to_paint().await;
        entries.push((name, data));
    }
    Ok(entries)
}
