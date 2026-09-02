use crate::Error;
use crate::progress::{Job, yield_to_paint};
use crate::worker::Reporter;
use std::io::{Cursor, Read, Write};
use zip::CompressionMethod;

const ZIP_CHUNK: usize = 2 * 1024 * 1024;

fn opts() -> zip::write::FileOptions<'static, ()> {
    zip::write::FileOptions::default()
        .compression_method(CompressionMethod::Deflated)
        .compression_level(Some(1))
}

pub(crate) async fn zip_entries<S>(
    zip: &mut zip::ZipWriter<Cursor<&mut Vec<u8>>>,
    entries: &[(String, Vec<u8>)],
    stage: S,
    report: &mut Reporter<S>,
    done: &mut u64,
    total: u64,
) -> Result<(), Error>
where
    S: Copy + Send + Sync + 'static,
{
    for (name, data) in entries {
        zip.start_file(name, opts())?;
        let display = name.strip_prefix("attachments/").unwrap_or(name);
        for chunk in data.chunks(ZIP_CHUNK) {
            zip.write_all(chunk)?;
            *done += chunk.len() as u64;
            report(Job {
                stage,
                done: *done,
                total,
                name: Some(display.to_string()),
            });
        }
        yield_to_paint().await;
    }
    Ok(())
}

pub async fn create_zip_report<S>(
    entries: Vec<(String, Vec<u8>)>,
    stage: S,
    report: &mut Reporter<S>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let mut buf = Vec::new();
    let total = entries.iter().map(|(_, d)| d.len() as u64).sum::<u64>();
    let mut done = 0u64;
    {
        let mut zip = zip::ZipWriter::new(Cursor::new(&mut buf));
        zip_entries(&mut zip, &entries, stage, report, &mut done, total).await?;
        let _ = zip.finish()?;
    }
    Ok(buf)
}

pub async fn unzip_report<S>(
    inner: Vec<u8>,
    stage: S,
    report: &mut Reporter<S>,
) -> Result<Vec<(String, Vec<u8>)>, Error>
where
    S: Copy + Send + Sync + 'static,
{
    let mut archive = zip::ZipArchive::new(Cursor::new(inner))?;
    let total = (0..archive.len())
        .map(|i| archive.by_index(i).map(|f| f.size()))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .sum::<u64>();
    let mut entries = Vec::new();
    let mut done = 0u64;
    for i in 0..archive.len() {
        let (name, size, data) = {
            let mut file = archive.by_index(i)?;
            let name = file.name().to_string();
            let size = file.size();
            let mut data =
                Vec::with_capacity(usize::try_from(size).map_err(|e| Error::Convert {
                    context: "zip entry size exceeds usize range",
                    source: e,
                })?);
            let _ = file.read_to_end(&mut data)?;
            (name, size, data)
        };
        done += size;
        entries.push((name.clone(), data));
        report(Job {
            stage,
            done,
            total,
            name: Some(name),
        });
        yield_to_paint().await;
    }
    Ok(entries)
}
