use egui::load::Bytes;
use egui::load::BytesLoadResult;
use egui::load::BytesLoader;
use egui::load::BytesPoll;
use egui::load::LoadError;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone)]
struct Entry {
    bytes: Arc<[u8]>,
    mime: Option<String>,
}

#[derive(Default)]
pub struct WasmSafeDataUrlLoader {
    cache: Arc<egui::mutex::Mutex<HashMap<String, Result<Entry, String>>>>,
}

impl WasmSafeDataUrlLoader {
    pub const ID: &'static str = egui::generate_loader_id!(WasmSafeDataUrlLoader);
}

fn decode(uri: &str) -> Result<Entry, LoadError> {
    let url = data_url::DataUrl::process(uri).map_err(|_| LoadError::NotSupported)?;
    let decoded = url
        .decode_to_vec()
        .map_err(|e| LoadError::Loading(e.to_string()))?;
    let mime = url.mime_type().to_string();
    Ok(Entry {
        bytes: Arc::from(decoded.0),
        mime: (!mime.is_empty()).then_some(mime),
    })
}

fn ready(entry: &Entry) -> BytesPoll {
    BytesPoll::Ready {
        size: None,
        bytes: Bytes::Shared(Arc::clone(&entry.bytes)),
        mime: entry.mime.clone(),
    }
}

impl BytesLoader for WasmSafeDataUrlLoader {
    fn id(&self) -> &str {
        Self::ID
    }

    fn load(&self, _ctx: &egui::Context, uri: &str) -> BytesLoadResult {
        let cached = self.cache.lock().get(uri).cloned();
        cached.map_or_else(
            || match decode(uri) {
                Ok(entry) => {
                    let poll = ready(&entry);
                    let _ = self.cache.lock().insert(uri.to_owned(), Ok(entry));
                    Ok(poll)
                }
                Err(LoadError::Loading(msg)) => {
                    let _ = self.cache.lock().insert(uri.to_owned(), Err(msg.clone()));
                    Err(LoadError::Loading(msg))
                }
                Err(other) => Err(other),
            },
            |hit| hit.map(|entry| ready(&entry)).map_err(LoadError::Loading),
        )
    }

    fn forget(&self, uri: &str) {
        let _ = self.cache.lock().remove(uri);
    }

    fn forget_all(&self) {
        self.cache.lock().clear();
    }

    fn byte_size(&self) -> usize {
        self.cache
            .lock()
            .values()
            .map(|cached| match cached {
                Ok(entry) => entry.bytes.len() + entry.mime.as_ref().map_or(0, String::len),
                Err(err) => err.len(),
            })
            .sum()
    }
}

pub fn install_data_url_loader(ctx: &egui::Context) {
    if !ctx.is_loader_installed(WasmSafeDataUrlLoader::ID) {
        ctx.add_bytes_loader(Arc::new(WasmSafeDataUrlLoader::default()));
    }
}
