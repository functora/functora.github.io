use crate::error::Error;

pub async fn read() -> Result<String, Error> {
    crate::platform::backend::clipboard_read().await
}

pub async fn write(text: String) -> Result<(), Error> {
    crate::platform::backend::clipboard_write(text).await
}

#[cfg(all(
    not(any(target_arch = "wasm32", target_os = "android")),
    feature = "images"
))]
pub async fn read_image() -> Result<Option<crate::paste_image::PasteImage>, Error> {
    std::future::ready(()).await;
    match arboard::Clipboard::new() {
        Ok(mut clipboard) => read_image_from(&mut clipboard),
        Err(e) => {
            tracing::warn!("Clipboard unavailable for image paste: {e}");
            Ok(None)
        }
    }
}

#[cfg(all(
    not(any(target_arch = "wasm32", target_os = "android")),
    feature = "images"
))]
fn read_image_from(
    clipboard: &mut arboard::Clipboard,
) -> Result<Option<crate::paste_image::PasteImage>, Error> {
    if clipboard.get_text().is_ok_and(|text| !text.is_empty()) {
        Ok(None)
    } else {
        match clipboard.get_image() {
            Ok(image) => {
                let data = rgba_to_png(image.width, image.height, image.bytes.as_ref())?;
                Ok(Some(crate::paste_image::PasteImage {
                    name: format!("pasted-{}.png", unix_ms()),
                    data,
                }))
            }
            Err(arboard::Error::ContentNotAvailable) => Ok(None),
            Err(e) => Err(Error::JS(format!("Clipboard image read: {e}"))),
        }
    }
}

#[cfg(all(
    not(any(target_arch = "wasm32", target_os = "android")),
    feature = "images"
))]
fn unix_ms() -> u64 {
    use std::time::UNIX_EPOCH;
    std::time::SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| u64::try_from(d.as_millis()).unwrap_or(u64::MAX))
}

#[cfg(all(
    not(any(target_arch = "wasm32", target_os = "android")),
    feature = "images"
))]
pub fn rgba_to_png(width: usize, height: usize, rgba: &[u8]) -> Result<Vec<u8>, Error> {
    use image::ImageEncoder as _;
    let width_u32 = u32::try_from(width).map_err(|source| Error::Convert {
        context: "clipboard image width",
        source,
    })?;
    let height_u32 = u32::try_from(height).map_err(|source| Error::Convert {
        context: "clipboard image height",
        source,
    })?;
    let expected = u64::from(width_u32)
        .saturating_mul(u64::from(height_u32))
        .saturating_mul(4);
    if u64::try_from(rgba.len()).unwrap_or(u64::MAX) != expected {
        return Err(Error::InvalidFormat(format!(
            "clipboard image: expected {expected} RGBA bytes, got {}",
            rgba.len()
        )));
    }
    let mut out = Vec::new();
    image::codecs::png::PngEncoder::new(&mut out)
        .write_image(rgba, width_u32, height_u32, image::ExtendedColorType::Rgba8)
        .map_err(|e| Error::JS(format!("Clipboard image encode: {e}")))?;
    Ok(out)
}
