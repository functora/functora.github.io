//! Procedural image fixtures for demos and tests (SVG, PNG, JPEG, GIF, WebP, BMP, ICO, QOI, Farbfeld, TIFF, PPM).

use std::sync::OnceLock;

const WIDTH: u32 = 48;
const HEIGHT: u32 = 48;
const CIRCLE_RADIUS_SQUARED: u32 = 121;
const CENTER: u32 = 24;
const CHECKER_CELL: u32 = 6;
const JPEG_QUALITY: u8 = 85;

const SAMPLE_SVG: &str = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"48\" height=\"48\" viewBox=\"0 0 48 48\"><rect width=\"48\" height=\"48\" fill=\"#f0f0f5\"/><path d=\"M0 0h24v12H12v12H0zM24 12h12v12H24zM12 24h12v12H12zM36 24h12v12H36zM0 36h12v12H0zM24 36h12v12H24z\" fill=\"#2d78c8\"/><circle cx=\"24\" cy=\"24\" r=\"11\" fill=\"#e65a3c\"/></svg>";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImageType {
    Svg,
    Png,
    Jpeg,
    Gif,
    Webp,
    Bmp,
    Ico,
    Qoi,
    Farbfeld,
    Tiff,
    Pnm,
}

impl ImageType {
    #[must_use]
    pub const fn extension(self) -> &'static str {
        match self {
            Self::Svg => "svg",
            Self::Png => "png",
            Self::Jpeg => "jpg",
            Self::Gif => "gif",
            Self::Webp => "webp",
            Self::Bmp => "bmp",
            Self::Ico => "ico",
            Self::Qoi => "qoi",
            Self::Farbfeld => "ff",
            Self::Tiff => "tiff",
            Self::Pnm => "ppm",
        }
    }
}

pub struct ImageBytes {
    pub name: &'static str,
    pub format: ImageType,
    pub bytes: Vec<u8>,
}

impl ImageBytes {
    #[must_use]
    pub fn uri(&self) -> String {
        format!("bytes://{}.{}", self.name, self.format.extension())
    }

    pub fn to_image(&self) -> egui::Image<'static> {
        egui::Image::from_bytes(self.uri(), self.bytes.clone())
    }
}

pub struct ImageSamples {
    pub svg: ImageBytes,
    pub png: ImageBytes,
    pub jpeg: ImageBytes,
    pub gif: ImageBytes,
    pub webp: ImageBytes,
    pub bmp: ImageBytes,
    pub ico: ImageBytes,
    pub qoi: ImageBytes,
    pub farbfeld: ImageBytes,
    pub tiff: ImageBytes,
    pub pnm: ImageBytes,
    pub transparent: ImageBytes,
}

fn sample(name: &'static str, format: ImageType, bytes: Vec<u8>) -> ImageBytes {
    ImageBytes {
        name,
        format,
        bytes,
    }
}

static SAMPLES: OnceLock<Result<ImageSamples, image::ImageError>> = OnceLock::new();

pub fn image_samples() -> Result<&'static ImageSamples, &'static image::ImageError> {
    SAMPLES.get_or_init(build_samples).as_ref()
}

fn rgb_channel(x: u32, y: u32) -> [u8; 3] {
    if x.abs_diff(CENTER).pow(2) + y.abs_diff(CENTER).pow(2) < CIRCLE_RADIUS_SQUARED {
        [230, 90, 60]
    } else if (x / CHECKER_CELL + y / CHECKER_CELL).is_multiple_of(2) {
        [45, 120, 200]
    } else {
        [240, 240, 245]
    }
}

fn rgba_channel(x: u32, y: u32) -> [u8; 4] {
    let rgb = rgb_channel(x, y);
    let alpha = if rgb == [240, 240, 245] { 0 } else { 255 };
    [rgb[0], rgb[1], rgb[2], alpha]
}

fn rgb_pixels() -> Vec<u8> {
    (0..HEIGHT)
        .flat_map(|y| (0..WIDTH).flat_map(move |x| rgb_channel(x, y)))
        .collect()
}

fn rgba_pixels() -> Vec<u8> {
    (0..HEIGHT)
        .flat_map(|y| (0..WIDTH).flat_map(move |x| rgba_channel(x, y)))
        .collect()
}

fn encode_png(rgb: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    use image::ImageEncoder as _;
    let mut out = Vec::new();
    image::codecs::png::PngEncoder::new(&mut out).write_image(
        rgb,
        WIDTH,
        HEIGHT,
        image::ExtendedColorType::Rgb8,
    )?;
    Ok(out)
}

fn encode_png_alpha(rgba: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    use image::ImageEncoder as _;
    let mut out = Vec::new();
    image::codecs::png::PngEncoder::new(&mut out).write_image(
        rgba,
        WIDTH,
        HEIGHT,
        image::ExtendedColorType::Rgba8,
    )?;
    Ok(out)
}

fn encode_jpeg(rgb: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    let mut out = Vec::new();
    image::codecs::jpeg::JpegEncoder::new_with_quality(&mut out, JPEG_QUALITY).encode(
        rgb,
        WIDTH,
        HEIGHT,
        image::ExtendedColorType::Rgb8,
    )?;
    Ok(out)
}

fn encode_gif(rgb: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    let mut out = Vec::new();
    image::codecs::gif::GifEncoder::new(&mut out).encode(
        rgb,
        WIDTH,
        HEIGHT,
        image::ExtendedColorType::Rgb8,
    )?;
    Ok(out)
}

fn encode_webp(rgb: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    let mut out = Vec::new();
    image::codecs::webp::WebPEncoder::new_lossless(&mut out).encode(
        rgb,
        WIDTH,
        HEIGHT,
        image::ExtendedColorType::Rgb8,
    )?;
    Ok(out)
}

fn encode_bmp(rgb: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    let mut out = Vec::new();
    image::codecs::bmp::BmpEncoder::new(&mut out).encode(
        rgb,
        WIDTH,
        HEIGHT,
        image::ExtendedColorType::Rgb8,
    )?;
    Ok(out)
}

fn encode_ico(rgba: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    let frame =
        image::codecs::ico::IcoFrame::as_png(rgba, WIDTH, HEIGHT, image::ExtendedColorType::Rgba8)?;
    let mut out = Vec::new();
    image::codecs::ico::IcoEncoder::new(&mut out).encode_images(&[frame])?;
    Ok(out)
}

fn encode_qoi(rgb: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    use image::ImageEncoder as _;
    let mut out = Vec::new();
    image::codecs::qoi::QoiEncoder::new(&mut out).write_image(
        rgb,
        WIDTH,
        HEIGHT,
        image::ExtendedColorType::Rgb8,
    )?;
    Ok(out)
}

fn encode_farbfeld(rgba: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    let raw16: Vec<u8> = rgba
        .iter()
        .flat_map(|channel| (u16::from(*channel) * 257).to_ne_bytes())
        .collect();
    let mut out = Vec::new();
    image::codecs::farbfeld::FarbfeldEncoder::new(&mut out).encode(&raw16, WIDTH, HEIGHT)?;
    Ok(out)
}

fn encode_tiff(rgb: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    use image::ImageEncoder as _;
    use std::io::Cursor;
    let mut out = Cursor::new(Vec::new());
    image::codecs::tiff::TiffEncoder::new(&mut out).write_image(
        rgb,
        WIDTH,
        HEIGHT,
        image::ExtendedColorType::Rgb8,
    )?;
    Ok(out.into_inner())
}

fn encode_pnm(rgb: &[u8]) -> Result<Vec<u8>, image::ImageError> {
    use image::codecs::pnm::{PnmSubtype, SampleEncoding};
    let mut out = Vec::new();
    image::codecs::pnm::PnmEncoder::new(&mut out)
        .with_subtype(PnmSubtype::Pixmap(SampleEncoding::Binary))
        .encode(rgb, WIDTH, HEIGHT, image::ExtendedColorType::Rgb8)?;
    Ok(out)
}

fn build_samples() -> Result<ImageSamples, image::ImageError> {
    let rgb = rgb_pixels();
    let rgba = rgba_pixels();
    Ok(ImageSamples {
        svg: sample("sample-svg", ImageType::Svg, SAMPLE_SVG.as_bytes().to_vec()),
        png: sample("sample-png", ImageType::Png, encode_png(&rgb)?),
        jpeg: sample("sample-jpeg", ImageType::Jpeg, encode_jpeg(&rgb)?),
        gif: sample("sample-gif", ImageType::Gif, encode_gif(&rgb)?),
        webp: sample("sample-webp", ImageType::Webp, encode_webp(&rgb)?),
        bmp: sample("sample-bmp", ImageType::Bmp, encode_bmp(&rgb)?),
        ico: sample("sample-ico", ImageType::Ico, encode_ico(&rgba)?),
        qoi: sample("sample-qoi", ImageType::Qoi, encode_qoi(&rgb)?),
        farbfeld: sample(
            "sample-farbfeld",
            ImageType::Farbfeld,
            encode_farbfeld(&rgba)?,
        ),
        tiff: sample("sample-tiff", ImageType::Tiff, encode_tiff(&rgb)?),
        pnm: sample("sample-ppm", ImageType::Pnm, encode_pnm(&rgb)?),
        transparent: sample(
            "sample-transparent",
            ImageType::Png,
            encode_png_alpha(&rgba)?,
        ),
    })
}
