//! Minimal SVG inner-XML parser for Lucide icon data.

/// Parse inner SVG XML (the content inside `<svg>...</svg>`) into icon elements.
#[must_use]
pub fn parse_svg(svg_inner: &str) -> Vec<super::icon_element::IconElement> {
    let mut elements = Vec::new();

    // Split on '<' to find each tag
    for chunk in svg_inner.split('<') {
        let trimmed = chunk.trim();
        if trimmed.is_empty() {
            continue;
        }

        // Take everything up to '>' or '/>'
        let tag_content = trimmed.split('>').next().unwrap_or("");

        if let Some(elem) = parse_tag(tag_content) {
            elements.push(elem);
        }
    }

    elements
}

fn parse_tag(tag: &str) -> Option<super::icon_element::IconElement> {
    let trimmed_tag = tag.trim().trim_end_matches('/');
    let tag_name = trimmed_tag.split_ascii_whitespace().next()?;

    match tag_name {
        "path" => parse_path_tag(trimmed_tag),
        "circle" => Some(parse_circle_tag(trimmed_tag)),
        "rect" => Some(parse_rect_tag(trimmed_tag)),
        "line" => Some(parse_line_tag(trimmed_tag)),
        "polyline" => parse_polyline_tag(trimmed_tag),
        "polygon" => parse_polygon_tag(trimmed_tag),
        "ellipse" => Some(parse_ellipse_tag(trimmed_tag)),
        _ => None,
    }
}

fn parse_path_tag(tag: &str) -> Option<super::icon_element::IconElement> {
    let d = extract_attr(tag, "d")?;
    let commands = super::parse_path::parse_path_data(&d);
    Some(super::icon_element::IconElement::Path(commands))
}

fn parse_circle_tag(tag: &str) -> super::icon_element::IconElement {
    let cx = extract_attr(tag, "cx")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let cy = extract_attr(tag, "cy")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let r = extract_attr(tag, "r")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    super::icon_element::IconElement::Circle { cx, cy, r }
}

fn parse_rect_tag(tag: &str) -> super::icon_element::IconElement {
    let x = extract_attr(tag, "x")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let y = extract_attr(tag, "y")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let width = extract_attr(tag, "width")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let height = extract_attr(tag, "height")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let rx = extract_attr(tag, "rx")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    super::icon_element::IconElement::Rect {
        x,
        y,
        width,
        height,
        rx,
    }
}

fn parse_line_tag(tag: &str) -> super::icon_element::IconElement {
    let x1 = extract_attr(tag, "x1")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let y1 = extract_attr(tag, "y1")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let x2 = extract_attr(tag, "x2")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let y2 = extract_attr(tag, "y2")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    super::icon_element::IconElement::Line { x1, y1, x2, y2 }
}

fn parse_polyline_tag(tag: &str) -> Option<super::icon_element::IconElement> {
    let points = extract_attr(tag, "points")?;
    Some(super::icon_element::IconElement::Polyline(parse_points(
        &points,
    )))
}

fn parse_polygon_tag(tag: &str) -> Option<super::icon_element::IconElement> {
    let points = extract_attr(tag, "points")?;
    Some(super::icon_element::IconElement::Polygon(parse_points(
        &points,
    )))
}

fn parse_ellipse_tag(tag: &str) -> super::icon_element::IconElement {
    let cx = extract_attr(tag, "cx")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let cy = extract_attr(tag, "cy")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let rx = extract_attr(tag, "rx")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    let ry = extract_attr(tag, "ry")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0);
    super::icon_element::IconElement::Ellipse { cx, cy, rx, ry }
}

// ── Helpers ─────────────────────────────────────────────────────

/// Extract the value of `attr="value"` from a tag string.
fn extract_attr(tag: &str, attr_name: &str) -> Option<String> {
    // Look for `attr_name="` — we must match as a word boundary to
    // avoid e.g. "ry" matching inside "stroke-linejoin-ry".
    // We search for ` attr_name="` (space-prefixed) or the tag starts
    // with it after the tag name.
    let needle = format!(" {attr_name}=\"");
    let start = tag.find(&needle).map(|i| i + needle.len())?;
    let rest = &tag[start..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

/// Parse an SVG `points` attribute ("x1,y1 x2,y2 ...") into coordinate pairs.
fn parse_points(s: &str) -> Vec<(f32, f32)> {
    let nums: Vec<f32> = s
        .split(|c: char| c == ',' || c.is_ascii_whitespace())
        .filter(|item| !item.is_empty())
        .filter_map(|item| item.parse().ok())
        .collect();

    nums.chunks(2)
        .filter_map(|pair| {
            if pair.len() == 2 {
                Some((pair[0], pair[1]))
            } else {
                None
            }
        })
        .collect()
}
