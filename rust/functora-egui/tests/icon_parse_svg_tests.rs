//! SVG element parser behavior.

use functora_egui::icons::icon_element::IconElement;
use functora_egui::icons::parse_svg::parse_svg;

#[test]
fn parse_path_element() {
    let elems = parse_svg(r#"<path d="M 10 20 L 30 40" />"#);
    assert_eq!(elems.len(), 1);
    assert!(matches!(elems[0], IconElement::Path(_)));
}

#[test]
fn parse_circle_element() {
    let elems = parse_svg(r#"<circle cx="12" cy="13" r="8" />"#);
    assert_eq!(elems.len(), 1);
    if let IconElement::Circle { cx, cy, r } = &elems[0] {
        assert!((*cx - 12.0).abs() < f32::EPSILON);
        assert!((*cy - 13.0).abs() < f32::EPSILON);
        assert!((*r - 8.0).abs() < f32::EPSILON);
    } else {
        panic!("Expected Circle");
    }
}

#[test]
fn parse_rect_element() {
    let elems = parse_svg(r#"<rect width="18" height="18" x="3" y="3" rx="2" />"#);
    assert_eq!(elems.len(), 1);
}

#[test]
fn parse_mixed_elements() {
    let svg = r#"<circle cx="12" cy="13" r="8" /> <path d="M 0 0 L 1 1" /> <line x1="1" y1="2" x2="3" y2="4" />"#;
    let elems = parse_svg(svg);
    assert_eq!(elems.len(), 3);
}

#[test]
fn parse_polyline_element() {
    let elems = parse_svg(r#"<polyline points="11 3 11 11 14 8 17 11 17 3" />"#);
    assert_eq!(elems.len(), 1);
    if let IconElement::Polyline(pts) = &elems[0] {
        assert_eq!(pts.len(), 5);
    } else {
        panic!("Expected Polyline");
    }
}

#[test]
fn parse_polygon_element() {
    let elems = parse_svg(r#"<polygon points="12 2 22 8.5 22 15.5 12 22 2 15.5 2 8.5 12 2" />"#);
    assert_eq!(elems.len(), 1);
    if let IconElement::Polygon(pts) = &elems[0] {
        assert_eq!(pts.len(), 7);
    } else {
        panic!("Expected Polygon");
    }
}

#[test]
fn parse_ellipse_element() {
    let elems = parse_svg(r#"<ellipse cx="12" cy="19" rx="9" ry="3" />"#);
    assert_eq!(elems.len(), 1);
}
