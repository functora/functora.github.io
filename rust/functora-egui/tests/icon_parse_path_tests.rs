//! SVG path `d`-attribute parser behavior.

use functora_egui::icons::parse_path::parse_path_data;

#[test]
fn parse_simple_move_line() {
    let cmds = parse_path_data("M 10 20 L 30 40");
    assert_eq!(cmds.len(), 2);
}

#[test]
fn parse_implicit_lineto_after_move() {
    // "M 0 0 10 10" should produce MoveTo + LineTo
    let cmds = parse_path_data("M 0 0 10 10");
    assert_eq!(cmds.len(), 2);
}

#[test]
fn parse_close() {
    let cmds = parse_path_data("M 0 0 L 10 0 L 10 10 Z");
    assert_eq!(cmds.len(), 4);
}

#[test]
fn parse_cubic() {
    let cmds = parse_path_data("M 0 0 C 1 2 3 4 5 6");
    assert_eq!(cmds.len(), 2);
}

#[test]
fn parse_arc() {
    let cmds = parse_path_data("M 0 0 A 10 10 0 0 1 20 20");
    assert_eq!(cmds.len(), 2);
}
