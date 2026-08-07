#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote::render_markdown;

#[test]
fn empty_input_returns_empty() {
    assert_eq!(render_markdown(""), "");
}

#[test]
fn plain_text_wraps_in_paragraph() {
    let html = render_markdown("Hello");
    assert_eq!(html, "<p>Hello</p>\n");
}

#[test]
fn bold_text_renders_as_strong() {
    let html = render_markdown("**bold**");
    assert_eq!(html, "<p><strong>bold</strong></p>\n");
}

#[test]
fn italic_text_renders_as_emphasis() {
    let html = render_markdown("*italic*");
    assert_eq!(html, "<p><em>italic</em></p>\n");
}

#[test]
fn strikethrough_is_enabled() {
    let html = render_markdown("~~strike~~");
    assert_eq!(html, "<p><del>strike</del></p>\n");
}

#[test]
fn tables_are_enabled() {
    let html = render_markdown("| a | b |\n|---|---|\n| 1 | 2 |");
    assert!(html.contains("<table>"));
    assert!(html.contains("<th>a</th>"));
}

#[test]
fn html_tags_are_removed() {
    let html = render_markdown("<script>alert('xss')</script>");
    assert!(!html.contains("script"));
    assert!(!html.contains("alert"));
}

#[test]
fn unsafe_attributes_are_stripped() {
    let html = render_markdown("<a href=\"javascript:alert(1)\">click</a>");
    assert!(html.contains("click"));
    assert!(!html.contains("javascript"));
}

#[test]
fn multiple_paragraphs() {
    let html = render_markdown("para1\n\npara2");
    assert!(html.contains("<p>para1</p>\n<p>para2</p>"));
}

#[test]
fn unordered_list() {
    let html = render_markdown("- item1\n- item2");
    assert!(html.contains("<ul>"));
    assert!(html.contains("<li>item1</li>"));
}

#[test]
fn ordered_list() {
    let html = render_markdown("1. first\n2. second");
    assert!(html.contains("<ol>"));
}

#[test]
fn code_block() {
    let html = render_markdown("```\nlet x = 1;\n```");
    assert!(html.contains("<code>"));
}

#[test]
fn unicode_content_preserved() {
    let html = render_markdown("Привет мир! ¡Hola! こんにちは");
    assert!(html.contains("Привет"));
    assert!(html.contains("Hola"));
    assert!(html.contains("こんにちは"));
}

#[test]
fn inline_code_renders() {
    let html = render_markdown("use `render_markdown`");
    assert!(html.contains("<code>render_markdown</code>"));
}

#[test]
fn safe_links_are_preserved() {
    let html = render_markdown("[link](https://safe.com)");
    assert!(html.contains("safe.com"));
}
