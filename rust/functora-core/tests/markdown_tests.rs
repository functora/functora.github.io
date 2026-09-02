use functora_core::markdown::render_markdown;

#[test]
fn markdown_keeps_external_images() {
    let out = render_markdown("![x](https://evil.example/track.png)");
    assert!(out.contains("<img"));
    assert!(out.contains("https://evil.example/track.png"));
}

#[test]
fn markdown_blocks_javascript_urls() {
    let out = render_markdown("[x](javascript:alert(1))");
    assert!(!out.contains("javascript:"));
}

#[test]
fn markdown_blocks_data_urls() {
    let out = render_markdown("[x](data:text/html,<script>alert(1)</script>)");
    assert!(!out.contains("data:"));
}

#[test]
fn markdown_adds_noreferrer_to_links() {
    let out = render_markdown("[x](https://example.com)");
    assert!(out.contains("rel=\"noopener noreferrer\""));
}

#[test]
fn markdown_strips_scripts_and_styles() {
    let out = render_markdown("<script>alert(1)</script><p style=\"background:url(x)\">hi</p>");
    assert!(!out.contains("<script"));
    assert!(!out.contains("style="));
}

#[test]
fn markdown_renders_tables_and_strikethrough() {
    let out = render_markdown("~~gone~~\n\n|a|b|\n|-|-|\n|1|2|");
    assert!(out.contains("<del>gone</del>"));
    assert!(out.contains("<table>"));
    assert!(out.contains("<td>1</td>"));
}
