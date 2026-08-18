use ammonia::Builder;
use pulldown_cmark::{Options, Parser, html};
use tap::Pipe;

#[must_use]
pub fn render_markdown(content: &str) -> String {
    Parser::new_ext(content, Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH)
        .pipe(|parser| {
            let mut html_out = String::new();
            html::push_html(&mut html_out, parser);
            html_out
        })
        .as_str()
        .pipe(|html_out| Builder::default().clean(html_out).to_string())
}
