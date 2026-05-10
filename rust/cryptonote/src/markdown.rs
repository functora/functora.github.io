use ammonia::clean;
use pulldown_cmark::{html, Options, Parser};
use tap::Pipe;

pub fn render_markdown(content: &str) -> String {
    let mut opts = Options::empty();
    opts.insert(Options::ENABLE_TABLES);
    opts.insert(Options::ENABLE_STRIKETHROUGH);
    Parser::new_ext(content, opts)
        .pipe(|parser| {
            let mut html_out = String::new();
            html::push_html(&mut html_out, parser);
            html_out
        })
        .as_str()
        .pipe(clean)
}
