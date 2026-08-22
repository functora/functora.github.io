use askama::Template;

#[derive(Template)]
#[template(path = "web/index.html")]
pub struct IndexHtml<'a> {
    pub title: &'a str,
    pub theme_color: &'a str,
    pub pkg_js: &'a str,
    pub vsn: &'a str,
}

#[derive(Template)]
#[template(path = "web/manifest.json", escape = "none")]
pub struct ManifestJson<'a> {
    pub title: &'a str,
    pub short_name: &'a str,
    pub theme_color: &'a str,
}
