use askama::Template;

#[derive(Template)]
#[template(path = "desktop/app.desktop", escape = "none", ext = "txt")]
pub struct LinuxDesktop<'a> {
    pub title: &'a str,
    pub comment: &'a str,
    pub exec_name: &'a str,
    pub icon_name: &'a str,
    pub categories: &'a str,
    pub mime_types: &'a str,
    pub schemes: &'a str,
}

#[derive(Template)]
#[template(path = "desktop/metainfo.xml", escape = "none", ext = "xml")]
pub struct Metainfo<'a> {
    pub app_id: &'a str,
    pub title: &'a str,
    pub comment: &'a str,
    pub description: &'a str,
    pub version: &'a str,
    pub date: &'a str,
    pub homepage: &'a str,
    pub developer: &'a str,
    pub developer_id: &'a str,
}
