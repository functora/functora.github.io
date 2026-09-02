#[must_use]
pub struct Navbar<'a> {
    pub(crate) brand: &'a str,
    pub(crate) version: Option<&'a str>,
    pub(crate) search_label: Option<&'a str>,
    pub(crate) search_shortcut: Option<&'a str>,
}

impl<'a> Navbar<'a> {
    pub fn new(brand: &'a str) -> Self {
        Self {
            brand,
            version: None,
            search_label: None,
            search_shortcut: None,
        }
    }

    pub fn version(mut self, version: &'a str) -> Self {
        self.version = Some(version);
        self
    }

    pub fn search(mut self, label: &'a str, shortcut: Option<&'a str>) -> Self {
        self.search_label = Some(label);
        self.search_shortcut = shortcut;
        self
    }
}
