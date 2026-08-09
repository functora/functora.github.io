use std::fmt::Write;
use std::fs;

fn main() -> Result<(), std::fmt::Error> {
    let methods = isolang::languages()
        .map(|lang| {
            let code = lang.to_639_3();
            let variant = capitalize(code);
            let method = format!("render_{code}");
            (variant, method)
        })
        .collect::<Vec<_>>();

    let out_dir = std::env::var("OUT_DIR").unwrap_or_else(|_| "/tmp".into());
    let path = format!("{out_dir}/i18n_trait.rs");

    let defaulted = methods
        .iter()
        .filter(|(_, method)| method != "render_eng")
        .try_fold(String::new(), |mut acc, (_, method)| {
            writeln!(acc, "    fn {method}(&self) -> String {{ self.render_eng() }}").map(|()| acc)
        })?;
    let dispatch = methods.iter().try_fold(String::new(), |mut acc, (variant, method)| {
        writeln!(acc, "            Language::{variant} => self.{method}(),").map(|()| acc)
    })?;

    let code = format!(
        "pub trait I18N {{\n\
             fn render_eng(&self) -> String;\n\n\
             {defaulted}\n\
             fn render(&self, lang: Language) -> String {{\n\
                 match lang {{\n\
                     {dispatch}\
                 }}\n\
             }}\n\
             fn render_markdown(&self, lang: Language) -> String {{\n\
                 crate::markdown::render_markdown(&self.render(lang))\n\
             }}\n\
         }}\n"
    );

    fs::write(&path, &code).unwrap_or_else(|e| panic!("Failed to write {path}: {e}"));
    println!("cargo:rerun-if-changed=build.rs");
    Ok(())
}

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().to_string() + c.as_str(),
    }
}
