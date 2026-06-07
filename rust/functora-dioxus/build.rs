#![allow(clippy::let_underscore_must_use)]

use std::fmt::Write;
use std::fs;

fn main() {
    let mut methods = Vec::new();

    for lang in isolang::languages() {
        let code = lang.to_639_3();
        let variant = capitalize(code);
        let method = format!("render_{code}");
        methods.push((variant, method));
    }

    let out_dir = std::env::var("OUT_DIR").unwrap_or_else(|_| "/tmp".into());
    let path = format!("{out_dir}/i18n_trait.rs");

    let mut defaulted = String::new();
    let mut dispatch = String::new();

    for (variant, method) in &methods {
        if method != "render_eng" {
            let _ = writeln!(defaulted, "    fn {method}(&self) -> String {{ self.render_eng() }}");
        }
        let _ = writeln!(dispatch, "            Language::{variant} => self.{method}(),");
    }

    let code = format!(
        "pub trait I18N {{\n\
             fn render_eng(&self) -> String;\n\n\
             {defaulted}\n\
             fn render(&self, lang: Language) -> String {{\n\
                 match lang {{\n\
                     {dispatch}\
                 }}\n\
             }}\n\
         }}\n"
    );

    fs::write(&path, &code).unwrap_or_else(|e| panic!("Failed to write {path}: {e}"));
    println!("cargo:rerun-if-changed=build.rs");
}

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().to_string() + c.as_str(),
    }
}
