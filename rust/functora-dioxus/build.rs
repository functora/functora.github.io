use std::fmt::Write;
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, thiserror::Error)]
enum BuildError {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Fmt(#[from] std::fmt::Error),
    #[error(transparent)]
    Var(#[from] std::env::VarError),
    #[error(transparent)]
    Clock(#[from] std::time::SystemTimeError),
    #[error(transparent)]
    Convert(#[from] std::num::TryFromIntError),
}

fn main() -> Result<(), BuildError> {
    emit_build_date()?;
    let methods = isolang::languages()
        .map(|lang| {
            let code = lang.to_639_3();
            let variant = capitalize(code);
            let method = format!("render_{code}");
            (variant, method)
        })
        .collect::<Vec<_>>();

    let out_dir = std::env::var("OUT_DIR")?;
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

    fs::write(&path, &code)?;
    println!("cargo:rerun-if-changed=build.rs");
    Ok(())
}

fn emit_build_date() -> Result<(), BuildError> {
    let secs = SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs();
    let (y, m, d) = civil_from_days(i64::try_from(secs / 86_400)?);
    println!("cargo:rustc-env=FUNCTORA_DIOXUS_YEAR={y:04}");
    println!("cargo:rustc-env=FUNCTORA_DIOXUS_DATE={y:04}-{m:02}-{d:02}");
    Ok(())
}

fn civil_from_days(z0: i64) -> (i64, i64, i64) {
    let z = z0 + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    (y + i64::from(m <= 2), m, d)
}

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().to_string() + c.as_str(),
    }
}
