use crate::dioxus_elements;
use crate::i18n::{I18N, Language};
use crate::progress::Job;
use dioxus::prelude::*;
use dioxus_core::Callback;

#[component]
pub fn JobProgressBar<S, U>(job: Option<Job<S>>, stage_label: Callback<S, U>, lang: Language) -> Element
where
    S: Copy + PartialEq + 'static,
    U: I18N + Clone + 'static,
{
    let Some(job) = job else {
        return rsx! {};
    };
    rsx! {
        fieldset { "aria-live": "polite", role: "status",
            card {
                label { "{stage_label.call(job.stage).render(lang)}" }
                if let Some(name) = &job.name {
                    small { "{name}" }
                }
                progress { max: "100", value: "{job.percent()}" }
                output { "{job.percent()}%" }
            }
        }
    }
}
