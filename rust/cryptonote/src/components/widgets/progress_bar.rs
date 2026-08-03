use crate::messages::Msg;
use crate::progress::Stage;
use crate::*;

#[component]
pub fn ProgressBar() -> Element {
    let tst = use_context::<Store<TemporaryState>>();
    let Some(job) = tst.progress()() else {
        return rsx! {};
    };
    let lang = use_lang();
    let operation = match job.stage {
        Stage::Attach => Msg::StageAttach,
        Stage::Zip => Msg::StageZip,
        Stage::Encrypt => Msg::StageEncrypt,
        Stage::Decrypt => Msg::StageDecrypt,
        Stage::Unzip => Msg::StageUnzip,
        Stage::Download => Msg::StageDownload,
    };
    rsx! {
        fieldset { "aria-live": "polite", role: "status",
            card {
                label { "{operation.render(lang)}" }
                if let Some(name) = &job.name {
                    small { "{name}" }
                }
                progress { max: "100", value: "{job.percent()}" }
                output { "{job.percent()}%" }
            }
        }
    }
}
