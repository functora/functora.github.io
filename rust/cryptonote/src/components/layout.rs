use crate::i18n::Language;
use crate::{Route, Screen};
use derive_more::Display;
use dioxus::document::EvalError;
use dioxus::prelude::*;
use either::*;
use enum_iterator::{Sequence, next_cycle};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

#[derive(
    Debug,
    Clone,
    PartialEq,
    Sequence,
    Display,
    Serialize,
    Deserialize,
)]
pub enum Theme {
    Light,
    Dark,
}

async fn jsfun<
    A: Serialize + 'static,
    B: DeserializeOwned + 'static,
    C,
>(
    arg: A,
    out: fn(Result<B, EvalError>) -> C,
    fun: &'static str,
) {
    let code = &format!(
        r#"
        let arg = await dioxus.recv();
        try {{
            let res = await (async {fun})(arg);
            dioxus.send({{"Right": res}});
        }} catch (e) {{
            dioxus.send({{"Left": String(e)}});
        }}
        "#
    );

    let mut eval = document::eval(code);

    let res = match eval.send(arg) {
        Ok(()) => eval
            .recv::<Either<String, B>>()
            .await
            .and_then(|res| match res {
                Either::Right(rhs) => Ok(rhs),
                Either::Left(lhs) => {
                    Err(EvalError::InvalidJs(lhs))
                }
            }),
        Err(e) => Err(e),
    };

    out(res);
}

#[component]
pub fn Layout() -> Element {
    let mut theme = use_signal(|| Theme::Light);
    let mut language = use_context::<Signal<Language>>();
    let mut app_context =
        use_context::<Signal<crate::AppContext>>();
    let nav = use_navigator();

    let mut nav_state =
        use_context::<Signal<crate::NavigationState>>();

    use_effect(move || {
        let _ = spawn(jsfun(
            (*theme.read()).to_string().to_lowercase(),
            |res: Result<(), _>| {
                tracing::debug!("{:#?}", res)
            },
            r#"function(arg){
              window
                .document
                .documentElement
                .setAttribute("data-theme", arg);
            }"#,
        ));
    });

    rsx! {
        nav {
            label {
                input { r#type: "checkbox" }
                header {
                    a {
                        href: "#",
                        onclick: move |evt| {
                            evt.prevent_default();
                            app_context.set(crate::AppContext::default());
                            nav.push(Screen::Home.to_route(None));
                        },
                        "🔐 Cryptonote"
                    }
                }

                ul {
                    li {
                        a { onclick: move |_| language.set(Language::English), "English" }
                    }
                    li {
                        a { onclick: move |_| language.set(Language::Spanish), "Español" }
                    }
                    li {
                        a { onclick: move |_| language.set(Language::Russian), "Русский" }
                    }
                    li {
                        a {
                            onclick: move |_| {
                                let prev = (*theme.read()).clone();
                                theme.set(next_cycle(&prev))
                            },
                            "Theme"
                        }
                    }
                }
            }
        }

        Outlet::<Route> {}

        p { "txt": "c",
            {crate::i18n::get_translations(language()).copyright}
            " 2025 "
            a { href: "https://functora.github.io/", "Functora" }
            ". "
            {crate::i18n::get_translations(language()).all_rights_reserved}
            " "
            {crate::i18n::get_translations(language()).by_continuing}
            " "
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_state.write().has_navigated = true;
                    nav.push(Screen::License.to_route(None));
                },
                "{crate::i18n::get_translations(language()).terms_of_service}"
            }
            " "
            {crate::i18n::get_translations(language()).you_agree}
            " "
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_state.write().has_navigated = true;
                    nav.push(Screen::Privacy.to_route(None));
                },
                "{crate::i18n::get_translations(language()).privacy_policy_and}"
            }
            ". "
            {crate::i18n::get_translations(language()).please}
            " "
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_state.write().has_navigated = true;
                    nav.push(Screen::Donate.to_route(None));
                },
                "{crate::i18n::get_translations(language()).donate_link}"
            }
            ". "
            {crate::i18n::get_translations(language()).version_label}
            " "
            {env!("CARGO_PKG_VERSION")}
            "."
        }

        br {}
    }
}
