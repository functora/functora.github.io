use crate::messages::Msg;

#[derive(Clone, Debug, PartialEq)]
pub struct CryptoBlock {
    pub label: String,
    pub address: String,
}

const BTC_ADDRESS: &str = "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q";
const XMR_ADDRESS: &str = "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG";

#[must_use]
pub fn donate_blocks() -> Vec<CryptoBlock> {
    vec![
        CryptoBlock {
            label: "BTC - Bitcoin".to_string(),
            address: BTC_ADDRESS.to_string(),
        },
        CryptoBlock {
            label: "XMR - Monero".to_string(),
            address: XMR_ADDRESS.to_string(),
        },
    ]
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhiteLabelContent<M = Msg> {
    pub license_text: Option<M>,
    pub privacy_text: Option<M>,
    pub donate_greeting: Option<M>,
    pub donate_intro: Option<M>,
    pub donate_blocks: Vec<CryptoBlock>,
}

impl<M> Default for WhiteLabelContent<M> {
    fn default() -> Self {
        Self {
            license_text: None,
            privacy_text: None,
            donate_greeting: None,
            donate_intro: None,
            donate_blocks: donate_blocks(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AppAttrs {
    pub app: &'static str,
    pub vsn: &'static str,
    pub org: &'static str,
    pub src: Option<&'static str>,
    pub dst: &'static str,
    pub description: &'static str,
}

pub(crate) fn capitalize_first(s: &str) -> String {
    let end = s.chars().next().map_or(0, char::len_utf8);
    format!("{}{}", s[..end].to_uppercase(), &s[end..])
}

impl AppAttrs {
    #[must_use]
    pub fn app_name(self) -> String {
        capitalize_first(self.app)
    }

    #[must_use]
    pub fn cache_name(self) -> String {
        format!("{}-v{}", self.app, self.vsn)
    }

    #[must_use]
    pub fn manifest_uri(self, icon_192: &str, icon_512: &str) -> Option<String> {
        #[cfg(target_arch = "wasm32")]
        {
            let loc = web_sys::window()?.location();
            let protocol = loc.protocol().ok()?;
            let host = loc.host().ok()?;
            let pathname = loc.pathname().ok()?;
            let origin_base = format!("{protocol}//{host}");
            let app_root = format!("{origin_base}{pathname}");
            let json = manifest_json(
                self.app,
                self.vsn,
                self.description,
                &app_root,
                &app_root,
                &[
                    ManifestIcon {
                        src: format!("{origin_base}{icon_192}"),
                        sizes: "192x192",
                        r#type: "image/png",
                        purpose: "any",
                    },
                    ManifestIcon {
                        src: format!("{origin_base}{icon_512}"),
                        sizes: "512x512",
                        r#type: "image/png",
                        purpose: "any",
                    },
                ],
            );
            Some(format!(
                "data:application/manifest+json,{}",
                urlencoding::encode(&json)
            ))
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = (icon_192, icon_512);
            None
        }
    }

    #[must_use]
    pub fn pages_url(self) -> String {
        format!("https://{}.github.io", self.org)
    }

    #[must_use]
    pub fn author_url(self) -> String {
        format!("{}/", self.pages_url())
    }

    #[must_use]
    pub fn app_url(self) -> String {
        format!("https://{}.github.io/{}/{}", self.org, self.dst, self.app)
    }

    #[must_use]
    pub fn origin(self) -> String {
        #[cfg(target_arch = "wasm32")]
        {
            web_sys::window()
                .and_then(|w| {
                    let loc = w.location();
                    let protocol = loc.protocol().ok()?;
                    let host = loc.host().ok()?;
                    let pathname = loc.pathname().ok()?;
                    let path = pathname.trim_end_matches('/');
                    Some(format!("{}//{}{}", protocol, host, path))
                })
                .unwrap_or_else(|| self.app_url())
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            self.app_url()
        }
    }

    #[must_use]
    pub fn source_url(self) -> String {
        self.src.map_or_else(
            || format!("https://github.com/{}/{}.github.io", self.org, self.org),
            |src| {
                format!(
                    "https://github.com/{}/{}.github.io/tree/master/{src}/{}",
                    self.org, self.org, self.app
                )
            },
        )
    }

    #[must_use]
    pub fn apk_url(self) -> String {
        format!(
            "https://github.com/{}/{}.github.io/releases/tag/{}-v{}",
            self.org, self.org, self.app, self.vsn
        )
    }

    #[must_use]
    pub fn google_play_url(self) -> String {
        format!(
            "https://play.google.com/store/apps/details?id=com.{}.{}",
            self.org, self.app
        )
    }

    #[must_use]
    pub fn beta_url(self) -> String {
        format!("https://groups.google.com/g/{}", self.org)
    }

    #[must_use]
    pub fn share_anchor_id(self) -> String {
        let hash = self.app.bytes().fold(0u32, |acc, byte| {
            acc.wrapping_mul(31).wrapping_add(u32::from(byte))
        });
        format!("sh-{hash:08x}")
    }
}

pub struct ManifestIcon {
    pub src: String,
    pub sizes: &'static str,
    pub r#type: &'static str,
    pub purpose: &'static str,
}

#[must_use]
pub fn manifest_json(
    app: &str,
    vsn: &str,
    description: &str,
    start_url: &str,
    scope: &str,
    icons: &[ManifestIcon],
) -> String {
    let name = capitalize_first(app);
    let icons_json = icons
        .iter()
        .map(|icon| {
            format!(
                "{{\"src\":{},\"sizes\":\"{}\",\"type\":\"{}\",\"purpose\":\"{}\"}}",
                json_str(&icon.src),
                icon.sizes,
                icon.r#type,
                icon.purpose
            )
        })
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "{{\"name\":{},\"short_name\":{},\"description\":{},\"start_url\":{},\"scope\":{},\"display\":\"standalone\",\"theme_color\":\"#679\",\"background_color\":\"#ffffff\",\"cache_name\":{},\"icons\":[{icons_json}]}}",
        json_str(&name),
        json_str(&name),
        json_str(description),
        json_str(start_url),
        json_str(scope),
        json_str(&format!("{app}-v{vsn}")),
    )
}

fn json_str(s: &str) -> String {
    serde_json::to_string(s).unwrap_or_default()
}
