use crate::messages::Msg;

#[derive(Clone, Debug, PartialEq)]
pub struct CryptoBlock {
    pub label: String,
    pub address: String,
}

/// Default Functora organization used by the white-label preset.
pub const FUNCTORA_ORG: &str = "functora";
/// Default contact email used by the white-label preset (privacy policy, about).
pub const FUNCTORA_EMAIL: &str = "functora@proton.me";

pub const BTC_ADDRESS: &str = "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q";
pub const XMR_ADDRESS: &str = "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG";

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
    pub fn app_version_url(self) -> String {
        format!("{}/{}", self.app_url(), self.vsn)
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
            self.app_version_url()
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

/// Default Functora organization attributes: reusable preset for Functora apps.
/// Supply the app name, version and description; org, repo layout and contacts
/// come from the Functora preset. Other users construct their own `AppAttrs`.
#[must_use]
pub const fn functora_attrs(
    app: &'static str,
    vsn: &'static str,
    description: &'static str,
) -> AppAttrs {
    AppAttrs {
        app,
        vsn,
        org: FUNCTORA_ORG,
        src: Some("rust"),
        dst: "apps",
        description,
    }
}

/// Default crypto donation blocks used by the white-label preset.
#[must_use]
pub fn default_donate_blocks() -> Vec<CryptoBlock> {
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

use crate::i18n::Language;
use crate::{FUNCTORA_CORE_DATE, FUNCTORA_CORE_YEAR};

/// Default MIT license text derived from `AppAttrs` (org + year), i18n EN/ES/RU.
#[must_use]
pub fn default_license_text(attrs: &AppAttrs, lang: Language) -> String {
    match lang {
        Language::Eng => format!(
            r#"The MIT License (MIT)

Copyright (c) {FUNCTORA_CORE_YEAR} {org}

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."#,
            org = attrs.org,
        ),
        Language::Spa => format!(
            r#"La Licencia MIT (MIT)

Copyright (c) {FUNCTORA_CORE_YEAR} {org}

Por la presente se concede permiso, libre de cargos, a cualquier persona que haya obtenido una copia de este software y archivos de documentación asociados (el "Software"), para utilizar el Software sin restricción, incluyendo sin limitación los derechos a usar, copiar, modificar, fusionar, publicar, distribuir, sublicenciar y/o vender copias del Software, y a permitir a las personas a las que se les proporcione el Software que hagan lo mismo, sujeto a las siguientes condiciones:

El aviso de copyright anterior y este aviso de permiso deberán incluirse en todas las copias o partes sustanciales del Software.

EL SOFTWARE SE PROPORCIONA "TAL CUAL", SIN GARANTÍA DE NINGÚN TIPO, EXPRESA O IMPLÍCITA, INCLUYENDO PERO NO LIMITADO A LAS GARANTÍAS DE COMERCIABILIDAD, IDONEIDAD PARA UN FIN PARTICULAR Y NO INFRACCIÓN. EN NINGÚN CASO LOS AUTORES O TITULARES DEL COPYRIGHT SERÁN RESPONSABLES DE NINGUNA RECLAMACIÓN, DAÑOS U OTRAS RESPONSABILIDADES, YA SEA EN UNA ACCIÓN DE CONTRATO, AGRAVIO O DE OTRO TIPO, QUE SURJA DE, O EN RELACIÓN CON EL SOFTWARE O EL USO U OTROS TRATOS EN EL SOFTWARE."#,
            org = attrs.org,
        ),
        Language::Rus => format!(
            r"The MIT License (MIT)

Copyright (c) {FUNCTORA_CORE_YEAR} {org}

Настоящим предоставляется бесплатное разрешение любому лицу, получившему копию данного программного обеспечения и сопутствующих файлов документации (далее - «Программное обеспечение»), использовать Программное обеспечение без ограничений, включая неограниченное право использовать, копировать, изменять, объединять, публиковать, распространять, сублицензировать и/или продавать копии Программного обеспечения, а также разрешать лицам, которым предоставлено Программное обеспечение, делать то же самое, при соблюдении следующих условий:

Указанное выше уведомление об авторских правах и данное уведомление о разрешении должны быть включены во все копии или существенные части Программного обеспечения.

ПРОГРАММНОЕ ОБЕСПЕЧЕНИЕ ПРЕДОСТАВЛЯЕТСЯ «КАК ЕСТЬ», БЕЗ КАКИХ-ЛИБО ГАРАНТИЙ, ЯВНО ВЫРАЖЕННЫХ ИЛИ ПОДРАЗУМЕВАЕМЫХ, ВКЛЮЧАЯ, НО НЕ ОГРАНИЧИВАЯСЬ ГАРАНТИЯМИ ТОВАРНОГО СОСТОЯНИЯ, ПРИГОДНОСТИ ДЛЯ КОНКРЕТНЫХ ЦЕЛЕЙ И ОТСУТСТВИЯ НАРУШЕНИЙ АВТОРСКИХ ПРАВ. НИ ПРИ КАКИХ ОБСТОЯТЕЛЬСТВАХ АВТОРЫ ИЛИ ПРАВООБЛАДАТЕЛИ НЕ НЕСУТ ОТВЕТСТВЕННОСТИ ПО ЛЮБЫМ ПРЕТЕНЗИЯМ, ЗА УБЫТКИ ИЛИ ДРУГИЕ ТРЕБОВАНИЯ, ВЫТЕКАЮЩИЕ ИЗ ДОГОВОРА, ДЕЛИКТА ИЛИ ИНЫХ ОБСТОЯТЕЛЬСТВ, СВЯЗАННЫЕ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ, ЕГО ИСПОЛЬЗОВАНИЕМ ИЛИ ДРУГИМИ ДЕЙСТВИЯМИ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ.",
            org = attrs.org,
        ),
        _ => default_license_text(attrs, Language::Eng),
    }
}

/// Default privacy policy text derived from `AppAttrs` (provider + contact + date), i18n EN/ES/RU.
#[must_use]
pub fn default_privacy_text(attrs: &AppAttrs, lang: Language) -> String {
    let provider = attrs.org;
    match lang {
        Language::Eng => format!(
            r#"## Privacy Policy

This privacy policy applies to the app (hereby referred to as "Application") for mobile devices that was created by {provider} (hereby referred to as "Service Provider") as a Free service. This service is intended for use "AS IS".

## What information does the Application obtain and how is it used?

The Application does not obtain any information when you download and use it. Registration is not required to use the Application.

## Does the Application collect precise real time location information of the device?

This Application does not collect precise information about the location of your mobile device.

## Do third parties see and/or have access to information obtained by the Application?

Since the Application does not collect any information, no data is shared with third parties.

## What are my opt-out rights?

You can stop all collection of information by the Application easily by uninstalling it. You may use the standard uninstall processes as may be available as part of your mobile device or via the mobile application marketplace or network.

## Children

The Application is not used to knowingly solicit data from or market to children under the age of 13.

The Service Provider does not knowingly collect personally identifiable information from children. The Service Provider encourages all children to never submit any personally identifiable information through the Application and/or Services. The Service Provider encourage parents and legal guardians to monitor their children's Internet usage and to help enforce this Policy by instructing their children never to provide personally identifiable information through the Application and/or Services without their permission. If you have reason to believe that a child has provided personally identifiable information to the Service Provider through the Application and/or Services, please contact the Service Provider ({FUNCTORA_EMAIL}) so that they will be able to take the necessary actions. You must also be at least 16 years of age to consent to the processing of your personally identifiable information in your country (in some countries we may allow your parent or guardian to do so on your behalf).

## Security

The Service Provider is concerned about safeguarding the confidentiality of your information. However, since the Application does not collect any information, there is no risk of your data being accessed by unauthorized individuals.

## Changes

This Privacy Policy may be updated from time to time for any reason. The Service Provider will notify you of any changes to their Privacy Policy by updating this page with the new Privacy Policy. You are advised to consult this Privacy Policy regularly for any changes, as continued use is deemed approval of all changes.

This privacy policy is effective as of {FUNCTORA_CORE_DATE}

## Your Consent

By using the Application, you are consenting to the processing of your information as set forth in this Privacy Policy now and as amended by the Service Provider.

## Contact Us

If you have any questions regarding privacy while using the Application, or have questions about the practices, please contact the Service Provider via email at {FUNCTORA_EMAIL}."#,
        ),
        Language::Spa => format!(
            r#"## Política de Privacidad

Esta política de privacidad se aplica a la aplicación (en adelante, "Aplicación") para dispositivos móviles creada por {provider} (en adelante, "Proveedor de Servicios") como un servicio gratuito. Este servicio está destinado a su uso "TAL CUAL".

## ¿Qué información obtiene la Aplicación y cómo se utiliza?

La Aplicación no obtiene ninguna información cuando la descargas y la usas. No se requiere registro para usar la Aplicación.

## ¿La Aplicación recopila información precisa de ubicación en tiempo real del dispositivo?

Esta Aplicación no recopila información precisa sobre la ubicación de tu dispositivo móvil.

## ¿Tienen terceros acceso a la información obtenida por la Aplicación?

Dado que la Aplicación no recopila ninguna información, no se comparten datos con terceros.

## ¿Cuáles son mis derechos de exclusión voluntaria?

Puedes detener toda la recopilación de información por parte de la Aplicación fácilmente desinstalándola. Puedes utilizar los procesos de desinstalación estándar disponibles como parte de tu dispositivo móvil o a través del mercado o red de aplicaciones móviles.

## Niños

La Aplicación no se utiliza para solicitar datos a sabiendas de niños menores de 13 años ni para comercializar con ellos.

El Proveedor de Servicios no recopila a sabiendas información de identificación personal de niños. El Proveedor de Servicios alienta a todos los niños a que nunca envíen ninguna información de identificación personal a través de la Aplicación y/o los Servicios. El Proveedor de Servicios alienta a los padres y tutores legales a monitorear el uso de Internet de sus hijos y a ayudar a hacer cumplir esta Política instruyendo a sus hijos para que nunca proporcionen información de identificación personal a través de la Aplicación y/o los Servicios sin su permiso. Si tienes motivos para creer que un niño ha proporcionado información de identificación personal al Proveedor de Servicios a través de la Aplicación y/o los Servicios, comunícate con el Proveedor de Servicios ({FUNCTORA_EMAIL}) para que puedan tomar las medidas necesarias. También debes tener al menos 16 años de edad para dar tu consentimiento al procesamiento de tu información de identificación personal en tu país (en algunos países podemos permitir que tu padre o tutor lo haga en tu nombre).

## Seguridad

El Proveedor de Servicios se preocupa por salvaguardar la confidencialidad de tu información. Sin embargo, dado que la Aplicación no recopila ninguna información, no existe riesgo de que personas no autorizadas accedan a tus datos.

## Cambios

Esta Política de Privacidad puede actualizarse de vez en cuando por cualquier motivo. El Proveedor de Servicios te notificará de cualquier cambio en su Política de Privacidad actualizando esta página con la nueva Política de Privacidad. Se te aconseja consultar esta Política de Privacidad regularmente para ver si hay cambios, ya que el uso continuado se considera aprobación de todos los cambios.

Esta política de privacidad es efectiva a partir de {FUNCTORA_CORE_DATE}

## Tu Consentimiento

Al usar la Aplicación, das tu consentimiento al procesamiento de tu información según lo establecido en esta Política de Privacidad ahora y según sea modificada por el Proveedor de Servicios.

## Contáctanos

Si tienes alguna pregunta sobre privacidad al usar la Aplicación, o tienes preguntas sobre las prácticas, comunícate con el Proveedor de Servicios por correo electrónico a {FUNCTORA_EMAIL}."#,
        ),
        Language::Rus => format!(
            r"## Политика конфиденциальности

Esta политика конфиденциальности применяется к приложению (далее именуемому «Приложение») для мобильных устройств, созданному {provider} (далее именуемому «Поставщик услуг») в качестве бесплатной услуги. Эта услуга предназначена для использования «КАК ЕСТЬ».

## Какую информацию получает Приложение и как она используется?

Приложение не получает никакой информации, когда вы загружаете и используете его. Регистрация не требуется для использования Приложения.

## Собирает ли Приложение точную информацию о местоположении устройства в реальном времени?

Это Приложение не собирает точную информацию о местоположении вашего мобильного устройства.

## Видят ли третьи стороны и/или имеют ли доступ к информации, полученной Приложением?

Поскольку Приложение не собирает никакой информации, никакие данные не передаются третьим лицам.

## Каковы мои права на отказ?

Вы можете легко прекратить сбор информации Приложением, удалив его. Вы можете использовать стандартные процессы удаления, доступные как часть вашего мобильного устройства или через магазин мобильных приложений или сеть.

## Дети

Приложение не используется для намеренного сбора данных или маркетинга среди детей младше 13 лет.

Поставщик услуг не собирает намеренно личную информацию от детей. Поставщик услуг призывает всех детей никогда не отправлять какую-либо личную информацию через Приложение и/или Услуги. Поставщик услуг призывает родителей и законных опекунов контролировать использование Интернета их детьми и помогать обеспечивать выполнение этой Политики, инструктируя своих детей никогда не предоставлять личную информацию через Приложение и/или Услуги без их разрешения. Если у вас есть основания полагать, что ребенок предоставил личную информацию Поставщику услуг через Приложение и/или Услуги, свяжитесь с Поставщиком услуг ({FUNCTORA_EMAIL}), чтобы они могли предпринять необходимые меры. Вам также должно быть не менее 16 лет, чтобы дать согласие на обработку вашей личной информации в вашей стране (в некоторых странах мы можем разрешить вашему родителю или опекуну сделать это от вашего имени).

## Безопасность

Поставщик услуг заботится о защите конфиденциальности вашей информации. Однако, поскольку Приложение не собирает никакой информации, нет риска доступа к вашим данным посторонних лиц.

## Изменения

Эта Политика конфиденциальности может время от времени обновляться по любой причине. Поставщик услуг уведомит вас о любых изменениях в своей Политике конфиденциальности, обновив эту страницу новой Политикой конфиденциальности. Вам рекомендуется регулярно просматривать эту Политику конфиденциальности на предмет изменений, так как продолжение использования считается одобрением всех изменений.

Эта политика конфиденциальности вступает в силу с {FUNCTORA_CORE_DATE}

## Ваше согласие

Используя Приложение, вы даете согласие на обработку вашей информации, как изложено в этой Политике конфиденциальности сейчас и с изменениями, внесенными Поставщиком услуг.

## Свяжитесь с нами

Если у вас есть какие-либо вопросы относительно конфиденциальности при использовании Приложения или вопросы о практике, свяжитесь с Поставщиком услуг по электронной почте {FUNCTORA_EMAIL}."
        ),
        _ => default_privacy_text(attrs, Language::Eng),
    }
}

/// Default donate greeting derived from `AppAttrs` (org), i18n EN/ES/RU.
#[must_use]
pub fn default_donate_greeting(attrs: &AppAttrs, lang: Language) -> String {
    match lang {
        Language::Eng => format!(
            "Hello, User! I'm {}, the creator of this software.",
            attrs.org
        ),
        Language::Spa => format!(
            "¡Hola, Usuario! Soy {}, el creador de este software.",
            attrs.org
        ),
        Language::Rus => format!(
            "Здравствуйте, пользователь! Я {}, создатель этого программного обеспечения.",
            attrs.org
        ),
        _ => default_donate_greeting(attrs, Language::Eng),
    }
}

/// Default donate intro derived from `AppAttrs` (org), i18n EN/ES/RU.
#[must_use]
pub fn default_donate_intro(attrs: &AppAttrs, lang: Language) -> String {
    match lang {
        Language::Eng => format!(
            "If you're enjoying this software, a donation would be greatly appreciated. Sincerely yours, {}.",
            attrs.org
        ),
        Language::Spa => format!(
            "Si estás disfrutando este software, una donación sería muy apreciada. Atentamente, {}.",
            attrs.org
        ),
        Language::Rus => format!(
            "Если это программное обеспечение вам нравится, я буду очень признателен за пожертвование. С уважением, {}.",
            attrs.org
        ),
        _ => default_donate_intro(attrs, Language::Eng),
    }
}

/// Default about text derived from `AppAttrs` (name, description, links), i18N EN/ES/RU.
#[must_use]
pub fn default_about_text(attrs: &AppAttrs, lang: Language) -> String {
    let app = attrs.app_name();
    let desc = attrs.description;
    let source = attrs.source_url();
    let author = attrs.author_url();
    let beta = attrs.beta_url();
    let play = attrs.google_play_url();
    let apk = attrs.apk_url();
    let links = format!(
        "\n\n## Links\n\n- [Source code]({source})\n- [Author]({author})\n- [Beta]({beta})\n- [Google Play]({play})\n- [APK]({apk})"
    );
    match lang {
        Language::Eng => {
            format!(
                r"{app} is a cross-platform application. {desc}

With {app}, you can:

- Use the application on desktop, mobile and web
- Explore the features and settings

The application follows modern best practices:

- Secure by design
- No data leaves your device unless you explicitly choose to share it

Secure, private, and yours alone."
            ) + &links
        }
        Language::Spa => {
            format!(
                r"{app} es una aplicación multiplataforma. {desc}

Con {app}, puedes:

- Usar la aplicación en escritorio, móvil y web
- Explorar las funciones y configuraciones

La aplicación sigue las mejores prácticas modernas:

- Segura por diseño
- Ningún dato sale de tu dispositivo a menos que elijas explícitamente compartirlo

Segura, privada y solo tuya."
            ) + &links
        }
        Language::Rus => {
            format!(
                r"{app} — кроссплатформенное приложение. {desc}

С {app} вы можете:

- Использовать приложение на компьютере, мобильном устройстве и в вебе
- Изучить функции и настройки

Приложение следует современным практикам:

- Безопасно по дизайну
- Никакие данные не покидают ваше устройство, если вы явно не решите ими поделиться

Безопасно, приватно и только ваше."
            ) + &links
        }
        _ => default_about_text(attrs, Language::Eng),
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
