use crate::FUNCTORA_DIOXUS_DATE;
use crate::FUNCTORA_DIOXUS_YEAR;
use crate::i18n::{I18N, Language};
use std::env::VarError;
use std::sync::mpsc::RecvError;

#[derive(Clone, Debug, PartialEq)]
pub enum Msg {
    Copied,
    Password,
    PasswordPlaceholder,
    Paste,
    Copy,
    Loading,
    ErrorTitle(String),
    EnvError(VarError),
    ChannelError(RecvError),
    NotJsonObject(String),
    #[cfg(target_os = "android")]
    JniError(String),
    ErrorTitleLabel,
    PasswordRequired,
    ClipboardWriteError(String),
    ClipboardReadError(String),
    CameraNotAvailable(String),
    CameraPermissionDenied(String),
    Back,
    Home,
    DonateGreeting,
    DonateIntro,
    Stage(crate::progress::Stage),
    Copyright,
    AllRightsReserved,
    ByContinuing,
    YouAgree,
    TermsOfService,
    TermsOfServiceTitle,
    PrivacyPolicyAnd,
    PrivacyPolicyTitle,
    VersionLabel,
    Application,
    Theme,
    Donate,
    DonateLink,
    And,
    FooterShareWord,
    FooterAppWord,
    LanguageFlag(Language),
    LanguageName(Language),
    LicenseText,
    PrivacyText,
    CopyAppLink,
    ShareAppLink,
    Sent,
    SourceCodeButton,
    AuthorButton,
    JoinTestingButton,
    GooglePlayButton,
    DownloadApkButton,
    AboutAndroidBeta1,
    AboutAndroidBetaLink1,
    AboutAndroidBeta2,
    AboutAndroidBetaLink2,
    AboutAndroidBeta3,
    AboutAndroidBetaLink3,
    AboutAndroidBeta4,
}

impl I18N for Msg {
    fn render_eng(&self) -> String {
        match self {
            Self::Copied => "Copied!".into(),
            Self::Password => "Password".into(),
            Self::PasswordPlaceholder => "Enter password".into(),
            Self::Paste => "Paste".into(),
            Self::Copy => "Copy".into(),
            Self::Loading => "Loading...".into(),
            Self::ErrorTitle(e) => format!("Error: {e}"),
            Self::EnvError(e) => format!("Environment variable error: {e}"),
            Self::ChannelError(e) => format!("Channel receive error: {e}"),
            Self::NotJsonObject(e) => format!("Expected JSON object, got: {e}"),
            #[cfg(target_os = "android")]
            Self::JniError(e) => format!("JNI error: {e}"),
            Self::ErrorTitleLabel => "Error".into(),
            Self::PasswordRequired => "Password is required for encryption".into(),
            Self::ClipboardWriteError(e) => format!("Failed to copy to clipboard: {e}"),
            Self::ClipboardReadError(e) => format!("Failed to read from clipboard: {e}"),
            Self::CameraNotAvailable(e) => crate::Error::CameraNotAvailable(e.clone()).render_eng(),
            Self::CameraPermissionDenied(e) => crate::Error::CameraPermissionDenied(e.clone()).render_eng(),
            Self::Back => "Back".into(),
            Self::Home => "Home".into(),
            Self::DonateGreeting => "Hello, User!".into(),
            Self::DonateIntro => {
                "I'm Functora, the creator of this software. If you're enjoying it, a donation would be greatly appreciated. Sincerely yours, Functora.".into()
            }
            Self::Stage(s) => match s {
                crate::progress::Stage::Attach => "Attaching files...".into(),
                crate::progress::Stage::Zip => "Zipping files...".into(),
                crate::progress::Stage::Encrypt => "Encrypting...".into(),
                crate::progress::Stage::Decrypt => "Decrypting...".into(),
                crate::progress::Stage::Unzip => "Unzipping...".into(),
                crate::progress::Stage::Download => "Downloading...".into(),
                crate::progress::Stage::Preview => "Preparing preview...".into(),
            },
            Self::Copyright => "©".into(),
            Self::AllRightsReserved => "All rights reserved.".into(),
            Self::ByContinuing => "By continuing to use this software, you agree to the".into(),
            Self::YouAgree | Self::And => "and".into(),
            Self::TermsOfService | Self::TermsOfServiceTitle => "Terms of Service".into(),
            Self::PrivacyPolicyAnd | Self::PrivacyPolicyTitle => "Privacy Policy".into(),
            Self::VersionLabel => "Version".into(),
            Self::Application => "Application".into(),
            Self::Theme => "Theme".into(),
            Self::Donate | Self::DonateLink => "Donate".into(),
            Self::FooterShareWord => "Share".into(),
            Self::FooterAppWord => "app".into(),
            Self::LanguageFlag(Language::Eng) => "🇬🇧".into(),
            Self::LanguageFlag(Language::Spa) => "🇪🇸".into(),
            Self::LanguageFlag(Language::Rus) => "🇷🇺".into(),
            Self::LanguageFlag(_) => "🌐".into(),
            Self::LanguageName(Language::Eng) => "English".into(),
            Self::LanguageName(Language::Spa) => "Español".into(),
            Self::LanguageName(Language::Rus) => "Русский".into(),
            Self::LanguageName(_) => "Unknown".into(),
            Self::LicenseText => format!(r#"Copyright (c) {FUNCTORA_DIOXUS_YEAR} Functora

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."#),
            Self::PrivacyText => format!(r#"Privacy Policy

This privacy policy applies to the app (hereby referred to as "Application") for mobile devices that was created by Functora (hereby referred to as "Service Provider") as a Free service. This service is intended for use "AS IS".

What information does the Application obtain and how is it used?

The Application does not obtain any information when you download and use it. Registration is not required to use the Application.

Does the Application collect precise real time location information of the device?

This Application does not collect precise information about the location of your mobile device.

Do third parties see and/or have access to information obtained by the Application?

Since the Application does not collect any information, no data is shared with third parties.

What are my opt-out rights?

You can stop all collection of information by the Application easily by uninstalling it. You may use the standard uninstall processes as may be available as part of your mobile device or via the mobile application marketplace or network.

Children

The Application is not used to knowingly solicit data from or market to children under the age of 13.

The Service Provider does not knowingly collect personally identifiable information from children. The Service Provider encourages all children to never submit any personally identifiable information through the Application and/or Services. The Service Provider encourage parents and legal guardians to monitor their children's Internet usage and to help enforce this Policy by instructing their children never to provide personally identifiable information through the Application and/or Services without their permission. If you have reason to believe that a child has provided personally identifiable information to the Service Provider through the Application and/or Services, please contact the Service Provider (functora@proton.me) so that they will be able to take the necessary actions. You must also be at least 16 years of age to consent to the processing of your personally identifiable information in your country (in some countries we may allow your parent or guardian to do so on your behalf).

Security

The Service Provider is concerned about safeguarding the confidentiality of your information. However, since the Application does not collect any information, there is no risk of your data being accessed by unauthorized individuals.

Changes

This Privacy Policy may be updated from time to time for any reason. The Service Provider will notify you of any changes to their Privacy Policy by updating this page with the new Privacy Policy. You are advised to consult this Privacy Policy regularly for any changes, as continued use is deemed approval of all changes.

This privacy policy is effective as of {FUNCTORA_DIOXUS_DATE}

Your Consent

By using the Application, you are consenting to the processing of your information as set forth in this Privacy Policy now and as amended by the Service Provider.

Contact Us

If you have any questions regarding privacy while using the Application, or have questions about the practices, please contact the Service Provider via email at functora@proton.me."#),
            Self::CopyAppLink => "Copy link".into(),
            Self::ShareAppLink => "Share app".into(),
            Self::Sent => "Sent!".into(),
            Self::SourceCodeButton => "Source code".into(),
            Self::AuthorButton => "Author".into(),
            Self::JoinTestingButton => "Join testing".into(),
            Self::GooglePlayButton => "Google Play".into(),
            Self::DownloadApkButton => "Download APK".into(),
            Self::AboutAndroidBeta1 => "The Android app is in closed beta. To install it, join the".into(),
            Self::AboutAndroidBetaLink1 => "closed beta".into(),
            Self::AboutAndroidBeta2 => "group and then install the app from".into(),
            Self::AboutAndroidBetaLink2 => "Google Play".into(),
            Self::AboutAndroidBeta3 => ", or download the".into(),
            Self::AboutAndroidBetaLink3 => "APK file".into(),
            Self::AboutAndroidBeta4 => "directly.".into(),
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Copied => "¡Copiado!".into(),
            Self::Password => "Contraseña".into(),
            Self::PasswordPlaceholder => "Ingresa contraseña".into(),
            Self::Paste => "Pegar".into(),
            Self::Copy => "Copiar".into(),
            Self::Loading => "Cargando...".into(),
            Self::ErrorTitle(e) => format!("Error: {e}"),
            Self::EnvError(e) => format!("Error de variable de entorno: {e}"),
            Self::ChannelError(e) => format!("Error de recepción en canal: {e}"),
            Self::NotJsonObject(e) => format!("Se esperaba un objeto JSON, se obtuvo: {e}"),
            #[cfg(target_os = "android")]
            Self::JniError(e) => format!("Error JNI: {e}"),
            Self::ErrorTitleLabel => "Error".into(),
            Self::PasswordRequired => "Se requiere contraseña para el cifrado".into(),
            Self::ClipboardWriteError(e) => format!("No se pudo copiar al portapapeles: {e}"),
            Self::ClipboardReadError(e) => format!("No se pudo leer del portapapeles: {e}"),
            Self::CameraNotAvailable(e) => crate::Error::CameraNotAvailable(e.clone()).render_spa(),
            Self::CameraPermissionDenied(e) => crate::Error::CameraPermissionDenied(e.clone()).render_spa(),
            Self::Back => "Atrás".into(),
            Self::Home => "Inicio".into(),
            Self::DonateGreeting => "¡Hola, Usuario!".into(),
            Self::DonateIntro => {
                "Soy Functora, el creador de este software. Si lo estás disfrutando, una donación sería muy apreciada. Atentamente, Functora.".into()
            }
            Self::Stage(s) => match s {
                crate::progress::Stage::Attach => "Adjuntando archivos...".into(),
                crate::progress::Stage::Zip => "Comprimiendo archivos...".into(),
                crate::progress::Stage::Encrypt => "Cifrando...".into(),
                crate::progress::Stage::Decrypt => "Descifrando...".into(),
                crate::progress::Stage::Unzip => "Descomprimiendo...".into(),
                crate::progress::Stage::Download => "Descargando...".into(),
                crate::progress::Stage::Preview => "Preparando vista previa...".into(),
            },
            Self::Copyright => "©".into(),
            Self::AllRightsReserved => "Todos los derechos reservados.".into(),
            Self::ByContinuing => "Al continuar usando este software, aceptas los".into(),
            Self::YouAgree => "y la".into(),
            Self::TermsOfService | Self::TermsOfServiceTitle => "Términos de Servicio".into(),
            Self::PrivacyPolicyAnd | Self::PrivacyPolicyTitle => "Política de Privacidad".into(),
            Self::VersionLabel => "Versión".into(),
            Self::Application => "Aplicación".into(),
            Self::Theme => "Tema".into(),
            Self::Donate | Self::DonateLink => "Donar".into(),
            Self::And => "y".into(),
            Self::FooterShareWord => "Compartir".into(),
            Self::FooterAppWord => "la app".into(),
            Self::LanguageFlag(Language::Eng) => "🇬🇧".into(),
            Self::LanguageFlag(Language::Spa) => "🇪🇸".into(),
            Self::LanguageFlag(Language::Rus) => "🇷🇺".into(),
            Self::LanguageFlag(_) => "🌐".into(),
            Self::LanguageName(Language::Eng) => "English".into(),
            Self::LanguageName(Language::Spa) => "Español".into(),
            Self::LanguageName(Language::Rus) => "Русский".into(),
            Self::LanguageName(_) => "Unknown".into(),
            Self::LicenseText => format!(r#"Copyright (c) {FUNCTORA_DIOXUS_YEAR} Functora

Por la presente se concede permiso, libre de cargos, a cualquier persona que haya obtenido una copia de este software y archivos de documentación asociados (el "Software"), para utilizar el Software sin restricción, incluyendo sin limitación los derechos a usar, copiar, modificar, fusionar, publicar, distribuir, sublicenciar y/o vender copias del Software, y a permitir a las personas a las que se les proporcione el Software que hagan lo mismo, sujeto a las siguientes condiciones:

El aviso de copyright anterior y este aviso de permiso deberán incluirse en todas las copias o partes sustanciales del Software.

EL SOFTWARE SE PROPORCIONA "TAL CUAL", SIN GARANTÍA DE NINGÚN TIPO, EXPRESA O IMPLÍCITA, INCLUYENDO PERO NO LIMITADO A LAS GARANTÍAS DE COMERCIABILIDAD, IDONEIDAD PARA UN FIN PARTICULAR Y NO INFRACCIÓN. EN NINGÚN CASO LOS AUTORES O TITULARES DEL COPYRIGHT SERÁN RESPONSABLES DE NINGUNA RECLAMACIÓN, DAÑOS U OTRAS RESPONSABILIDADES, YA SEA EN UNA ACCIÓN DE CONTRATO, AGRAVIO O DE OTRO TIPO, QUE SURJA DE, O EN RELACIÓN CON EL SOFTWARE O EL USO U OTROS TRATOS EN EL SOFTWARE."#),
            Self::PrivacyText => format!(r#"Política de Privacidad

Esta política de privacidad se aplica a la aplicación (en adelante, "Aplicación") para dispositivos móviles creada por Functora (en adelante, "Proveedor de Servicios") como un servicio gratuito. Este servicio está destinado a su uso "TAL CUAL".

¿Qué información obtiene la Aplicación y cómo se utiliza?

La Aplicación no obtiene ninguna información cuando la descargas y la usas. No se requiere registro para usar la Aplicación.

¿La Aplicación recopila información precisa de ubicación en tiempo real del dispositivo?

Esta Aplicación no recopila información precisa sobre la ubicación de tu dispositivo móvil.

¿Tienen terceros acceso a la información obtenida por la Aplicación?

Dado que la Aplicación no recopila ninguna información, no se comparten datos con terceros.

¿Cuáles son mis derechos de exclusión voluntaria?

Puedes detener toda la recopilación de información por parte de la Aplicación fácilmente desinstalándola. Puedes utilizar los procesos de desinstalación estándar disponibles como parte de tu dispositivo móvil o a través del mercado o red de aplicaciones móviles.

Niños

La Aplicación no se utiliza para solicitar datos a sabiendas de niños menores de 13 años ni para comercializar con ellos.

El Proveedor de Servicios no recopila a sabiendas información de identificación personal de niños. El Proveedor de Servicios alienta a todos los niños a que nunca envíen ninguna información de identificación personal a través de la Aplicación y/o los Servicios. El Proveedor de Servicios alienta a los padres y tutores legales a monitorear el uso de Internet de sus hijos y a ayudar a hacer cumplir esta Política instruyendo a sus hijos para que nunca proporcionen información de identificación personal a través de la Aplicación y/o los Servicios sin su permiso. Si tienes motivos para creer que un niño ha proporcionado información de identificación personal al Proveedor de Servicios a través de la Aplicación y/o los Servicios, comunícate con el Proveedor de Servicios (functora@proton.me) para que puedan tomar las medidas necesarias. También debes tener al menos 16 años de edad para dar tu consentimiento al procesamiento de tu información de identificación personal en tu país (en algunos países podemos permitir que tu padre o tutor lo haga en tu nombre).

Seguridad

El Proveedor de Servicios se preocupa por salvaguardar la confidencialidad de tu información. Sin embargo, dado que la Aplicación no recopila ninguna información, no existe riesgo de que personas no autorizadas accedan a tus datos.

Cambios

Esta Política de Privacidad puede actualizarse de vez en cuando por cualquier motivo. El Proveedor de Servicios te notificará de cualquier cambio en su Política de Privacidad actualizando esta página con la nueva Política de Privacidad. Se te aconseja consultar esta Política de Privacidad regularmente para ver si hay cambios, ya que el uso continuado se considera aprobación de todos los cambios.

Esta política de privacidad es efectiva a partir de {FUNCTORA_DIOXUS_DATE}

Tu Consentimiento

Al usar la Aplicación, das tu consentimiento al procesamiento de tu información según lo establecido en esta Política de Privacidad ahora y según sea modificada por el Proveedor de Servicios.

Contáctanos

Si tienes alguna pregunta sobre privacidad al usar la Aplicación, o tienes preguntas sobre las prácticas, comunícate con el Proveedor de Servicios por correo electrónico a functora@proton.me."#),
            Self::CopyAppLink => "Copiar enlace".into(),
            Self::ShareAppLink => "Compartir la app".into(),
            Self::Sent => "¡Enviado!".into(),
            Self::SourceCodeButton => "Código fuente".into(),
            Self::AuthorButton => "Autor".into(),
            Self::JoinTestingButton => "Unirse a prueba".into(),
            Self::GooglePlayButton => "Google Play".into(),
            Self::DownloadApkButton => "Descargar APK".into(),
            Self::AboutAndroidBeta1 => "La aplicación de Android está en beta cerrada. Para instalarla, únase al grupo de".into(),
            Self::AboutAndroidBetaLink1 => "beta cerrada".into(),
            Self::AboutAndroidBeta2 => "y luego instale la aplicación desde".into(),
            Self::AboutAndroidBetaLink2 => "Google Play".into(),
            Self::AboutAndroidBeta3 => ", o descargue el".into(),
            Self::AboutAndroidBetaLink3 => "archivo APK".into(),
            Self::AboutAndroidBeta4 => "directamente.".into(),
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Copied => "Скопировано!".into(),
            Self::Password => "Пароль".into(),
            Self::PasswordPlaceholder => "Введите пароль".into(),
            Self::Paste => "Вставить".into(),
            Self::Copy => "Копировать".into(),
            Self::Loading => "Загрузка...".into(),
            Self::ErrorTitle(e) => format!("Ошибка: {e}"),
            Self::EnvError(e) => format!("Ошибка переменной окружения: {e}"),
            Self::ChannelError(e) => format!("Ошибка получения из канала: {e}"),
            Self::NotJsonObject(e) => format!("Ожидался JSON-объект, получено: {e}"),
            #[cfg(target_os = "android")]
            Self::JniError(e) => format!("Ошибка JNI: {e}"),
            Self::ErrorTitleLabel => "Ошибка".into(),
            Self::PasswordRequired => "Для шифрования требуется пароль".into(),
            Self::ClipboardWriteError(e) => format!("Не удалось скопировать в буфер обмена: {e}"),
            Self::ClipboardReadError(e) => format!("Не удалось прочитать из буфера обмена: {e}"),
            Self::CameraNotAvailable(e) => crate::Error::CameraNotAvailable(e.clone()).render_rus(),
            Self::CameraPermissionDenied(e) => crate::Error::CameraPermissionDenied(e.clone()).render_rus(),
            Self::Back => "Назад".into(),
            Self::Home => "Главная".into(),
            Self::DonateGreeting => "Здравствуйте, пользователь!".into(),
            Self::DonateIntro => {
                "Я Functora, создатель этого программного обеспечения. Если оно вам нравится, я буду очень признателен за пожертвование. С уважением, Functora.".into()
            }
            Self::Stage(s) => match s {
                crate::progress::Stage::Attach => "Прикрепление файлов...".into(),
                crate::progress::Stage::Zip => "Архивация файлов...".into(),
                crate::progress::Stage::Encrypt => "Шифрование...".into(),
                crate::progress::Stage::Decrypt => "Расшифровка...".into(),
                crate::progress::Stage::Unzip => "Распаковка...".into(),
                crate::progress::Stage::Download => "Скачивание...".into(),
                crate::progress::Stage::Preview => "Подготовка предпросмотра...".into(),
            },
            Self::Copyright => "©".into(),
            Self::AllRightsReserved => "Все права защищены.".into(),
            Self::ByContinuing => "Продолжая использовать это программное обеспечение, вы соглашаетесь с".into(),
            Self::YouAgree | Self::And => "и".into(),
            Self::TermsOfService => "Условиями обслуживания".into(),
            Self::TermsOfServiceTitle => "Условия обслуживания".into(),
            Self::PrivacyPolicyAnd => "Политикой конфиденциальности".into(),
            Self::PrivacyPolicyTitle => "Политика конфиденциальности".into(),
            Self::VersionLabel => "Версия".into(),
            Self::Application => "Приложение".into(),
            Self::Theme => "Тема".into(),
            Self::Donate => "Пожертвовать".into(),
            Self::DonateLink => "Сделайте пожертвование".into(),
            Self::FooterShareWord => "Поделитесь".into(),
            Self::FooterAppWord => "приложением".into(),
            Self::LanguageFlag(Language::Eng) => "🇬🇧".into(),
            Self::LanguageFlag(Language::Spa) => "🇪🇸".into(),
            Self::LanguageFlag(Language::Rus) => "🇷🇺".into(),
            Self::LanguageFlag(_) => "🌐".into(),
            Self::LanguageName(Language::Eng) => "English".into(),
            Self::LanguageName(Language::Spa) => "Español".into(),
            Self::LanguageName(Language::Rus) => "Русский".into(),
            Self::LanguageName(_) => "Unknown".into(),
            Self::LicenseText => format!(r"Copyright (c) {FUNCTORA_DIOXUS_YEAR} Functora

Настоящим предоставляется бесплатное разрешение любому лицу, получившему копию данного программного обеспечения и сопутствующих файлов документации (далее - «Программное обеспечение»), использовать Программное обеспечение без ограничений, включая неограниченное право использовать, копировать, изменять, объединять, публиковать, распространять, сублицензировать и/или продавать копии Программного обеспечения, а также разрешать лицам, которым предоставлено Программное обеспечение, делать то же самое, при соблюдении следующих условий:

Указанное выше уведомление об авторских правах и данное уведомление о разрешении должны быть включены во все копии или существенные части Программного обеспечения.

ПРОГРАММНОЕ ОБЕСПЕЧЕНИЕ ПРЕДОСТАВЛЯЕТСЯ «КАК ЕСТЬ», БЕЗ КАКИХ-ЛИБО ГАРАНТИЙ, ЯВНО ВЫРАЖЕННЫХ ИЛИ ПОДРАЗУМЕВАЕМЫХ, ВКЛЮЧАЯ, НО НЕ ОГРАНИЧИВАЯСЬ ГАРАНТИЯМИ ТОВАРНОГО СОСТОЯНИЯ, ПРИГОДНОСТИ ДЛЯ КОНКРЕТНЫХ ЦЕЛЕЙ И ОТСУТСТВИЯ НАРУШЕНИЙ АВТОРСКИХ ПРАВ. НИ ПРИ КАКИХ ОБСТОЯТЕЛЬСТВАХ АВТОРЫ ИЛИ ПРАВООБЛАДАТЕЛИ НЕ НЕСУТ ОТВЕТСТВЕННОСТИ ПО ЛЮБЫМ ПРЕТЕНЗИЯМ, ЗА УБЫТКИ ИЛИ ДРУГИЕ ТРЕБОВАНИЯ, ВЫТЕКАЮЩИЕ ИЗ ДОГОВОРА, ДЕЛИКТА ИЛИ ИНЫХ ОБСТОЯТЕЛЬСТВ, СВЯЗАННЫЕ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ, ЕГО ИСПОЛЬЗОВАНИЕМ ИЛИ ДРУГИМИ ДЕЙСТВИЯМИ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ."),
            Self::PrivacyText => format!(r"Политика конфиденциальности

Эта политика конфиденциальности применяется к приложению (далее именуемому «Приложение») для мобильных устройств, созданному Functora (далее именуемому «Поставщик услуг») в качестве бесплатной услуги. Эта услуга предназначена для использования «КАК ЕСТЬ».

Какую информацию получает Приложение и как она используется?

Приложение не получает никакой информации, когда вы загружаете и используете его. Регистрация не требуется для использования Приложения.

Собирает ли Приложение точную информацию о местоположении устройства в реальном времени?

Это Приложение не собирает точную информацию о местоположении вашего мобильного устройства.

Видят ли третьи стороны и/или имеют ли доступ к информации, полученной Приложением?

Поскольку Приложение не собирает никакой информации, никакие данные не передаются третьим лицам.

Каковы мои права на отказ?

Вы можете легко прекратить сбор информации Приложением, удалив его. Вы можете использовать стандартные процессы удаления, доступные как часть вашего мобильного устройства или через магазин мобильных приложений или сеть.

Дети

Приложение не используется для намеренного сбора данных или маркетинга среди детей младше 13 лет.

Поставщик услуг не собирает намеренно личную информацию от детей. Поставщик услуг призывает всех детей никогда не отправлять какую-либо личную информацию через Приложение и/или Услуги. Поставщик услуг призывает родителей и законных опекунов контролировать использование Интернета их детьми и помогать обеспечивать выполнение этой Политики, инструктируя своих детей никогда не предоставлять личную информацию через Приложение и/или Услуги без их разрешения. Если у вас есть основания полагать, что ребенок предоставил личную информацию Поставщику услуг через Приложение и/или Услуги, свяжитесь с Поставщиком услуг (functora@proton.me), чтобы они могли предпринять необходимые меры. Вам также должно быть не менее 16 лет, чтобы дать согласие на обработку вашей личной информации в вашей стране (в некоторых странах мы можем разрешить вашему родителю или опекуну сделать это от вашего имени).

Безопасность

Поставщик услуг заботится о защите конфиденциальности вашей информации. Однако, поскольку Приложение не собирает никакой информации, нет риска доступа к вашим данным посторонних лиц.

Изменения

Эта Политика конфиденциальности может время от времени обновляться по любой причине. Поставщик услуг уведомит вас о любых изменениях в своей Политике конфиденциальности, обновив эту страницу новой Политикой конфиденциальности. Вам рекомендуется регулярно просматривать эту Политику конфиденциальности на предмет изменений, так как продолжение использования считается одобрением всех изменений.

Эта политика конфиденциальности вступает в силу с {FUNCTORA_DIOXUS_DATE}

Ваше согласие

Используя Приложение, вы даете согласие на обработку вашей информации, как изложено в этой Политике конфиденциальности сейчас и с изменениями, внесенными Поставщиком услуг.

Свяжитесь с нами

Если у вас есть какие-либо вопросы относительно конфиденциальности при использовании Приложения или вопросы о практике, свяжитесь с Поставщиком услуг по электронной почте functora@proton.me."),
            Self::CopyAppLink => "Скопировать ссылку".into(),
            Self::ShareAppLink => "Поделиться приложением".into(),
            Self::Sent => "Отправлено!".into(),
            Self::SourceCodeButton => "Исходный код".into(),
            Self::AuthorButton => "Автор".into(),
            Self::JoinTestingButton => "Вступить в бета-тест".into(),
            Self::GooglePlayButton => "Google Play".into(),
            Self::DownloadApkButton => "Скачать APK".into(),
            Self::AboutAndroidBeta1 => "Приложение Android в закрытом бета-тестировании. Чтобы установить его, вступите в группу".into(),
            Self::AboutAndroidBetaLink1 => "бета-тестирования".into(),
            Self::AboutAndroidBeta2 => ", затем установите приложение из".into(),
            Self::AboutAndroidBetaLink2 => "Google Play".into(),
            Self::AboutAndroidBeta3 => " или скачайте".into(),
            Self::AboutAndroidBetaLink3 => "APK-файл".into(),
            Self::AboutAndroidBeta4 => "напрямую.".into(),
        }
    }
}
