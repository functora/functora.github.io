use wasm_bindgen::prelude::*;

#[wasm_bindgen(inline_js = r#"
const cryptogram_secure_storage_unsupported = (error) => (
  error === 'WebAppMethodUnsupported'
  || error?.name === 'WebAppMethodUnsupported'
  || error?.message === 'WebAppMethodUnsupported'
);

export function cryptogram_serverless_call(endpoint, input) {
  return new Promise((resolve, reject) => {
    const app = globalThis.Telegram && globalThis.Telegram.WebApp;
    if (!app || !app.Serverless || typeof app.Serverless.call !== 'function') {
      reject(new Error('Telegram Serverless is unavailable'));
      return;
    }
    app.Serverless.call(endpoint, input, (error, result) => {
      if (error) {
        reject(error);
      } else {
        resolve(result);
      }
    });
  });
}

export function cryptogram_secure_get(key) {
  return new Promise((resolve, reject) => {
    const app = globalThis.Telegram && globalThis.Telegram.WebApp;
    if (!app || !app.SecureStorage || typeof app.SecureStorage.getItem !== 'function') {
      resolve({ value: null, available: false });
      return;
    }
    try {
      app.SecureStorage.getItem(key, (error, value, canRestore) => {
        if (error) {
          if (cryptogram_secure_storage_unsupported(error)) {
            resolve({ value: null, available: false });
          } else {
            reject(error);
          }
          return;
        }
        if ((value === null || value === undefined) && canRestore) {
          try {
            app.SecureStorage.restoreItem(key, (restoreError, restored) => {
              if (restoreError) {
                if (cryptogram_secure_storage_unsupported(restoreError)) {
                  resolve({ value: null, available: false });
                } else {
                  reject(restoreError);
                }
              } else {
                resolve({ value: restored === undefined ? null : restored, available: true });
              }
            });
          } catch (restoreError) {
            if (cryptogram_secure_storage_unsupported(restoreError)) {
              resolve({ value: null, available: false });
            } else {
              reject(restoreError);
            }
          }
        } else {
          resolve({ value: value === undefined ? null : value, available: true });
        }
      });
    } catch (error) {
      if (cryptogram_secure_storage_unsupported(error)) {
        resolve({ value: null, available: false });
      } else {
        reject(error);
      }
    }
  });
}

export function cryptogram_secure_set(key, value) {
  return new Promise((resolve, reject) => {
    const app = globalThis.Telegram && globalThis.Telegram.WebApp;
    if (!app || !app.SecureStorage || typeof app.SecureStorage.setItem !== 'function') {
      resolve(false);
      return;
    }
    try {
      app.SecureStorage.setItem(key, value, (error) => {
        if (error) {
          reject(error);
        } else {
          resolve(true);
        }
      });
    } catch (error) {
      if (cryptogram_secure_storage_unsupported(error)) {
        resolve(false);
      } else {
        reject(error);
      }
    }
  });
}

export function cryptogram_secure_remove(key) {
  return new Promise((resolve, reject) => {
    const app = globalThis.Telegram && globalThis.Telegram.WebApp;
    if (!app || !app.SecureStorage || typeof app.SecureStorage.removeItem !== 'function') {
      resolve(false);
      return;
    }
    try {
      app.SecureStorage.removeItem(key, (error) => {
        if (error) {
          reject(error);
        } else {
          resolve(true);
        }
      });
    } catch (error) {
      if (cryptogram_secure_storage_unsupported(error)) {
        resolve(false);
      } else {
        reject(error);
      }
    }
  });
}
"#)]
extern "C" {
    #[wasm_bindgen(js_name = cryptogram_serverless_call)]
    fn serverless_call_js(endpoint: &str, input: JsValue) -> JsValue;

    #[wasm_bindgen(js_name = cryptogram_secure_get)]
    fn secure_get_js(key: &str) -> JsValue;

    #[wasm_bindgen(js_name = cryptogram_secure_set)]
    fn secure_set_js(key: &str, value: &str) -> JsValue;

    #[wasm_bindgen(js_name = cryptogram_secure_remove)]
    fn secure_remove_js(key: &str) -> JsValue;
}

use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;

use crate::error::{Error, Result};

pub async fn serverless_call(endpoint: &str, input: JsValue) -> Result<JsValue> {
    let promise = serverless_call_js(endpoint, input);
    let promise_value = promise
        .dyn_into::<js_sys::Promise>()
        .map_err(|_| Error::Relay("Telegram Serverless did not return a promise".to_owned()))?;
    JsFuture::from(promise_value)
        .await
        .map_err(|error| Error::Relay(format!("Telegram Serverless call failed: {error:?}")))
}

pub async fn secure_get(key: &str) -> Result<Option<String>> {
    let promise = secure_get_js(key)
        .dyn_into::<js_sys::Promise>()
        .map_err(|_| Error::Vault("Telegram SecureStorage did not return a promise".to_owned()))?;
    let result_value = JsFuture::from(promise)
        .await
        .map_err(|error| Error::Vault(format!("Telegram SecureStorage read failed: {error:?}")))?;
    let available =
        js_sys::Reflect::get(&result_value, &JsValue::from_str("available")).map_err(|error| {
            Error::Vault(format!(
                "Telegram SecureStorage result is invalid: {error:?}"
            ))
        })?;
    match available.as_bool() {
        Some(true) => {}
        Some(false) => {
            return Err(Error::Unsupported(
                "Telegram SecureStorage is available only in the web build",
            ));
        }
        None => {
            return Err(Error::Vault(
                "Telegram SecureStorage returned an invalid availability flag".to_owned(),
            ));
        }
    }
    let secure_value =
        js_sys::Reflect::get(&result_value, &JsValue::from_str("value")).map_err(|error| {
            Error::Vault(format!(
                "Telegram SecureStorage result is invalid: {error:?}"
            ))
        })?;
    if secure_value.is_null() || secure_value.is_undefined() {
        Ok(None)
    } else {
        secure_value
            .as_string()
            .map(Some)
            .ok_or_else(|| Error::Vault("Telegram SecureStorage returned a non-string".to_owned()))
    }
}

pub async fn secure_set(key: &str, value: &str) -> Result<()> {
    let promise = secure_set_js(key, value)
        .dyn_into::<js_sys::Promise>()
        .map_err(|_| Error::Vault("Telegram SecureStorage did not return a promise".to_owned()))?;
    let result = JsFuture::from(promise)
        .await
        .map_err(|error| Error::Vault(format!("Telegram SecureStorage write failed: {error:?}")))?;
    match result.as_bool() {
        Some(true) => Ok(()),
        Some(false) => Err(Error::Unsupported(
            "Telegram SecureStorage is available only in the web build",
        )),
        None => Err(Error::Vault(
            "Telegram SecureStorage returned an invalid result".to_owned(),
        )),
    }
}

pub async fn secure_remove(key: &str) -> Result<()> {
    let promise = secure_remove_js(key)
        .dyn_into::<js_sys::Promise>()
        .map_err(|_| Error::Vault("Telegram SecureStorage did not return a promise".to_owned()))?;
    let result = JsFuture::from(promise).await.map_err(|error| {
        Error::Vault(format!("Telegram SecureStorage removal failed: {error:?}"))
    })?;
    match result.as_bool() {
        Some(true) => Ok(()),
        Some(false) => Err(Error::Unsupported(
            "Telegram SecureStorage is available only in the web build",
        )),
        None => Err(Error::Vault(
            "Telegram SecureStorage returned an invalid result".to_owned(),
        )),
    }
}

#[must_use]
pub fn current_invitation() -> Option<Result<crate::protocol::Invitation>> {
    web_sys::window()
        .and_then(|window| window.location().hash().ok())
        .filter(|hash| !hash.is_empty())
        .map(|hash| crate::protocol::Invitation::from_telegram_fragment(&hash))
}

#[wasm_bindgen(start)]
pub fn start() -> std::result::Result<(), JsValue> {
    functora_egui::web::runner::launch("the_canvas_id", |cc| {
        Ok(Box::new(crate::CryptogramApp::new(cc)) as Box<dyn eframe::App>)
    })
}
