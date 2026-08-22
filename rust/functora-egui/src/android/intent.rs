use android_activity::AndroidApp;
use jni::JavaVM;
use jni::objects::{JObject, JString};
use jni::signature::ReturnType;

#[must_use]
pub fn get_data_string(app: &AndroidApp) -> Option<String> {
    let vm = unsafe { JavaVM::from_raw(app.vm_as_ptr().cast()) }.ok()?;
    let mut env = vm.attach_current_thread().ok()?;
    let activity: JObject = unsafe { JObject::from_raw(app.activity_as_ptr().cast()) };
    let activity_cls = env.get_object_class(&activity).ok()?;
    let get_intent = env
        .get_method_id(&activity_cls, "getIntent", "()Landroid/content/Intent;")
        .ok()?;
    let intent =
        unsafe { env.call_method_unchecked(&activity, get_intent, ReturnType::Object, &[]) }
            .ok()?
            .l()
            .ok()?;
    if intent.is_null() {
        return None;
    }
    let intent_cls = env.get_object_class(&intent).ok()?;
    let get_data = env
        .get_method_id(&intent_cls, "getDataString", "()Ljava/lang/String;")
        .ok()?;
    let data = unsafe { env.call_method_unchecked(&intent, get_data, ReturnType::Object, &[]) }
        .ok()?
        .l()
        .ok()?;
    if data.is_null() {
        return None;
    }
    let jstring = JString::from(data);
    let url = env.get_string(&jstring).ok()?;
    Some(String::from(url))
}
