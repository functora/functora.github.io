#[unsafe(no_mangle)]
pub extern "system" fn Java_dev_dioxus_main_MainActivity_handleDeepLink<'local>(
    mut env: jni::JNIEnv<'local>,
    _class: jni::objects::JClass<'local>,
    url: jni::objects::JString<'local>,
) {
    if let Ok(s) = env.get_string(&url) {
        crate::deep_link::store_url(s.into());
    }
}
