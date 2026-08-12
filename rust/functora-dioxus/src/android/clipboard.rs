use crate::android::dispatch::jni_dispatch;
use crate::error::Error;

pub async fn read_clipboard() -> Result<String, Error> {
    jni_dispatch(|env, activity| {
        use jni::objects::JString;
        let svc_name: JString = env.new_string("clipboard")?;
        let clipboard_svc = env
            .call_method(
                activity,
                "getSystemService",
                "(Ljava/lang/String;)Ljava/lang/Object;",
                &[(&svc_name).into()],
            )?
            .l()?;
        let clipboard = env.new_global_ref(clipboard_svc)?;
        let clip = env
            .call_method(
                clipboard.as_obj(),
                "getPrimaryClip",
                "()Landroid/content/ClipData;",
                &[],
            )?
            .l()?;
        let item = env
            .call_method(
                &clip,
                "getItemAt",
                "(I)Landroid/content/ClipData$Item;",
                &[jni::objects::JValue::Int(0)],
            )?
            .l()?;
        let text_obj = env
            .call_method(&item, "getText", "()Ljava/lang/CharSequence;", &[])?
            .l()?;
        let s = JString::from(text_obj);
        Ok(env.get_string(&s).map(String::from)?)
    })
}

pub async fn clipboard_write(msg: String) -> Result<(), Error> {
    jni_dispatch(move |env, activity| {
        use jni::objects::JString;
        let label: JString = env.new_string("Cryptonote")?;
        let text: JString = env.new_string(&msg)?;
        let svc_name: JString = env.new_string("clipboard")?;
        let clipboard_svc = env
            .call_method(
                activity,
                "getSystemService",
                "(Ljava/lang/String;)Ljava/lang/Object;",
                &[(&svc_name).into()],
            )?
            .l()?;
        let clipboard = env.new_global_ref(clipboard_svc)?;
        let clip_data = env
            .call_static_method(
                "android/content/ClipData",
                "newPlainText",
                "(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Landroid/content/ClipData;",
                &[(&label).into(), (&text).into()],
            )?
            .l()?;
        let _ = env.call_method(
            clipboard.as_obj(),
            "setPrimaryClip",
            "(Landroid/content/ClipData;)V",
            &[(&clip_data).into()],
        )?;
        Ok(())
    })
}
