use crate::android::dispatch::jni_dispatch;
use crate::error::Error;
use crate::ffi::ShareData;

pub async fn social_share(data: ShareData) -> Result<(), Error> {
    jni_dispatch(move |env, activity| {
        let intent_class = env.find_class("android/content/Intent")?;
        let action_send = env
            .get_static_field(&intent_class, "ACTION_SEND", "Ljava/lang/String;")?
            .l()?;
        let intent = env.new_object(&intent_class, "(Ljava/lang/String;)V", &[(&action_send).into()])?;
        let intent = env.new_global_ref(intent)?;
        let text_type = env.new_string("text/plain")?;
        let _ = env.call_method(
            intent.as_obj(),
            "setType",
            "(Ljava/lang/String;)Landroid/content/Intent;",
            &[(&text_type).into()],
        )?;
        let extra_text = env
            .get_static_field(&intent_class, "EXTRA_TEXT", "Ljava/lang/String;")?
            .l()?;
        let share_text = env.new_string(format!("{}\n{}", data.text, data.url))?;
        let _ = env.call_method(
            intent.as_obj(),
            "putExtra",
            "(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;",
            &[(&extra_text).into(), (&share_text).into()],
        )?;
        let extra_subject = env
            .get_static_field(&intent_class, "EXTRA_SUBJECT", "Ljava/lang/String;")?
            .l()?;
        let title = env.new_string(&data.title)?;
        let _ = env.call_method(
            intent.as_obj(),
            "putExtra",
            "(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;",
            &[(&extra_subject).into(), (&title).into()],
        )?;
        let chooser_title = env.new_string("Share via")?;
        let chooser = env.call_static_method(
            "android/content/Intent",
            "createChooser",
            "(Landroid/content/Intent;Ljava/lang/CharSequence;)Landroid/content/Intent;",
            &[(&intent.as_obj()).into(), (&chooser_title).into()],
        )?;
        let chooser = chooser.l()?;
        let flags = env
            .get_static_field(&intent_class, "FLAG_ACTIVITY_NEW_TASK", "I")?
            .i()?;
        let _ = env.call_method(
            intent.as_obj(),
            "setFlags",
            "(I)Landroid/content/Intent;",
            &[jni::objects::JValue::Int(flags)],
        )?;
        let _ = env.call_method(
            activity,
            "startActivity",
            "(Landroid/content/Intent;)V",
            &[(&chooser).into()],
        )?;
        Ok(())
    })
}
