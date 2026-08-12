use crate::android::dispatch::jni_dispatch_with_webview;
use crate::error::Error;

pub async fn print_page() -> Result<(), Error> {
    jni_dispatch_with_webview(move |env, activity, webview| {
        if webview.as_raw().is_null() {
            return Err(jni::errors::Error::NullPtr("webview is null"));
        }
        let svc_name = env.new_string("print")?;
        let print_manager = env
            .call_method(
                activity,
                "getSystemService",
                "(Ljava/lang/String;)Ljava/lang/Object;",
                &[(&svc_name).into()],
            )?
            .l()?;
        if print_manager.as_raw().is_null() {
            return Err(jni::errors::Error::NullPtr("PrintManager not available"));
        }
        let job_name = env.new_string("Cryptonote")?;
        let print_adapter = env
            .call_method(
                webview,
                "createPrintDocumentAdapter",
                "(Ljava/lang/String;)Landroid/print/PrintDocumentAdapter;",
                &[(&job_name).into()],
            )?
            .l()?;
        if print_adapter.as_raw().is_null() {
            return Err(jni::errors::Error::NullPtr("PrintDocumentAdapter not available"));
        }
        let builder_class = env.find_class("android/print/PrintAttributes$Builder")?;
        let builder = env.new_object(builder_class, "()V", &[])?;
        let print_attributes = env
            .call_method(builder, "build", "()Landroid/print/PrintAttributes;", &[])?
            .l()?;
        let _ = env.call_method(
            &print_manager,
            "print",
            "(Ljava/lang/String;Landroid/print/PrintDocumentAdapter;Landroid/print/PrintAttributes;)Landroid/print/PrintJob;",
            &[(&job_name).into(), (&print_adapter).into(), (&print_attributes).into()],
        )?;
        Ok(())
    })
}
