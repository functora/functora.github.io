use crate::android::dispatch::jni_dispatch;
use crate::error::Error;

pub fn get_files_dir() -> Result<std::path::PathBuf, Error> {
    use jni::objects::JString;
    jni_dispatch(|env, activity| {
        env.call_method(activity, "getFilesDir", "()Ljava/io/File;", &[])
            .and_then(|v| v.l())
            .and_then(|f| env.call_method(f, "getAbsolutePath", "()Ljava/lang/String;", &[]))
            .and_then(|v| v.l())
            .map(JString::from)
            .and_then(|s| env.get_string(&s).map(String::from))
            .map(std::path::PathBuf::from)
    })
}
