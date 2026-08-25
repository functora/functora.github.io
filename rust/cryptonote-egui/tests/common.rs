#![allow(dead_code)]

pub fn fast_kdf() {
    if std::env::var("FUNCTORA_KDF_M_COST_KIB").is_err() {
        unsafe {
            std::env::set_var("FUNCTORA_KDF_M_COST_KIB", "1024");
            std::env::set_var("FUNCTORA_KDF_T_COST", "1");
        }
    }
}
