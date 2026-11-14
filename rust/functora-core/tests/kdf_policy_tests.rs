#![cfg(feature = "crypto")]

use functora_core::crypto::{Kdf, derive_key};

/// The Argon2id cost is a compile-time constant of the library. A debug
/// build must not accept a cheaper cost from the environment, because the
/// ciphertext is public and every cheap guess is an offline brute-force
/// attempt against the note.
#[test]
fn kdf_parameters_ignore_the_environment_override() {
    let salt = vec![7u8; 32];
    let baseline =
        derive_key("pw", &salt, Kdf::Argon2id).unwrap_or_else(|e| panic!("derive: {e:?}"));
    unsafe {
        std::env::set_var("FUNCTORA_KDF_M_COST_KIB", "8");
        std::env::set_var("FUNCTORA_KDF_T_COST", "1");
    }
    let overridden =
        derive_key("pw", &salt, Kdf::Argon2id).unwrap_or_else(|e| panic!("derive: {e:?}"));
    unsafe {
        std::env::remove_var("FUNCTORA_KDF_M_COST_KIB");
        std::env::remove_var("FUNCTORA_KDF_T_COST");
    }
    assert_eq!(
        baseline.as_slice(),
        overridden.as_slice(),
        "the environment must not change the key derivation cost"
    );
}
