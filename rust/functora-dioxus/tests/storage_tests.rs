use functora_dioxus::storage::files_dir;
use std::fmt::Debug;

fn ok<T, E>(result: Result<T, E>) -> T
where
    E: Debug,
{
    match result {
        Ok(value) => value,
        Err(error) => panic!("expected Ok, got: {error:?}"),
    }
}

#[test]
fn test_files_dir_not_fails_on_non_android() {
    #[cfg(not(target_os = "android"))]
    {
        let result = files_dir();
        assert!(result.is_ok());
        let path = ok(result);
        assert!(path.exists());
    }
}
