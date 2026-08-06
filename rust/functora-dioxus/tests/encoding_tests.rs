use functora_dioxus::encoding::{append_query_param, decode_payload, encode_payload, extract_query_param};

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
struct Payload {
    note: String,
}

#[test]
fn payload_roundtrip() {
    let payload = Payload {
        note: "hello +/= world".into(),
    };
    let encoded = encode_payload(&payload).expect("encode");
    assert!(!encoded.contains('+'));
    assert!(!encoded.contains('='));
    assert!(!encoded.contains('/'));
    let decoded: Payload = decode_payload(&encoded).expect("decode");
    assert_eq!(decoded, payload);
}

#[test]
fn decode_payload_invalid_base64_fails() {
    assert!(decode_payload::<Payload>("not-valid-base64!!!").is_err());
}

#[test]
fn decode_payload_invalid_json_fails() {
    assert!(decode_payload::<Payload>("_w").is_err());
}

#[test]
fn append_query_param_adds_question_mark_when_absent() {
    assert_eq!(
        append_query_param("https://example.com", "note", "abc"),
        "https://example.com?note=abc"
    );
}

#[test]
fn append_query_param_uses_ampersand_when_query_exists() {
    assert_eq!(
        append_query_param("https://example.com/?screen=view", "note", "a b"),
        "https://example.com/?screen=view&note=a%20b"
    );
}

#[test]
fn extract_query_param_returns_value() {
    let url = "https://example.com/?screen=view&note=abc%2Bdef";
    assert_eq!(extract_query_param(url, "note").as_deref(), Some("abc+def"));
}

#[test]
fn extract_query_param_missing_returns_none() {
    assert_eq!(extract_query_param("https://example.com/?screen=view", "note"), None);
    assert_eq!(extract_query_param("https://example.com/", "note"), None);
}

#[test]
fn extract_query_param_takes_first_and_keeps_equals_and_fragment() {
    assert_eq!(
        extract_query_param("https://example.com/?note=first&note=second", "note").as_deref(),
        Some("first")
    );
    assert_eq!(
        extract_query_param("https://example.com/?note=abc%3Ddef#frag", "note").as_deref(),
        Some("abc=def#frag")
    );
}

#[test]
fn extract_query_param_empty_value() {
    assert_eq!(
        extract_query_param("https://example.com/?note=", "note").as_deref(),
        Some("")
    );
}

#[test]
fn download_script_escapes_special_characters() {
    let script = functora_dioxus::encoding::download_script("a\"</script>").expect("script");
    assert!(!script.contains("</script>"));
    assert!(script.contains("a.download="));
}

#[cfg(feature = "qr")]
#[test]
fn generate_qr_code_produces_svg() {
    let svg = functora_dioxus::encoding::generate_qr_code("https://example.com").expect("qr");
    assert!(svg.starts_with("<svg"));
    assert!(svg.contains("viewBox"));
}

#[cfg(feature = "qr")]
#[test]
fn generate_qr_code_fails_on_empty_url() {
    assert!(functora_dioxus::encoding::generate_qr_code("").is_err());
}
