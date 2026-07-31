use cryptonote::archive::Attachment;
use cryptonote::{add_attachment, format_size};

#[test]
fn add_attachment_unique_name() {
    let mut atts = Vec::new();
    add_attachment(
        &mut atts,
        Attachment {
            name: "a.txt".into(),
            data: b"one".to_vec(),
        },
    );
    add_attachment(
        &mut atts,
        Attachment {
            name: "b.txt".into(),
            data: b"two".to_vec(),
        },
    );
    assert_eq!(atts.len(), 2);
    assert_eq!(atts[0].name, "a.txt");
    assert_eq!(atts[1].name, "b.txt");
}

#[test]
fn add_attachment_duplicate_name_replaces() {
    let mut atts = vec![Attachment {
        name: "a.txt".into(),
        data: b"old".to_vec(),
    }];
    add_attachment(
        &mut atts,
        Attachment {
            name: "a.txt".into(),
            data: b"new".to_vec(),
        },
    );
    assert_eq!(atts.len(), 1);
    assert_eq!(atts[0].name, "a.txt");
    assert_eq!(atts[0].data, b"new");
}

#[test]
fn add_attachment_duplicate_name_mixed_order() {
    let mut atts = vec![
        Attachment {
            name: "a.txt".into(),
            data: b"one".to_vec(),
        },
        Attachment {
            name: "b.txt".into(),
            data: b"two".to_vec(),
        },
    ];
    add_attachment(
        &mut atts,
        Attachment {
            name: "a.txt".into(),
            data: b"one-replaced".to_vec(),
        },
    );
    add_attachment(
        &mut atts,
        Attachment {
            name: "b.txt".into(),
            data: b"two-replaced".to_vec(),
        },
    );
    assert_eq!(atts.len(), 2);
    assert_eq!(atts[0].name, "a.txt");
    assert_eq!(atts[0].data, b"one-replaced");
    assert_eq!(atts[1].name, "b.txt");
    assert_eq!(atts[1].data, b"two-replaced");
}

#[test]
fn zero_bytes() {
    assert_eq!(format_size(0), "0 B");
}

#[test]
fn single_byte() {
    assert_eq!(format_size(1), "1 B");
}

#[test]
fn max_bytes() {
    assert_eq!(format_size(1023), "1023 B");
}

#[test]
fn exactly_one_kb() {
    assert_eq!(format_size(1024), "1.0 KB");
}

#[test]
fn fractional_kb() {
    assert_eq!(format_size(1536), "1.5 KB");
}

#[test]
fn nearly_one_mb() {
    assert_eq!(format_size(1048575), "1024.0 KB");
}

#[test]
fn exactly_one_mb() {
    assert_eq!(format_size(1048576), "1.0 MB");
}

#[test]
fn fractional_mb() {
    assert_eq!(format_size(2097152), "2.0 MB");
}

#[test]
fn large_mb_value() {
    assert_eq!(format_size(10_485_760), "10.0 MB");
}

#[test]
fn large_size() {
    let result = format_size(1_073_741_824);
    assert!(result.ends_with(" MB"));
}

#[test]
fn precision_half_mb() {
    assert_eq!(format_size(1572864), "1.5 MB");
}
