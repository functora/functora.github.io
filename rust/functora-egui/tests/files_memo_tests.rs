use functora_egui::files::BlobMemo;

#[test]
fn insert_replacement_returns_previous_url() {
    let mut memo = BlobMemo::default();
    assert_eq!(
        memo.insert("a.png", 1, "blob:one".to_owned()),
        Vec::<String>::new()
    );
    let removed = memo.insert("a.png", 1, "blob:two".to_owned());
    assert_eq!(removed, vec!["blob:one".to_owned()]);
    assert_eq!(memo.get("a.png", 1), Some("blob:two"));
}

#[test]
fn insert_skips_data_urls() {
    let mut memo = BlobMemo::default();
    let removed = memo.insert("a.png", 1, "data:image/png;base64,AAAA".to_owned());
    assert_eq!(removed, Vec::<String>::new());
    assert_eq!(memo.get("a.png", 1), None);
}

#[test]
fn insert_evicts_oldest_beyond_capacity() {
    let mut memo = BlobMemo::default();
    let removed_urls: Vec<String> = (0..130u64)
        .flat_map(|i| memo.insert("a.png", i, format!("blob:{i}")))
        .collect();
    assert_eq!(removed_urls, vec!["blob:0".to_owned(), "blob:1".to_owned()]);
    assert_eq!(memo.get("a.png", 0), None);
    assert_eq!(memo.get("a.png", 129), Some("blob:129"));
}

#[test]
fn forget_still_removes_entries() {
    let mut memo = BlobMemo::default();
    assert_eq!(
        memo.insert("a.png", 1, "blob:one".to_owned()),
        Vec::<String>::new()
    );
    assert_eq!(memo.forget("blob:one"), 1);
    assert_eq!(memo.get("a.png", 1), None);
}
