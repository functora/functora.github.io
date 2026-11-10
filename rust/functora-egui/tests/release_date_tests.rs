//! `release_date` must never panic on any platform.
//!
//! The browser build has no `std` clock, so `SystemTime::now()` aborts the
//! whole app. The release date is therefore derived from a fallible clock
//! reading: when the platform has no clock the Unix epoch is used instead.

#![cfg(any(feature = "build", feature = "desktop"))]

use functora_egui::desktop::config::{
    iso_date_from_days_since_epoch, release_date, release_date_from,
};

#[test]
fn release_date_falls_back_to_the_epoch_without_a_clock() {
    assert_eq!(release_date_from(None), "1970-01-01");
    assert_eq!(release_date_from(None), iso_date_from_days_since_epoch(0));
}

#[test]
fn release_date_uses_the_clock_reading() {
    assert_eq!(release_date_from(Some(0)), "1970-01-01");
    assert_eq!(release_date_from(Some(86_400)), "1970-01-02");
    assert_eq!(release_date_from(Some(1_700_000_000)), "2023-11-14");
}

#[test]
fn release_date_is_an_iso_date() {
    let date = release_date();
    assert_eq!(date.len(), 10);
    assert_eq!(date.as_bytes()[4], b'-');
    assert_eq!(date.as_bytes()[7], b'-');
    assert!(
        date.bytes()
            .enumerate()
            .filter(|(i, _)| *i != 4 && *i != 7)
            .all(|(_, b)| b.is_ascii_digit()),
        "unexpected date: {date}"
    );
}
