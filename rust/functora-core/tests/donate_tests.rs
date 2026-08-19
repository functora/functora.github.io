use functora_core::i18n::{I18N, Language};
use functora_core::{Msg, donate_blocks};

#[test]
fn donate_blocks_have_default_labels_and_addresses() {
    let blocks = donate_blocks();
    assert_eq!(blocks.len(), 2);
    assert_eq!(blocks[0].label, "BTC - Bitcoin");
    assert_eq!(
        blocks[0].address,
        "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q"
    );
    assert_eq!(blocks[1].label, "XMR - Monero");
    assert_eq!(
        blocks[1].address,
        "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG"
    );
}

#[test]
fn donate_messages_render_all_languages() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert!(!Msg::DonateGreeting.render(lang).is_empty());
        assert!(Msg::DonateIntro.render(lang).contains("Functora"));
    }
}
