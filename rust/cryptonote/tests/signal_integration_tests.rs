use dioxus::prelude::*;

#[test]
fn nav_write_signal_conversion() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<u32> = Signal::new(0);
        let write: WriteSignal<u32> = signal.into();
        assert_eq!(write(), 0);

        rsx! { "" }
    });

    dom.rebuild_in_place();
}

#[test]
fn write_clipboard_message_conversion() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<Option<String>> = Signal::new(None);
        let write: WriteSignal<Option<String>> = signal.into();
        assert!(write().is_none());

        rsx! { "" }
    });

    dom.rebuild_in_place();
}
