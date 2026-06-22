use dioxus::prelude::*;
use functora_dioxus::PersistentSignal;

#[test]
fn signal_to_write_signal_conversion() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<i32> = Signal::new(42);
        let write: WriteSignal<i32> = signal.into();
        assert_eq!(write(), 42);

        rsx! { "" }
    });

    dom.rebuild_in_place();
}

#[test]
fn signal_to_read_signal_conversion() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<String> = Signal::new("hello".to_string());
        let read: ReadSignal<String> = signal.into();
        assert_eq!(read(), "hello");

        rsx! { "" }
    });

    dom.rebuild_in_place();
}

#[test]
fn write_signal_to_read_signal_conversion() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<i32> = Signal::new(99);
        let write: WriteSignal<i32> = signal.into();
        let read: ReadSignal<i32> = write.into();
        assert_eq!(read(), 99);

        rsx! { "" }
    });

    dom.rebuild_in_place();
}

#[test]
fn write_signal_set_and_read() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<i32> = Signal::new(0);
        let mut write: WriteSignal<i32> = signal.into();
        write.set(42);
        assert_eq!(signal(), 42);

        rsx! { "" }
    });

    dom.rebuild_in_place();
}

#[test]
fn write_signal_with_mut() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<Vec<i32>> = Signal::new(Vec::new());
        let mut write: WriteSignal<Vec<i32>> = signal.into();
        write.with_mut(|v| v.push(1));
        write.with_mut(|v| v.push(2));
        assert_eq!(&*signal(), &[1, 2]);

        rsx! { "" }
    });

    dom.rebuild_in_place();
}

#[test]
fn read_signal_callable() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<String> = Signal::new("test".to_string());
        let read: ReadSignal<String> = signal.into();
        let val = read();
        assert_eq!(val, "test");

        rsx! { "" }
    });

    dom.rebuild_in_place();
}

#[test]
fn persistent_signal_readable_trait() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<u32> = Signal::new(123);
        let ps = PersistentSignal::new(signal, "test_readable");
        let val: u32 = *ps.read();
        assert_eq!(val, 123);

        rsx! { "" }
    });

    dom.rebuild_in_place();
}

#[test]
fn write_signal_partial_eq() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<i32> = Signal::new(5);
        let write1: WriteSignal<i32> = signal.into();
        let write2: WriteSignal<i32> = signal.into();
        assert_eq!(write1, write2);

        rsx! { "" }
    });

    dom.rebuild_in_place();
}

#[test]
fn read_signal_from_mapped_signal() {
    let mut dom = VirtualDom::new(|| {
        let signal: Signal<Vec<i32>> = Signal::new(vec![10, 20, 30]);
        let first = signal.map(|v| v.first().unwrap());
        let read: ReadSignal<i32> = first.into();
        assert_eq!(read(), 10);

        rsx! { "" }
    });

    dom.rebuild_in_place();
}
