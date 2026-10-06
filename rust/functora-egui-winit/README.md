# functora-egui-winit

[![Latest version](https://img.shields.io/crates/v/functora-egui-winit.svg)](https://crates.io/crates/functora-egui-winit)
[![Documentation](https://docs.rs/functora-egui-winit/badge.svg)](https://docs.rs/functora-egui-winit)
![MIT](https://img.shields.io/badge/license-MIT-blue.svg)
![Apache](https://img.shields.io/badge/license-Apache-blue.svg)

Fork of [`egui-winit`](https://github.com/emilk/egui/tree/main/crates/egui-winit) with an Android IME fix.

This crates provides bindings between [`egui`](https://github.com/emilk/egui) and [`winit`](https://crates.io/crates/winit).

The library translates winit events to egui, handled copy/paste, updates the cursor, open links clicked in egui, etc.

## Differences from upstream

- On Android, `set_ime_allowed` directly shows/hides the soft keyboard. The fork re-asserts it on a fresh press of a focused text field, so the keyboard reappears even when the user dismissed it with a gesture, without the show/hide cycle that would make it flicker.
