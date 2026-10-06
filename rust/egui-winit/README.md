# egui-winit (patch shim)

Internal `[patch.crates-io]` shim. It re-exports `functora-egui-winit` under the `egui-winit` package name so `eframe` keeps using the fork with the Android IME fix.

The real code lives in `../functora-egui-winit`. This crate is marked `publish = false` and must never be published.
