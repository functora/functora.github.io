//! Mobile-first responsive layout support.
//!
//! Mirrors the breakpoint and spacing conventions of `functora-css`:
//! viewports narrower than 800px (the `@mobile: 50rem` breakpoint) use the
//! `Mobile` sizing scale, wider ones use the `Desktop` scale.

pub mod breakpoint;
pub mod responsive_ext;
pub mod spacing;

pub use breakpoint::Breakpoint;
pub use responsive_ext::ResponsiveExt;
pub use spacing::Spacing;
