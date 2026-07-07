pub(crate) fn bool_attr(val: bool) -> Option<&'static str> {
    val.then_some("")
}

pub(crate) fn overflow_style(overflow: bool) -> Option<&'static str> {
    (!overflow).then_some("pre-wrap")
}

pub mod banner;
pub mod breadcrumb;
pub mod button;
pub mod dock;
pub mod extlink;
pub mod nav;
pub mod par;
pub mod pre;
pub mod qr_scanner;
pub mod quote;
pub mod static_page;

pub use banner::*;
pub use breadcrumb::*;
pub use button::*;
pub use dock::*;
pub use extlink::*;
pub use nav::*;
pub use par::*;
pub use pre::*;
pub use qr_scanner::*;
pub use quote::*;
pub use static_page::*;

pub use crate::nav::*;
