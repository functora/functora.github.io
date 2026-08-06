pub(crate) fn bool_attr(val: bool) -> Option<&'static str> {
    val.then_some("")
}

pub mod attachment_uploader;
pub mod banner;
pub mod breadcrumb;
pub mod button;
pub mod dock;
pub mod extlink;
pub mod job_progress_bar;
pub mod nav;
pub mod par;
pub mod pre;
pub mod qr_scanner;
pub mod static_page;

pub use attachment_uploader::*;
pub use banner::*;
pub use breadcrumb::*;
pub use button::*;
pub use dock::*;
pub use extlink::*;
pub use job_progress_bar::*;
pub use nav::*;
pub use par::*;
pub use pre::*;
pub use qr_scanner::*;
pub use static_page::*;

pub use crate::nav::*;
