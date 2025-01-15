#![feature(box_as_ptr)]
#![allow(clippy::missing_safety_doc)]

use log::LevelFilter;
mod closure;
mod ml_list;
mod tuple;
use std::io::Write;
mod list;
mod ops;

pub use closure::*;
pub use ml_list::*;
pub use ops::*;
pub use tuple::*;

/// Set up the logger with the default log level of `info`.
/// This can be overridden by setting the `RUST_LOG` environment variable.
#[no_mangle]
pub extern "C" fn runtime_init() {
    env_logger::Builder::new()
        .format(|buf, record| writeln!(buf, "[{}]: {}", record.target(), record.args()))
        .filter_level(LevelFilter::Info)
        .parse_default_env()
        .try_init()
        .unwrap();
}

unsafe fn with_raw<T, F, R>(ptr: *mut T, func: F) -> R
where F: Fn(&T) -> R {
    let x = Box::from_raw(ptr);
    let res = func(&x);
    let _ = Box::into_raw(x);
    res
}

unsafe fn with_raw_mut<T, F, R>(ptr: *mut T, func: F) -> R
where F: Fn(&mut T) -> R {
    let mut x = Box::from_raw(ptr);
    let res = func(&mut x);
    let _ = Box::into_raw(x);
    res
}
