#![feature(box_as_ptr)]

use env_logger;
use log::LevelFilter;
mod closure;
mod list;
mod tuple;
use std::io::Write;
mod ops;

pub use closure::*;
pub use list::*;
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
