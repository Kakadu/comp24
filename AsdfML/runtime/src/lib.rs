#![feature(box_as_ptr)]
#![allow(clippy::missing_safety_doc)]

use log::LevelFilter;
mod closure;
mod ml_list;
mod tuple;
use std::{
    io::Write,
    ops::{Deref, DerefMut},
    rc::Rc,
};
mod cons_list;
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

pub trait WithRaw: Deref {
    type Ptr;

    fn into_raw(self) -> *const Self::Target;
    unsafe fn from_raw(ptr: *const Self::Target) -> Self;

    unsafe fn with_raw_ref<T, Fun, Out>(ptr: *const T, func: Fun) -> Out
    where
        Fun: Fn(&T) -> Out,
        Self::Ptr: WithRaw<Target = T>, {
        let x = Self::Ptr::from_raw(ptr);
        let res = func(&x);
        let _ = Self::Ptr::into_raw(x);
        res
    }

    unsafe fn with_raw_mut<T, Fun, Out>(ptr: *mut T, func: Fun) -> Out
    where
        Fun: Fn(&mut T) -> Out,
        Self::Ptr: WithRaw<Target = T> + DerefMut, {
        let mut x = Self::Ptr::from_raw(ptr);
        let res = func(&mut x);
        let _ = Self::Ptr::into_raw(x);
        res
    }
}

impl<T> WithRaw for Rc<T> {
    type Ptr = Self;

    fn into_raw(self) -> *const Self::Target { Rc::into_raw(self) }

    unsafe fn from_raw(ptr: *const Self::Target) -> Self { Rc::from_raw(ptr) }
}

impl<T> WithRaw for Box<T> {
    type Ptr = Self;

    fn into_raw(self) -> *const Self::Target { Box::into_raw(self) }

    unsafe fn from_raw(ptr: *const Self::Target) -> Self { Box::from_raw(ptr as *mut _) }
}
