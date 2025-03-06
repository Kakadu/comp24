use core::panic;
use std::{
    cmp::Ordering,
    ffi::c_void,
    fmt::{self, Debug, Formatter},
};

use backtrace::{self, Symbol};
use log::debug;

use crate::Tuple;

#[derive(Clone)]
pub struct Closure {
    pub(crate) fn_ptr: *const fn(),
    pub(crate) arity: usize,
    pub(crate) args: Vec<isize>,
}

impl Debug for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut fn_name = None;
        backtrace::resolve(self.fn_ptr as *mut c_void, |symbol: &Symbol| {
            fn_name = symbol.name().map(|name| name.to_string());
        });
        let fn_info = match fn_name {
            Some(name) => format!("{:?} ({})", &self.fn_ptr, name),
            None => format!("{:?}", self.fn_ptr),
        };

        let args_info = format!("dec{:?} / hex{:x?}", self.args, self.args);

        f.debug_struct("Closure")
            .field("fn_ptr", &format_args!("{}", fn_info))
            .field("arity", &self.arity)
            .field("args", &format_args!("{}", args_info))
            .finish()
    }
}

type Fn1 = fn(isize) -> isize;
type Fn2 = fn(isize, isize) -> isize;
type Fn3 = fn(isize, isize, isize) -> isize;
type Fn4 = fn(isize, isize, isize, isize) -> isize;
type Fn5 = fn(isize, isize, isize, isize, isize) -> isize;
type Fn6 = fn(isize, isize, isize, isize, isize, isize) -> isize;
type Fn7 = fn(isize, isize, isize, isize, isize, isize, isize) -> isize;
type Fn8 = fn(isize, isize, isize, isize, isize, isize, isize, isize) -> isize;

impl Closure {
    #[export_name = "create_closure"]
    pub extern "C" fn new(fn_ptr: *const fn(), arity: usize) -> *mut Closure {
        let closure = Box::new(Closure {
            fn_ptr,
            arity,
            args: Vec::with_capacity(arity),
        });
        debug!("Creating {:?} at {:?}", closure, Box::as_ptr(&closure));
        Box::into_raw(closure)
    }

    fn apply(&mut self) -> Option<isize> {
        debug!("Applying {:?}", self);
        match self.args.len().cmp(&self.arity) {
            Ordering::Less => {
                debug!("Not enough args");
                None
            }
            Ordering::Greater => {
                let rest = self.args.split_off(self.arity);
                debug!("Too many args, {:?} goes to next closure", rest);
                if let Some(res) = self.apply() {
                    let mut new_closure = unsafe { Box::from_raw(res as *mut Closure) };
                    new_closure.args.extend(rest);
                    new_closure.apply()
                } else {
                    panic!("Closure does not returned a value");
                }
            }
            Ordering::Equal => {
                let res = match self.arity {
                    0 => panic!("Closure of arity 0"),
                    1 => {
                        let func: Fn1 = unsafe { std::mem::transmute(self.fn_ptr) };
                        func(self.args[0])
                    }
                    2 => {
                        let func: Fn2 = unsafe { std::mem::transmute(self.fn_ptr) };
                        func(self.args[0], self.args[1])
                    }
                    3 => {
                        let func: Fn3 = unsafe { std::mem::transmute(self.fn_ptr) };
                        func(self.args[0], self.args[1], self.args[2])
                    }
                    4 => {
                        let func: Fn4 = unsafe { std::mem::transmute(self.fn_ptr) };
                        func(self.args[0], self.args[1], self.args[2], self.args[3])
                    }
                    5 => {
                        let func: Fn5 = unsafe { std::mem::transmute(self.fn_ptr) };
                        #[rustfmt::skip]
                        func(self.args[0], self.args[1], self.args[2], self.args[3], self.args[4])
                    }
                    6 => {
                        let func: Fn6 = unsafe { std::mem::transmute(self.fn_ptr) };
                        #[rustfmt::skip]
                        func(self.args[0], self.args[1], self.args[2], self.args[3], self.args[4], self.args[5] )
                    }
                    7 => {
                        let func: Fn7 = unsafe { std::mem::transmute(self.fn_ptr) };
                        #[rustfmt::skip]
                        func(self.args[0], self.args[1], self.args[2], self.args[3], self.args[4], self.args[5], self.args[6])
                    }
                    8 => {
                        let func: Fn8 = unsafe { std::mem::transmute(self.fn_ptr) };
                        #[rustfmt::skip]
                        func(self.args[0], self.args[1], self.args[2], self.args[3], self.args[4], self.args[5], self.args[6], self.args[7])
                    }
                    9.. => {
                        let func: Fn8 = unsafe { std::mem::transmute(self.fn_ptr) };
                        let rest = Box::new(self.args.split_off(7));
                        #[rustfmt::skip]
                        let res = func(
                            self.args[0], self.args[1], self.args[2], self.args[3], self.args[4], self.args[5], self.args[6],
                            (Box::as_ptr(&rest)) as isize,
                        );
                        let _ = Box::into_raw(rest);
                        res
                    }
                };
                debug!("Closure result: {} / {:#x}", res, res);
                Some(res)
            }
        }
    }
}

macro_rules! apply_closure_n {
    ($fn_name:ident, $($arg:ident),*) => {
        #[no_mangle]
        pub unsafe extern "C" fn $fn_name(closure_ptr: *mut Closure, $($arg: isize),*) -> isize {
            let mut closure = Box::new((*closure_ptr).clone());
            closure.args.extend_from_slice(&[$($arg),*]);
            match closure.apply() {
                Some(res) => res,
                None => Box::into_raw(closure) as isize,
            }
        }
    };
}
apply_closure_n!(apply_closure_1, a1);
apply_closure_n!(apply_closure_2, a1, a2);
apply_closure_n!(apply_closure_3, a1, a2, a3);
apply_closure_n!(apply_closure_4, a1, a2, a3, a4);
apply_closure_n!(apply_closure_5, a1, a2, a3, a4, a5);
apply_closure_n!(apply_closure_6, a1, a2, a3, a4, a5, a6);
apply_closure_n!(apply_closure_7, a1, a2, a3, a4, a5, a6, a7);

#[no_mangle]
pub unsafe extern "C" fn apply_closure_9plus(
    closure_ptr: *mut Closure,
    a1: isize,
    a2: isize,
    a3: isize,
    a4: isize,
    a5: isize,
    a6: isize,
    tuple_ptr: *mut Tuple,
) -> isize {
    let mut closure = Box::new((*closure_ptr).clone());
    let tuple = Box::from_raw(tuple_ptr);
    debug!("Tuple with arguments {:?} at {:?}", tuple, tuple_ptr);
    closure.args.extend_from_slice(&[a1, a2, a3, a4, a5, a6]);
    closure.args.extend(tuple.iter());
    match closure.apply() {
        Some(res) => res,
        None => Box::into_raw(closure) as isize,
    }
}
