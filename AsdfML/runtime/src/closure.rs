use std::cmp::Ordering;

use log::debug;

use crate::Tuple;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Closure {
    pub(crate) fn_ptr: *const fn(),
    pub(crate) arity: usize,
    pub(crate) args: Vec<isize>,
}

type Fn1 = fn(isize) -> isize;
type Fn2 = fn(isize, isize) -> isize;
type Fn3 = fn(isize, isize, isize) -> isize;
type Fn4 = fn(isize, isize, isize, isize) -> isize;
type Fn5 = fn(isize, isize, isize, isize, isize) -> isize;
type Fn6 = fn(isize, isize, isize, isize, isize, isize) -> isize;
type Fn7 = fn(isize, isize, isize, isize, isize, isize, isize) -> isize;
type Fn8 = fn(isize, isize, isize, isize, isize, isize, isize, isize) -> isize;

// TODO:
// - types (i/u size) + in lib
// - shitcode with apply_closure
// - pass args as tuple?

#[no_mangle]
pub extern "C" fn create_closure(fn_ptr: *const fn(), arity: usize) -> *mut Closure {
    let closure = Box::new(Closure {
        fn_ptr,
        arity,
        args: Vec::with_capacity(arity),
    });
    debug!("Creating {:?} at {:?}", closure, Box::as_ptr(&closure));
    Box::into_raw(closure)
}

fn apply_closure(closure: &mut Closure) -> Option<isize> {
    debug!("Applying {:?} / Hex args: {:x?}", closure, closure.args);
    match closure.args.len().cmp(&closure.arity) {
        Ordering::Less => {
            debug!("Not enough args");
            return None;
        }
        Ordering::Greater => {
            panic!(
                "Applied {} args to closure with arity {}",
                closure.args.len(),
                closure.arity
            )
        }
        Ordering::Equal => {}
    }
    let res = match closure.arity {
        1 => {
            let func: Fn1 = unsafe { std::mem::transmute(closure.fn_ptr) };
            func(closure.args[0])
        }
        2 => {
            let func: Fn2 = unsafe { std::mem::transmute(closure.fn_ptr) };
            func(closure.args[0], closure.args[1])
        }
        3 => {
            let func: Fn3 = unsafe { std::mem::transmute(closure.fn_ptr) };
            func(closure.args[0], closure.args[1], closure.args[2])
        }
        4 => {
            let func: Fn4 = unsafe { std::mem::transmute(closure.fn_ptr) };
            func(closure.args[0], closure.args[1], closure.args[2], closure.args[3])
        }
        5 => {
            let func: Fn5 = unsafe { std::mem::transmute(closure.fn_ptr) };
            func(
                closure.args[0],
                closure.args[1],
                closure.args[2],
                closure.args[3],
                closure.args[4],
            )
        }
        6 => {
            let func: Fn6 = unsafe { std::mem::transmute(closure.fn_ptr) };
            func(
                closure.args[0],
                closure.args[1],
                closure.args[2],
                closure.args[3],
                closure.args[4],
                closure.args[5],
            )
        }
        7 => {
            let func: Fn7 = unsafe { std::mem::transmute(closure.fn_ptr) };
            func(
                closure.args[0],
                closure.args[1],
                closure.args[2],
                closure.args[3],
                closure.args[4],
                closure.args[5],
                closure.args[6],
            )
        }
        8 => {
            let func: Fn8 = unsafe { std::mem::transmute(closure.fn_ptr) };
            func(
                closure.args[0],
                closure.args[1],
                closure.args[2],
                closure.args[3],
                closure.args[4],
                closure.args[5],
                closure.args[6],
                closure.args[7],
            )
        }
        _ => {
            let func: Fn8 = unsafe { std::mem::transmute(closure.fn_ptr) };
            let rest = closure.args.split_off(7);
            func(
                closure.args[0],
                closure.args[1],
                closure.args[2],
                closure.args[3],
                closure.args[4],
                closure.args[5],
                closure.args[6],
                (&rest as *const Tuple) as isize,
            )
        }
    };
    debug!("Result: {} / {:#x}", res, res);
    Some(res)
}

macro_rules! apply_closure_n {
    ($fn_name:ident, $($arg:ident),*) => {
        #[no_mangle]
        pub unsafe extern "C" fn $fn_name(closure_ptr: *mut Closure, $($arg: isize),*) -> isize {
            let mut closure = Box::new((*closure_ptr).clone());
            closure.args.extend_from_slice(&[$($arg),*]);
            match apply_closure(&mut closure) {
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
apply_closure_n!(apply_closure_8, a1, a2, a3, a4, a5, a6, a7, a8);

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
    debug!("tuple_ptr: {:?}", tuple_ptr);
    let tuple = Box::from_raw(tuple_ptr);
    debug!("Tuple: {:?}", tuple);
    closure.args.extend_from_slice(&[a1, a2, a3, a4, a5, a6]);
    closure.args.extend(tuple.iter());
    match apply_closure(&mut closure) {
        Some(res) => res,
        None => Box::into_raw(closure) as isize,
    }
}
