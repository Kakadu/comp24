use std::cmp::Ordering;

use log::debug;

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
    debug!("Created {:?} at {:?}", closure, Box::as_ptr(&closure));
    Box::into_raw(closure)
}

fn apply_closure(closure: &Closure) -> Option<isize> {
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
        x => {
            unimplemented!("Closure with size {x}")
        }
    };
    debug!("Result: {} / {:#x}", res, res);
    Some(res)
}

#[no_mangle]
pub unsafe extern "C" fn apply_closure_1(closure_ptr: *mut Closure, arg: isize) -> isize {
    // TODO: less cloning
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg);
    match apply_closure(&closure) {
        Some(res) => res,
        None => Box::into_raw(closure) as isize,
    }
}

#[no_mangle]
pub unsafe extern "C" fn apply_closure_2(closure_ptr: *mut Closure, arg1: isize, arg2: isize) -> isize {
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg1);
    closure.args.push(arg2);
    match apply_closure(&closure) {
        Some(res) => res,
        None => Box::into_raw(closure) as isize,
    }
}

#[no_mangle]
pub unsafe extern "C" fn apply_closure_3(closure_ptr: *mut Closure, arg1: isize, arg2: isize, arg3: isize) -> isize {
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg1);
    closure.args.push(arg2);
    closure.args.push(arg3);
    match apply_closure(&closure) {
        Some(res) => res,
        None => Box::into_raw(closure) as isize,
    }
}

#[no_mangle]
pub unsafe extern "C" fn apply_closure_4(
    closure_ptr: *mut Closure,
    arg1: isize,
    arg2: isize,
    arg3: isize,
    arg4: isize,
) -> isize {
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg1);
    closure.args.push(arg2);
    closure.args.push(arg3);
    closure.args.push(arg4);
    match apply_closure(&closure) {
        Some(res) => res,
        None => Box::into_raw(closure) as isize,
    }
}

#[no_mangle]
pub unsafe extern "C" fn apply_closure_5(
    closure_ptr: *mut Closure,
    arg1: isize,
    arg2: isize,
    arg3: isize,
    arg4: isize,
    arg5: isize,
) -> isize {
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg1);
    closure.args.push(arg2);
    closure.args.push(arg3);
    closure.args.push(arg4);
    closure.args.push(arg5);
    match apply_closure(&closure) {
        Some(res) => res,
        None => Box::into_raw(closure) as isize,
    }
}
