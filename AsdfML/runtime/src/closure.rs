use std::clone;

use log::debug;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Closure {
    pub(crate) fn_ptr: *const fn(),
    pub(crate) arity: usize,
    pub(crate) args: Vec<isize>,
}

// pub impl Closure {
//     // TODO: this and types (i/u size) + in lib
// }

#[no_mangle]
pub extern "C" fn create_closure(fn_ptr: *const fn(), arity: usize) -> *mut Closure {
    let closure = Box::new(Closure {
        fn_ptr,
        arity,
        args: Vec::with_capacity(arity),
    });
    debug!("Created {:?} at {:?}", closure, Box::as_ptr(&closure));
    let res = Box::into_raw(closure);
    res
}

#[no_mangle]
pub extern "C" fn apply_closure(closure_ptr: *mut Closure, arg: isize) -> isize {
    // TODO: less cloning
    let mut closure = Box::new(unsafe { &*closure_ptr }.clone());
    closure.args.push(arg);
    debug!("Args: {:?} / {:x?}", closure.args, closure.args);
    if closure.args.len() == closure.arity {
        let res = match closure.arity {
            1 => {
                let func: fn(isize) -> isize = unsafe { std::mem::transmute(closure.fn_ptr) };
                func(closure.args[0])
            }
            2 => {
                let func: fn(isize, isize) -> isize = unsafe { std::mem::transmute(closure.fn_ptr) };
                func(closure.args[0], closure.args[1])
            }
            3 => {
                let func: fn(isize, isize, isize) -> isize = unsafe { std::mem::transmute(closure.fn_ptr) };
                func(closure.args[0], closure.args[1], closure.args[2])
            }
            4 => {
                let func: fn(isize, isize, isize, isize) -> isize = unsafe { std::mem::transmute(closure.fn_ptr) };
                func(closure.args[0], closure.args[1], closure.args[2], closure.args[3])
            }
            5 => {
                let func: fn(isize, isize, isize, isize, isize) -> isize =
                    unsafe { std::mem::transmute(closure.fn_ptr) };
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
        res
    } else {
        debug!("Not enough args");
        Box::into_raw(closure) as isize
    }
}
