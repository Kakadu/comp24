use log::debug;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Closure {
    pub(crate) fn_ptr: *const fn(),
    pub(crate) arity: usize,
    pub(crate) args: Vec<isize>,
}

// TODO: 
// - impl 
// - types (i/u size) + in lib
// - shitcode with apply_closure
// pub impl Closure {
// }

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

fn apply_closure(closure: &Closure) -> isize {
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
            let func: fn(isize, isize, isize, isize, isize) -> isize = unsafe { std::mem::transmute(closure.fn_ptr) };
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
    // let _ = Box::into_raw(closure); // TODO:
    res
}

#[no_mangle]
pub unsafe extern "C" fn apply_closure_1(closure_ptr: *mut Closure, arg: isize) -> isize {
    // TODO: less cloning
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg);
    debug!("Args: {:?} / {:x?}", closure.args, closure.args);
    if closure.args.len() == closure.arity {
        apply_closure(&closure)
    } else {
        debug!("Not enough args");
        Box::into_raw(closure) as isize
    }
}

#[no_mangle]
pub unsafe extern "C" fn apply_closure_2(closure_ptr: *mut Closure, arg1: isize, arg2: isize) -> isize {
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg1);
    closure.args.push(arg2);
    debug!("Args: {:?} / {:x?}", closure.args, closure.args);
    if closure.args.len() == closure.arity {
        apply_closure(&closure)
    } else {
        debug!("Not enough args");
        Box::into_raw(closure) as isize
    }
}

#[no_mangle]
pub unsafe extern "C" fn apply_closure_3(closure_ptr: *mut Closure, arg1: isize, arg2: isize, arg3:isize) -> isize {
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg1);
    closure.args.push(arg2);
    closure.args.push(arg3);
    debug!("Args: {:?} / {:x?}", closure.args, closure.args);
    if closure.args.len() == closure.arity {
        apply_closure(&closure)
    } else {
        debug!("Not enough args");
        Box::into_raw(closure) as isize
    }
}
#[no_mangle]
pub unsafe extern "C" fn apply_closure_4(closure_ptr: *mut Closure, arg1: isize, arg2: isize, arg3:isize, arg4:isize) -> isize {
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg1);
    closure.args.push(arg2);
    closure.args.push(arg3);
    closure.args.push(arg4);
    debug!("Args: {:?} / {:x?}", closure.args, closure.args);
    if closure.args.len() == closure.arity {
        apply_closure(&closure)
    } else {
        debug!("Not enough args");
        Box::into_raw(closure) as isize
    }
}
#[no_mangle]
pub unsafe extern "C" fn apply_closure_5(closure_ptr: *mut Closure, arg1: isize, arg2: isize, arg3:isize, arg4:isize, arg5:isize) -> isize {
    let mut closure = Box::new((*closure_ptr).clone());
    closure.args.push(arg1);
    closure.args.push(arg2);
    closure.args.push(arg3);
    closure.args.push(arg4);
    closure.args.push(arg5);
    debug!("Args: {:?} / {:x?}", closure.args, closure.args);
    if closure.args.len() == closure.arity {
        apply_closure(&closure)
    } else {
        debug!("Not enough args");
        Box::into_raw(closure) as isize
    }
}