use log::debug;

#[repr(C)]
#[derive(Debug)]
pub struct Closure {
    pub(crate) fn_ptr: usize,
    pub(crate) arity: usize,
    pub(crate) args: Vec<isize>,
}

// pub impl Closure {
//     // TODO: this and types (i/u size) + in lib
// }

#[no_mangle]
pub extern "C" fn create_closure(fn_ptr: usize, arity: usize) -> *mut Closure {
    let closure = Box::new(Closure {
        fn_ptr,
        arity,
        args: Vec::with_capacity(arity),
    });
    Box::into_raw(closure)
}

#[no_mangle]
pub extern "C" fn apply_closure(closure_ptr: *mut Closure, arg: isize) -> isize {
    let closure = unsafe { &mut *closure_ptr };
    closure.args.push(arg);
    debug!("Args: {:?}", closure.args);
    if closure.args.len() == closure.arity {
        debug!("Enough args");
        match closure.arity {
            1 => {
                let func: fn(isize) -> isize = unsafe { std::mem::transmute(closure.fn_ptr) };
                let res = func(closure.args[0]);
                debug!("Result: {}", res);
                res
            }
            2 => {
                let func: fn(isize, isize) -> isize = unsafe { std::mem::transmute(closure.fn_ptr) };
                let res = func(closure.args[0], closure.args[1]);
                debug!("Result: {}", res);
                res
            }
            _ => {
                unimplemented!()
            }
        }
    } else {
        debug!("Not enough args");
        closure_ptr as isize
    }
}
