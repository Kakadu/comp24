use log::debug;

use crate::{with_raw, with_raw_mut};

type Tuple = Vec<isize>;

#[no_mangle]
pub extern "C" fn ml_create_tuple(size: usize) -> *mut Tuple {
    let tuple = Box::new(vec![0isize; size]);
    debug!("Created tuple of size {} at {:?}", size, Box::as_ptr(&tuple));
    Box::into_raw(tuple)
}

#[no_mangle]
pub unsafe extern "C" fn ml_set_tuple_field(tuple_ptr: *mut Tuple, idx: usize, value: isize) -> *mut Tuple {
    with_raw_mut(tuple_ptr, |tuple| {
        tuple[idx] = value;
        debug!("Set [{}] = {} in {:?} at {:?}", idx, value, tuple, tuple_ptr);
    });
    tuple_ptr
}

#[no_mangle]
pub unsafe extern "C" fn ml_get_tuple_field(tuple_ptr: *mut Tuple, idx: usize) -> isize {
    with_raw(tuple_ptr, |tuple| {
        debug!("Getting {} of {:?} at {:?}", idx, tuple, tuple_ptr);
        tuple[idx]
    })
}

#[no_mangle]
pub unsafe extern "C" fn ml_print_tuple(tuple_ptr: *mut Tuple) -> isize {
    debug!("Printing tuple at {:?}", tuple_ptr);
    with_raw(tuple_ptr, |tuple| {
        println!(
            "({})",
            tuple.iter().map(isize::to_string).collect::<Vec<_>>().join(", ")
        );
        0
    })
}
