use log::debug;

use crate::WithRaw;

type Tuple = Vec<isize>;

#[no_mangle]
pub extern "C" fn ml_create_tuple(size: usize) -> *mut Tuple {
    let tuple = Box::new(vec![0isize; size]);
    debug!("Created tuple of size {} at {:?}", size, Box::as_ptr(&tuple));
    Box::into_raw(tuple)
}

#[no_mangle]
pub unsafe extern "C" fn ml_set_tuple_field(tuple_ptr: *mut Tuple, idx: usize, value: isize) {
    Box::with_raw_mut(tuple_ptr, |tuple| {
        tuple[idx] = value;
        debug!("Set [{}] = {} in {:?} at {:?}", idx, value, tuple, tuple_ptr);
    });
}

#[no_mangle]
pub unsafe extern "C" fn ml_get_tuple_field(tuple_ptr: *const Tuple, idx: usize) -> isize {
    Box::with_raw_ref(tuple_ptr, |tuple| {
        debug!("Getting {} of {:?} at {:?}", idx, tuple, tuple_ptr);
        tuple[idx]
    })
}

#[no_mangle]
pub unsafe extern "C" fn ml_print_tuple(tuple_ptr: *const Tuple) -> isize {
    debug!("Printing tuple at {:?}", tuple_ptr);
    Box::with_raw_ref(tuple_ptr, |tuple| {
        println!(
            "({})",
            tuple.iter().map(isize::to_string).collect::<Vec<_>>().join(", ")
        );
        0
    })
}
