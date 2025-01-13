use log::debug;

type Tuple = Vec<isize>;

#[no_mangle]
pub extern "C" fn ml_create_tuple(size: usize) -> *mut Tuple {
    let tuple = Box::new(vec![0isize; size]);
    debug!("Created tuple of size {} at {:?}", size, Box::as_ptr(&tuple));
    Box::into_raw(tuple)
}

#[no_mangle]
pub extern "C" fn ml_set_tuple_field(tuple_ptr: *mut Tuple, idx: usize, value: isize) -> *mut Tuple {
    let mut tuple = unsafe { Box::from_raw(tuple_ptr) };
    tuple[idx] = value;
    debug!("Set [{}] = {} in {:?} at {:?}", idx, value, tuple, tuple_ptr);
    Box::into_raw(tuple)
}

#[no_mangle]
pub extern "C" fn ml_get_tuple_field(tuple_ptr: *mut Tuple, idx: usize) -> isize {
    let tuple = unsafe { Box::from_raw(tuple_ptr) };
    debug!("Getting {} of {:?} at {:?}", idx, tuple, tuple_ptr);
    let res = tuple[idx];
    let _ = Box::into_raw(tuple);
    res
}

#[no_mangle]
pub extern "C" fn ml_print_tuple(tuple_ptr: *mut Tuple) -> isize {
    debug!("Printing tuple at {:?}", tuple_ptr);
    let tuple = unsafe { Box::from_raw(tuple_ptr) };
    println!(
        "({})",
        tuple.iter().map(isize::to_string).collect::<Vec<_>>().join(", ")
    );
    let _ = Box::into_raw(tuple);
    0
}
