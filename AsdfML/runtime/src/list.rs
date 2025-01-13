use std::collections::VecDeque;

use log::debug;

type List = VecDeque<isize>;

#[no_mangle]
pub extern "C" fn ml_create_list() -> *mut List {
    let list = Box::new(List::new());
    debug!("Created list at {:?}", Box::as_ptr(&list));
    Box::into_raw(list)
}

#[no_mangle]
pub extern "C" fn ml_list_cons(list_ptr: *mut List, value: isize) -> *mut List {
    let mut list = unsafe { Box::from_raw(list_ptr) };
    debug!("Cons {} to list {:?} at {:?}", value, list, list_ptr);
    list.push_front(value);
    Box::into_raw(list)
}

#[no_mangle]
pub extern "C" fn ml_list_hd(list_ptr: *mut List) -> isize {
    let list = unsafe { Box::from_raw(list_ptr) };
    let res = match list.front() {
        Some(&value) => value,
        None => panic!("ml_list_hd: empty list"),
    };
    let _ = Box::into_raw(list);
    res
}

#[no_mangle]
pub extern "C" fn ml_list_tl(list_ptr: *mut List) -> *mut List { unimplemented!() }

#[no_mangle]
pub extern "C" fn ml_list_field(list_ptr: *mut List, idx: usize) -> isize {
    let list = unsafe { Box::from_raw(list_ptr) };
    let res = list
        .iter()
        .enumerate()
        .find(|(i, _)| *i == idx)
        .map(|(_, &value)| value)
        .expect("ml_list_field: index out of bounds");
    let _ = Box::into_raw(list);
    res
}

#[no_mangle]
pub extern "C" fn ml_print_list(list_ptr: *mut List) {
    let list = unsafe { Box::from_raw(list_ptr) };
    println!("{:?}", list);
    let _ = Box::into_raw(list);
}
