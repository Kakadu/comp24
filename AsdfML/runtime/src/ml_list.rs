use log::debug;

use crate::{list::List, with_raw};

pub type MlList = List<isize>;

#[no_mangle]
pub extern "C" fn ml_create_list() -> *mut MlList {
    let list = Box::new(MlList::new());
    debug!("Created list at {:?}", Box::as_ptr(&list));
    Box::into_raw(list)
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_cons(list_ptr: *mut MlList, value: isize) -> *mut MlList {
    let new_list = with_raw(list_ptr, |list| {
        debug!("Cons {} to list {:?} at {:?}", value, list, list_ptr);
        Box::new(list.prepend(value))
    });
    debug!("New list at {:?}", Box::as_ptr(&new_list));
    Box::into_raw(new_list)
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_hd(list_ptr: *mut MlList) -> isize {
    with_raw(list_ptr, |list| *list.head().expect("ml_list_hd: empty list"))
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_tl(list_ptr: *mut MlList) -> *mut MlList {
    let tail = with_raw(list_ptr, |list| Box::new(list.tail()));
    Box::into_raw(tail)
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_field(list_ptr: *mut MlList, idx: usize) -> isize {
    with_raw(list_ptr, |list| {
        list.iter()
            .enumerate()
            .find(|(i, _)| *i == idx)
            .map(|(_, &value)| value)
            .expect("ml_list_field: index out of bounds")
    })
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_is_empty(list_ptr: *mut MlList) -> isize {
    with_raw(list_ptr, |list| (list.len() == 0) as isize)
}

#[no_mangle]
pub unsafe extern "C" fn ml_print_list(list_ptr: *mut MlList) {
    with_raw(list_ptr, |list| {
        println!("{}", list);
    });
}
