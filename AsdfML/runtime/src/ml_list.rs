use std::rc::Rc;

use log::debug;

use crate::{cons_list::ConsList, WithRaw};

pub type MlList = ConsList<isize>;

#[no_mangle]
pub extern "C" fn ml_create_list() -> *const MlList {
    let list = ConsList::new();
    debug!("Created list at {:?}", Rc::as_ptr(&list));
    Rc::into_raw(list)
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_cons(value: isize, list_ptr: *const MlList) -> *const MlList {
    let list = Rc::from_raw(list_ptr);
    debug!("Cons {} to list {} at {:?}", value, list, list_ptr);
    let new_list = ConsList::cons(value, list);
    debug!("New head at {:?}", Rc::as_ptr(&new_list));
    Rc::into_raw(new_list)
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_hd(list_ptr: *const MlList) -> isize {
    Rc::with_raw_ref(list_ptr, |list| {
        debug!("Head of list {} at {:?}", list, list_ptr);
        *list.head().expect("ml_list_hd: empty list")
    })
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_tl(list_ptr: *const MlList) -> *const MlList {
    let tail = Rc::with_raw_ref(list_ptr, |list| {
        let tail = list.tail().expect("ml_list_tl: empty list");
        debug!(
            "Tail of list {} at {:?} is {} at {:?}",
            list,
            list_ptr,
            tail,
            Rc::as_ptr(&tail)
        );
        tail
    });
    Rc::into_raw(tail)
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_field(list_ptr: *const MlList, idx: usize) -> isize {
    Rc::with_raw_ref(list_ptr, |list| {
        *list.iter().nth(idx).expect("ml_list_field: index out of bounds")
    })
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_is_empty(list_ptr: *const MlList) -> isize {
    Rc::with_raw_ref(list_ptr, |list| {
        debug!("Is list {} at {:?} empty? {}", list, list_ptr, list.is_empty());
        list.is_empty() as isize
    })
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_len(list_ptr: *const MlList) -> isize {
    Rc::with_raw_ref(list_ptr, |list| list.len() as isize)
}

#[no_mangle]
pub unsafe extern "C" fn ml_print_list(list_ptr: *const MlList) {
    Rc::with_raw_ref(list_ptr, |list| {
        println!("{}", list);
    });
}
