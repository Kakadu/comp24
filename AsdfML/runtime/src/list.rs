use std::collections::VecDeque;

type List = VecDeque<isize>;

#[no_mangle]
pub extern "C" fn ml_create_list() -> *mut List {
    let list = Box::new(List::new());
    Box::into_raw(list)
}

#[no_mangle]
pub unsafe extern "C" fn ml_cons(list: *mut List, value: isize) { (*list).push_front(value); }

#[no_mangle]
pub unsafe extern "C" fn ml_list_hd(list: *mut List) -> isize {
    match (*list).front() {
        Some(&value) => value,
        None => panic!("ml_list_hd: empty list"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn ml_list_tl(list: *mut List) -> *mut List { unimplemented!() }

#[no_mangle]
pub unsafe extern "C" fn ml_list_field(list: *mut List, idx: usize) -> isize {
    (*list)
        .iter()
        .enumerate()
        .find(|(i, _)| *i == idx)
        .map(|(_, &value)| value)
        .expect("ml_list_field: index out of bounds")
}
