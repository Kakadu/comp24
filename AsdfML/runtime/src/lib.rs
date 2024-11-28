#[no_mangle]
pub extern "C" fn add(left: u64, right: u64) -> u64 { left + right }

#[no_mangle]
pub unsafe extern "C" fn asdf_print_int(int_ptr: usize) { print!("{}", *(int_ptr as *const i64)) }
