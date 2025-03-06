#[no_mangle]
pub extern "C" fn ml_add(lhs: isize, rhs: isize) -> isize {
    lhs + rhs
}
