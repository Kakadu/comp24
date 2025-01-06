macro_rules! make_int_binop {
    ($name:ident, $op:tt) => {
        #[no_mangle]
        pub extern "C" fn $name(lhs: i64, rhs: i64) -> i64 {
            lhs $op rhs
        }
    };
}

macro_rules! make_bool_binop {
    ($name:ident, $op:tt) => {
        #[no_mangle]
        pub extern "C" fn $name(lhs: i64, rhs: i64) -> i64 {
            (lhs $op rhs) as i64
        }
    };
}

make_int_binop!(ml_add, +);
make_int_binop!(ml_sub, -);
make_int_binop!(ml_mul, *);
#[no_mangle]
pub extern "C" fn ml_div(lhs: i64, rhs: i64) -> i64 { lhs.checked_div(rhs).unwrap_or(0) }

make_bool_binop!(ml_gt, >);
make_bool_binop!(ml_lt, <);
make_bool_binop!(ml_ge, >=);
make_bool_binop!(ml_le, <=);
make_bool_binop!(ml_eq, ==);
make_bool_binop!(ml_ne, !=);
make_bool_binop!(ml_and, &);
make_bool_binop!(ml_or, |);

#[no_mangle]
pub extern "C" fn print_int(int_ptr: usize) { unsafe { print!("{}", *(int_ptr as *const i64)) } }

/*
#[no_mangle]
pub extern "C" fn ml_neg(lhs: u64, rhs: u64) -> u64 { lhs + rhs }

#[no_mangle]
pub extern "C" fn ml_not(lhs: u64, rhs: u64) -> u64 { lhs + rhs }

#[no_mangle]
pub extern "C" fn ml_cons(lhs: u64, rhs: u64) -> u64 { lhs + rhs }

#[no_mangle]
pub extern "C" fn ml_tuple_field(lhs: u64, rhs: u64) -> u64 { lhs + rhs }

#[no_mangle]
pub extern "C" fn ml_list_field(lhs: u64, rhs: u64) -> u64 { lhs + rhs }

#[no_mangle]
pub extern "C" fn ml_list_hd(lhs: u64, rhs: u64) -> u64 { lhs + rhs }

#[no_mangle]
pub extern "C" fn ml_list_tl(lhs: u64, rhs: u64) -> u64 { lhs + rhs }
*/
