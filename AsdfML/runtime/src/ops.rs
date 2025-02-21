macro_rules! make_int_bin_op {
    ($name:ident, $op:tt) => {
        #[no_mangle]
        pub extern "C" fn $name(lhs: isize, rhs: isize) -> isize {
            (lhs $op rhs) as isize
        }
    };
}

macro_rules! make_bool_bin_op {
    ($name:ident, $op:tt) => {
        #[no_mangle]
        pub extern "C" fn $name(lhs: isize, rhs: isize) -> isize {
            ((lhs != 0) $op (rhs != 0)) as isize
        }
    };
}

make_int_bin_op!(ml_add, +);
make_int_bin_op!(ml_sub, -);
make_int_bin_op!(ml_mul, *);
#[no_mangle]
pub extern "C" fn ml_div(lhs: isize, rhs: isize) -> isize { 
    lhs.checked_div(rhs).expect("Division by zero") }

make_int_bin_op!(ml_gt, >);
make_int_bin_op!(ml_lt, <);
make_int_bin_op!(ml_ge, >=);
make_int_bin_op!(ml_le, <=);
make_int_bin_op!(ml_eq, ==);
make_int_bin_op!(ml_ne, !=);

make_bool_bin_op!(ml_and, &&);
make_bool_bin_op!(ml_or, ||);

#[no_mangle]
pub extern "C" fn ml_println_int(i: isize) { println!("{}", i) }

#[no_mangle]
pub extern "C" fn ml_print_int(i: isize) { print!("{} ", i) }

#[no_mangle]
pub extern "C" fn ml_println_bool(b: isize) { println!("{}", b != 0) }

#[no_mangle]
pub extern "C" fn ml_print_newline() { println!() }

#[no_mangle]
pub extern "C" fn ml_print_char(i: isize) { print!("{}", i as u8 as char) }

#[no_mangle]
pub extern "C" fn ml_panic() { panic!("Panic from AsdfML") }

#[no_mangle]
pub extern "C" fn ml_neg(x: isize) -> isize { -x }

#[no_mangle]
pub extern "C" fn ml_not(x: isize) -> isize { !x }
