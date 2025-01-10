use env_logger;
use log::LevelFilter;
mod closure;
mod list;
mod tuple;
pub use closure::*;
pub use list::*;
pub use tuple::*;

/// Set up the logger with the default log level of `info`.
/// This can be overridden by setting the `RUST_LOG` environment variable.
#[no_mangle]
pub extern "C" fn runtime_init() {
    let _ = env_logger::Builder::new()
        .filter_level(LevelFilter::Info)
        .parse_default_env()
        .try_init();
}

macro_rules! make_int_bin_op {
    ($name:ident, $op:tt) => {
        #[no_mangle]
        pub extern "C" fn $name(lhs: isize, rhs: isize) -> isize {
            lhs $op rhs
        }
    };
}

macro_rules! make_bool_bin_op {
    ($name:ident, $op:tt) => {
        #[no_mangle]
        pub extern "C" fn $name(lhs: isize, rhs: isize) -> isize {
            (lhs $op rhs) as isize
        }
    };
}

make_int_bin_op!(ml_add, +);
make_int_bin_op!(ml_sub, -);
make_int_bin_op!(ml_mul, *);
#[no_mangle]
pub extern "C" fn ml_div(lhs: isize, rhs: isize) -> isize { lhs.checked_div(rhs).expect("Division by zero") }

make_bool_bin_op!(ml_gt, >);
make_bool_bin_op!(ml_lt, <);
make_bool_bin_op!(ml_ge, >=);
make_bool_bin_op!(ml_le, <=);
make_bool_bin_op!(ml_eq, ==);
make_bool_bin_op!(ml_ne, !=);
make_bool_bin_op!(ml_and, &);
make_bool_bin_op!(ml_or, |);

pub extern "C" fn print_int(int_ptr: usize) { unsafe { print!("{}", *(int_ptr as *const i64)) } }

#[no_mangle]
pub extern "C" fn ml_print_bool(b: isize) {
    println!("{}", b != 0);
}

#[no_mangle]
pub extern "C" fn ml_neg(x: isize) -> isize { -x }
pub extern "C" fn ml_not(x: isize) -> isize { !x }
