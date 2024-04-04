#![no_std]
use core::ffi::{c_char, CStr};
use libc_print::std_name::print;

/// Panic handler for the intrinsics crate.
#[cfg(not(test))]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    libc_print::std_name::eprintln!("Abort! :: {info}");
    loop {}
}

/// Prints a single string to `stdout` without appending a newline.
#[no_mangle]
pub unsafe extern "C" fn cb_print_str(c_buf: *const c_char) {
    let c_str: &CStr = unsafe { CStr::from_ptr(c_buf) };
    let str_slice: &str = c_str.to_str().unwrap();
    print!("intrinsic test: '{str_slice}'");
}

/// Prints a single [`i64`] to `stdout` without appending a newline.
#[no_mangle]
pub unsafe extern "C" fn cb_print_i64(i: i64) {
    print!("{i}");
}

/// Prints a single [`f64`] to `stdout` without appending a newline.
#[no_mangle]
pub unsafe extern "C" fn cb_print_f64(f: f64) {
    print!("{f}");
}
