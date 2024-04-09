#![no_std]
use core::ffi::{c_char, CStr};
use libc_print::std_name::print;

/// This is a horrible hack.
/// Currently, `rustc` is buggy, and includes an external reference to the `eh_personality`
/// lang item, even when the crate exclusively uses panic=abort. This is the only solid workaround
/// that I could find so far, however it's quite fragile.
/// See: https://github.com/rust-lang/rust/issues/106864#issuecomment-1858861750
#[no_mangle]
extern "C" fn rust_eh_personality() {}

/// Ditto to above.
/// See: https://github.com/rust-lang/rust/issues/106864#issuecomment-1858861750
#[allow(non_snake_case)]
#[no_mangle]
extern "C" fn _Unwind_Resume() {}

/// Panic handler for the intrinsics crate.
#[cfg(not(test))]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    libc_print::std_name::eprintln!("Abort! :: {info}");
    unsafe { libc::abort() }
}

/// Prints a single string to `stdout` without appending a newline.
#[no_mangle]
pub unsafe extern "C" fn cb_print_str(c_buf: *const c_char) {
    let c_str: &CStr = unsafe { CStr::from_ptr(c_buf) };
    let str_slice: &str = c_str.to_str().unwrap();
    print!("{str_slice}");
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

/// Compares two given strings, returning the following as an [`i64`]:
/// - If the two strings match, 1.
/// - If the two strings do not match, 0.
#[no_mangle]
pub unsafe extern "C" fn cb_strcmp(str_a: *const c_char, str_b: *const c_char) -> i64 {
    // Convert both strings into slices.
    let c_str: &CStr = unsafe { CStr::from_ptr(str_a) };
    let slice_a: &str = c_str.to_str().unwrap();
    let c_str: &CStr = unsafe { CStr::from_ptr(str_b) };
    let slice_b: &str = c_str.to_str().unwrap();

    // Perform a bytewise comparison.
    if slice_a.eq(slice_b) {
        1
    } else {
        0
    }
}
