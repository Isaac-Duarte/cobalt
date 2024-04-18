#![no_std]
extern crate alloc;
use alloc::string::String;
use core::ffi::{c_char, CStr};
use libc_alloc::LibcAlloc;
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

/// The allocator to use for this library.
#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

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

/// Compares two given strings, returning the following as an [`i8`]:
/// - If the two strings match, 1.
/// - If the two strings do not match, 0.
#[no_mangle]
pub unsafe extern "C" fn cb_strcmp(str_a: *const c_char, str_b: *const c_char) -> i8 {
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

/// Reads a single line from the console, copying the data into the given buffer.
/// The length of this buffer must be provided.
#[no_mangle]
pub unsafe extern "C" fn cb_readstr(buf: *mut c_char, buf_len: usize) {
    let input = cb_readline();
    if input.len() >= buf_len {
        panic!(
            "Input string was too long for string buffer ({} > {})",
            input.len() + 1,
            buf_len
        );
    }
    let input_bytes = input.as_bytes();
    let input_ptr = input_bytes.as_ptr();

    // Safety: This is only valid because our string lives until the end of the function.
    core::ptr::copy_nonoverlapping(input_ptr, buf.cast(), input_bytes.len());
    // Add our null terminator.
    *buf.add(input.len()) = b'\0' as _;
}

/// Reads a single integer from the console, returning the result.
/// On failure to parse, panics.
#[no_mangle]
pub unsafe extern "C" fn cb_readint() -> i64 {
    let input = cb_readline();
    input
        .parse::<i64>()
        .expect("Invalid integer, could not parse.")
}

/// Reads a single floating point number from the console, returning the result.
/// On failure to parse, panics.
#[no_mangle]
pub unsafe extern "C" fn cb_readfloat() -> f64 {
    let input = cb_readline();
    input
        .parse::<f64>()
        .expect("Invalid float, could not parse.")
}

/// Reads a single owned line from the console, returning it.
unsafe fn cb_readline() -> String {
    let mut input = String::new();
    loop {
        let cur_char = char::from_u32(libc::getchar() as u32).expect("Invalid char code.");
        if cur_char == '\n' {
            break;
        }
        input.push(cur_char);
    }
    input
}

/// COBOL modulus intrinsic.
/// Performs a modulus operation on two integers.
#[no_mangle]
pub unsafe extern "C" fn cb_mod(a: i64, b: i64) -> i64 {
    a % b
}
