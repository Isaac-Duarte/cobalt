#![no_std]
extern crate alloc;
use alloc::string::String;
use core::{
    ffi::{c_char, CStr},
    ptr::null_mut,
};
use libc_alloc::LibcAlloc;
use libc_print::std_name::print;
use once_cell::unsync::Lazy;
use rand::{rngs::SmallRng, RngCore, SeedableRng};

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

/// Copies a portion of the given source string into the destination string.
/// Panics if the source or destination span are invalid.
/// If the source span does not fit within the destination span, the source is truncated upon copy.
#[no_mangle]
pub unsafe extern "C" fn cb_strcpy(
    src_str: *const c_char,
    dest_str: *mut c_char,
    src_len: i64,
    dest_len: i64,
    src_span_idx: i64,
    src_span_len: i64,
    dest_span_idx: i64,
    dest_span_len: i64,
) {
    // Length sanity check.
    if src_span_idx < 0 || dest_span_idx < 0 || src_span_len < 1 || dest_span_len < 1 {
        panic!(
            "Source ({}:{}) or destination ({}:{}) spans contain out of bounds accesses or are zero-copy.",
            src_span_idx, src_span_len, dest_span_idx, dest_span_len
        );
    }

    // Verify the source span is valid within the source, destination span is valid within destination.
    if src_span_idx + src_span_len > src_len {
        panic!(
            "Source span ({}:{}) out of bounds for given source string ({} bytes).",
            src_span_idx, src_span_len, src_len
        );
    }
    if dest_span_idx + dest_span_len > dest_len {
        panic!(
            "Destination span ({}:{}) out of bounds for given destination string ({} bytes).",
            dest_span_idx, dest_span_len, dest_len
        );
    }

    // The destination string may currently have garbage post-terminator data before our index.
    // If that's the case, we need to overwrite it with spaces.
    let dest_cstr: &CStr = unsafe { CStr::from_ptr(dest_str) };
    let dest_slice: &str = dest_cstr.to_str().unwrap();
    let orig_dest_len = dest_slice.len();
    if dest_slice.len() < dest_span_idx as usize {
        libc::memset(
            dest_str.add(dest_slice.len()).cast(),
            (' ' as u8) as i32,
            dest_span_idx as usize - dest_slice.len(),
        );
    }

    // Calculate the pointer offset for source, destination & size.
    let src_ptr = src_str.add(src_span_idx as usize);
    let dest_ptr = dest_str.add(dest_span_idx as usize);
    let size = core::cmp::min(src_span_len, dest_span_len) as usize;

    // Perform copy (possibly overlapping).
    core::ptr::copy(src_ptr, dest_ptr, size);

    // Add a null terminator, if the original string ended before where our copy ended.
    if orig_dest_len < (dest_span_idx as usize) + size {
        *dest_ptr.add(size) = b'\0' as _;
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

/// COBOL length intrinsic.
/// Returns the length of the given string.
#[no_mangle]
pub unsafe extern "C" fn cb_length(str: *const c_char) -> i64 {
    let c_str: &CStr = unsafe { CStr::from_ptr(str) };
    let slice: &str = c_str.to_str().unwrap();
    slice.len() as i64
}

/// COBOL random intrinsic.
/// Generates a random floating point value between 0 and 1, and returns it.
/// Adapted from the C implementation in the following article:
/// https://www.corsix.org/content/higher-quality-random-floats
#[no_mangle]
pub unsafe extern "C" fn cb_random() -> f64 {
    static mut RNG: Lazy<SmallRng> =
        Lazy::new(|| SmallRng::seed_from_u64(unsafe { libc::time(null_mut()) } as u64));
    let x = RNG.next_u64() >> 11;
    let mut e: u64 = 1022;
    loop {
        if RNG.next_u64() & 1 == 1 {
            break;
        }
        e -= 1;
        if e <= 1022 - 75 {
            break;
        }
    }
    let out = ((x + 1) >> 1) + (e << 52);
    f64::from_ne_bytes(out.to_ne_bytes())
}

/// COBOL integer intrinsic.
/// Converts the given float to the next highest integer.
#[no_mangle]
pub unsafe extern "C" fn cb_integer(f: f64) -> i64 {
    libm::ceil(f) as i64
}
