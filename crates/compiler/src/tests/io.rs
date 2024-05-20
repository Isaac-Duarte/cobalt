use super::common::CommonTestRunner;

/// Tests that a basic display instruction outputs as expected.
#[test]
fn basic_display_test() {
    CommonTestRunner::new("display_string")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

PROCEDURE DIVISION.
    DISPLAY "Hello world!".
STOP RUN.
        "#)
        .expect_output("Hello world!\n")
        .run();
}

/// Tests that a basic display instruction of an integer variable works as expected.
#[test]
fn int_display_test() {
    CommonTestRunner::new("display_int")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. DISPLAY-DATA-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 NUM-VAL PIC 9(4) COMP VALUE 1234.

PROCEDURE DIVISION.
    DISPLAY NUM-VAL.
STOP RUN.
        "#)
        .expect_output("1234\n")
        .run();
}

/// Tests that a basic display instruction of a floating point variable works as expected.
#[test]
fn float_display_test() {
    CommonTestRunner::new("display_float")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. DISPLAY-DATA-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 FLT-VAL PIC S9(4)P9(4) COMP VALUE -1234.5678.

PROCEDURE DIVISION.
    DISPLAY FLT-VAL.
STOP RUN.
        "#)
        .expect_output("-1234.5678\n")
        .run();
}


/// Tests that a basic display instruction with interleaved variables outputs as expected.
#[test]
fn interleaved_display_test() {
    CommonTestRunner::new("display_interleaved")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. DISPLAY-DATA-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 NUM-VAL PIC 9(4) COMP VALUE 1234.

PROCEDURE DIVISION.
    DISPLAY "My favourite number is " NUM-VAL ".".
STOP RUN.
        "#)
        .expect_output("My favourite number is 1234.\n")
        .run();
}

/// Tests that accepting a string via. stdin works as expected.
#[test]
fn accept_str() {
    CommonTestRunner::new("accept_str")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. ACCEPT-STR-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10).

PROCEDURE DIVISION.
    ACCEPT STR-VAL.
    DISPLAY STR-VAL.
STOP RUN.
        "#)
        .expect_output_with_input("InitialVal\n", "InitialVal\n")
        .run();
}

/// Tests that accepting an integer via. stdin works as expected.
#[test]
fn accept_int() {
    CommonTestRunner::new("accept_int")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. ACCEPT-INT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 INT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    ACCEPT INT-VAL.
    DISPLAY INT-VAL.
STOP RUN.
        "#)
        .expect_output_with_input("2024\n", "2024\n")
        .run();
}

/// Tests that accepting a floating point value via. stdin works as expected.
#[test]
fn accept_float() {
    CommonTestRunner::new("accept_float")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. ACCEPT-FLT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 FLT-VAL PIC S9(4)P9(5) COMP.

PROCEDURE DIVISION.
    ACCEPT FLT-VAL.
    DISPLAY FLT-VAL.
STOP RUN.
        "#)
        .expect_output_with_input("-1234.5678\n", "-1234.5678\n")
        .run();
}