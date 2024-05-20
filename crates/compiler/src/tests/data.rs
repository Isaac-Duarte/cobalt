use super::common::CommonTestRunner;

/// Tests that a string variable can hold an initial value.
#[test]
fn str_initial_value() {
    CommonTestRunner::new("str_initial_val")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-INITIAL-VALUE-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10) VALUE "InitialVal".

PROCEDURE DIVISION.
    DISPLAY STR-VAL.
STOP RUN.
        "#)
        .expect_output("InitialVal\n")
        .run();
}

/// Tests that a string variable cannot hold an invalid initial value.
#[test]
fn invalid_str_initial_value() {
    CommonTestRunner::new("invalid_str_initial_val")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INV-STR-INITIAL-VALUE-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10) VALUE "InitialValTooLongOhNo!".

PROCEDURE DIVISION.
    DISPLAY STR-VAL.
STOP RUN.
        "#)
        .expect_fail(Some("does not fit data layout"))
        .run();
}

/// Tests that an integer variable can hold an initial value.
#[test]
fn int_initial_value() {
    CommonTestRunner::new("int_initial_val")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INT-INITIAL-VALUE-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 TEST-VAL PIC 9(4) COMP VALUE 9004.

PROCEDURE DIVISION.
    DISPLAY TEST-VAL.
STOP RUN.
        "#)
        .expect_output("9004\n")
        .run();
}

/// Tests that an integer variable cannot hold an initial float value.
#[test]
fn invalid_int_initial_value() {
    CommonTestRunner::new("invalid_int_initial_val")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INV-INT-INITIAL-VALUE-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 TEST-VAL PIC 9(4) COMP VALUE 40.2.

PROCEDURE DIVISION.
    DISPLAY TEST-VAL.
STOP RUN.
        "#)
        .expect_fail(Some("does not fit data layout"))
        .run();
}

/// Tests that a floating point variable can hold an initial value.
#[test]
fn float_initial_value() {
    CommonTestRunner::new("float_initial_val")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. FLT-INITIAL-VALUE-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 TEST-VAL PIC S9(4)P9(4) COMP VALUE -9004.2.

PROCEDURE DIVISION.
    DISPLAY TEST-VAL.
STOP RUN.
        "#)
        .expect_output("-9004.2\n")
        .run();
}