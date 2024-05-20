use super::common::CommonTestRunner;

/// Tests that the "MOD" intrinsic functions correctly on an exact
/// divisor.
#[test]
fn mod_exact() {
    CommonTestRunner::new("mod_exact")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. MOD-EXACT.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 OUT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION MOD(4, 2) TO OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("0\n")
        .run();
}

/// Tests that the "MOD" intrinsic functions correctly on an exact
/// divisor.
#[test]
fn mod_non_divisor() {
    CommonTestRunner::new("mod_non_divisor")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. MOD-NON-DIV.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 OUT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION MOD(5, 3) TO OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("2\n")
        .run();
}

/// Tests that the "LENGTH" intrinsic functions correctly on a zero
/// length string.
#[test]
fn str_length_zero() {
    CommonTestRunner::new("str_length_zero")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-LEN-ZERO.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 OUT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION LENGTH("") TO OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("0\n")
        .run();
}

/// Tests that the "LENGTH" intrinsic functions correctly on a
/// non-zero length string.
#[test]
fn str_length_non_zero() {
    CommonTestRunner::new("str_length_non_zero")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-LEN-NON-ZERO.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 OUT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION LENGTH("SomeLitVal") TO OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("10\n")
        .run();
}

/// Tests that the "RANDOM" intrinsic generates different
/// values on subsequent executions.
#[test]
fn random_differs() {
    CommonTestRunner::new("random_differs")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. RANDOM-DIFFERS.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC S9(4)P9(4) COMP.
    01 B-VAL PIC S9(4)P9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION RANDOM() TO A-VAL.
    MOVE FUNCTION RANDOM() TO B-VAL.
    IF A-VAL = B-VAL THEN
        DISPLAY "fail"
    ELSE
        DISPLAY "pass"
    END-IF.
STOP RUN.
        "#)
        .expect_output("pass\n")
        .run();
}

/// Tests that the "INTEGER" intrinsic correctly rounds up a given
/// floating point value.
#[test]
fn integer_rounds_up() {
    CommonTestRunner::new("integer_rounds_up")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INT-ROUNDS-UP.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 OUT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION INTEGER(2.398282) TO OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("3\n")
        .run();
}

/// Tests that the "INTEGER" intrinsic correctly converts an exact
/// floating point value to the equivalent integer.
#[test]
fn integer_translates_exact() {
    CommonTestRunner::new("integer_translates_exact")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INT-TRANS-EXACT.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 OUT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION INTEGER(214.0) TO OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("214\n")
        .run();
}