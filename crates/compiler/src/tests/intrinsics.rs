use super::common::CommonTestRunner;

/// Tests that the MOD intrinsic for an exact divisor functions.
#[test]
fn mod_exact() {
    CommonTestRunner::new("mod_exact")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. MOD-EXACT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 INT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION MOD(20, 2) TO INT-VAL.
    DISPLAY INT-VAL.
STOP RUN.
        "#)
        .expect_output("0\n")
        .run();
}

/// Tests that the MOD intrinsic for an non-exact divisor functions.
#[test]
fn mod_non_exact() {
    CommonTestRunner::new("mod_non_exact")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. MOD-NON-EXACT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 INT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION MOD(16, 3) TO INT-VAL.
    DISPLAY INT-VAL.
STOP RUN.
        "#)
        .expect_output("1\n")
        .run();
}

/// Tests that the LENGTH intrinsic works for a zero-length string.
#[test]
fn length_zero() {
    CommonTestRunner::new("length_zero")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. LENGTH-ZERO-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10) VALUE "".
    01 INT-VAL PIC 9(10) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION LENGTH(STR-VAL) TO INT-VAL.
    DISPLAY INT-VAL.
STOP RUN.
        "#)
        .expect_output("0\n")
        .run();
}

/// Tests that the LENGTH intrinsic works for a non-zero length string.
#[test]
fn length_non_zero() {
    CommonTestRunner::new("length_non_zero")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. LENGTH-NON-ZERO-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10) VALUE "SomeStr".
    01 INT-VAL PIC 9(10) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION LENGTH(STR-VAL) TO INT-VAL.
    DISPLAY INT-VAL.
STOP RUN.
        "#)
        .expect_output("7\n")
        .run();
}


/// Tests that the RANDOM intrinsic differs on subsequent calls.
#[test]
fn random_differs() {
    CommonTestRunner::new("random_differs")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. RANDOM-DIFFERS-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(4)P9(4) COMP.
    01 B-VAL PIC 9(4)P9(4) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION RANDOM() TO A-VAL.
    MOVE FUNCTION RANDOM() TO B-VAL.
    IF NOT A-VAL = B-VAL THEN
        DISPLAY "pass"
    ELSE
        DISPLAY "fail"
    END-IF.
STOP RUN.
        "#)
        .expect_output("pass\n")
        .run();
}

/// Tests that the INTEGER intrinsic works for a non-integer equivalent value.
#[test]
fn integer_converts_float() {
    CommonTestRunner::new("integer_converts_float")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INT-CONV-FLOAT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 FLT-VAL PIC 9(4)P9(4) COMP VALUE 20.43.
    01 INT-VAL PIC 9(10) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION INTEGER(FLT-VAL) TO INT-VAL.
    DISPLAY INT-VAL.
STOP RUN.
        "#)
        .expect_output("21\n")
        .run();
}

/// Tests that the INTEGER intrinsic works for an integer equivalent value.
#[test]
fn integer_converts_int() {
    CommonTestRunner::new("integer_converts_int")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INT-CONV-INT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 FLT-VAL PIC 9(4)P9(4) COMP VALUE 220.0.
    01 INT-VAL PIC 9(10) COMP.

PROCEDURE DIVISION.
    MOVE FUNCTION INTEGER(FLT-VAL) TO INT-VAL.
    DISPLAY INT-VAL.
STOP RUN.
        "#)
        .expect_output("220\n")
        .run();
}