use super::common::CommonTestRunner;

/// Tests that a simple integer MOVE instruction compiles.
#[test]
fn int_lit_move() {
    CommonTestRunner::new("int_lit_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INT-MOVE-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 INT-VAL PIC 9(4) COMP.

PROCEDURE DIVISION.
    MOVE 20 TO INT-VAL.
STOP RUN.
        "#)
        .expect_pass()
        .run();
}

/// Tests that a simple floating point MOVE instruction compiles.
#[test]
fn float_move() {
    CommonTestRunner::new("float_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. FLT-MOVE-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 FLT-VAL PIC 9(4)P9(4) COMP.

PROCEDURE DIVISION.
    MOVE 20.04 TO FLT-VAL.
STOP RUN.
        "#)
        .expect_pass()
        .run();
}

/// Tests that a simple string MOVE instruction compiles.
#[test]
fn str_move() {
    CommonTestRunner::new("str_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-MOVE-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE "TestVal" TO STR-VAL.
STOP RUN.
        "#)
        .expect_pass()
        .run();
}

/// Tests that an overly large string MOVE instruction does not compile.
#[test]
fn str_invalid_move() {
    CommonTestRunner::new("str_move_invalid")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-MOVE-INV-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE "TestValTooLongOhNo" TO STR-VAL.
STOP RUN.
        "#)
        .expect_fail(Some("incompatible literal"))
        .run();
}

/// Tests that a simple spanned source string MOVE instruction compiles.
#[test]
fn str_move_src_spanned() {
    CommonTestRunner::new("str_move_src_spanned")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-MOVE-SRC-SPANNED-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 SRC-VAL PIC X(10) VALUE "TestVal".
    01 STR-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE SRC-VAL(5:3) TO STR-VAL.
    DISPLAY STR-VAL.
STOP RUN.
        "#)
        .expect_output("Val\n")
        .run();
}

/// Tests that a simple spanned destination string MOVE instruction compiles.
#[test]
fn str_move_dest_spanned() {
    CommonTestRunner::new("str_move_dest_spanned")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-MOVE-DEST-SPANNED-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE "TestVal" TO STR-VAL(1:4).
    DISPLAY STR-VAL.
STOP RUN.
        "#)
        .expect_output("Test\n")
        .run();
}

/// Tests that a simple spanned source & destination string MOVE instruction compiles.
#[test]
fn str_move_all_spanned() {
    CommonTestRunner::new("str_move_all_spanned")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-MOVE-ALL-SPANNED-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 SRC-VAL PIC X(10) VALUE "TestVal".
    01 STR-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE SRC-VAL(2:3) TO STR-VAL(1:2).
    DISPLAY STR-VAL.
STOP RUN.
        "#)
        .expect_output("es\n")
        .run();
}