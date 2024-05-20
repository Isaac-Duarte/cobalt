use super::common::CommonTestRunner;

/// Tests that a simple integer MOVE instruction compiles.
#[test]
fn int_lit_move() {
    CommonTestRunner::new("int_lit_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INT-LIT-MOV.

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
fn float_lit_move() {
    CommonTestRunner::new("float_lit_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. FLT-LIT-MOV.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 FLT-VAL PIC S9(4)P9(4) COMP.

PROCEDURE DIVISION.
    MOVE -1234.5678 TO FLT-VAL.
STOP RUN.
        "#)
        .expect_pass()
        .run();
}

/// Tests that a simple string literal MOVE instruction compiles.
#[test]
fn str_lit_move() {
    CommonTestRunner::new("str_lit_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-LIT-MOV.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE "SomeString" TO STR-VAL.
STOP RUN.
        "#)
        .expect_pass()
        .run();
}

/// Tests that an OOB string literal MOVE instruction fails to compile.
#[test]
fn str_inv_lit_move() {
    CommonTestRunner::new("str_inv_lit_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-INV-LIT-MOV.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STR-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE "SomeTooLargeString" TO STR-VAL.
STOP RUN.
        "#)
        .expect_fail(Some("move incompatible literal"))
        .run();
}

/// Tests that a spanned source string MOVE instruction behaves as expected.
#[test]
fn str_spanned_src_move() {
    CommonTestRunner::new("str_spanned_src_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-SPANNED-SRC-MOV.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 SRC-VAL PIC X(10) VALUE "SomeString".
    01 DEST-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE SRC-VAL(1:4) TO DEST-VAL.
    DISPLAY DEST-VAL.
STOP RUN.
        "#)
        .expect_output("Some\n")
        .run();
}

/// Tests that a spanned destination MOVE instruction behaves as expected.
#[test]
fn str_spanned_dest_move() {
    CommonTestRunner::new("str_spanned_dest_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-SPANNED-DEST-MOV.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 SRC-VAL PIC X(10) VALUE "SomeString".
    01 DEST-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE SRC-VAL TO DEST-VAL(4:4).
    DISPLAY DEST-VAL.
STOP RUN.
        "#)
        .expect_output("   Some\n")
        .run();
}

/// Tests that a spanned source and destination MOVE instruction
/// behaves as expected.
#[test]
fn str_spanned_src_dest_move() {
    CommonTestRunner::new("str_spanned_src_dest_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STR-SPANNED-SRC-DEST-MOV.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 SRC-VAL PIC X(10) VALUE "SomeString".
    01 DEST-VAL PIC X(10).

PROCEDURE DIVISION.
    MOVE SRC-VAL(5:6) TO DEST-VAL(2:3).
    DISPLAY DEST-VAL.
STOP RUN.
        "#)
        .expect_output(" Str\n")
        .run();
}