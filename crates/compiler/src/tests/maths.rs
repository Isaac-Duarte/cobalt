use super::common::CommonTestRunner;

/// Tests that adding two integer values works as expected.
#[test]
fn add_int() {
    CommonTestRunner::new("add_int")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. ADD-INT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 6.
    01 B-VAL PIC 9(6) COMP VALUE -4.

PROCEDURE DIVISION.
    ADD A-VAL TO B-VAL.
    DISPLAY B-VAL.
STOP RUN.
        "#)
        .expect_output("2\n")
        .run();
}

/// Tests that adding an integer value into two outputs works as expected.
#[test]
fn add_int_two_outputs() {
    CommonTestRunner::new("add_int_two_outputs")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. ADD-INT-MULT-OUT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 6.
    01 B-VAL PIC 9(6) COMP VALUE 3.
    01 C-VAL PIC 9(6) COMP VALUE -4.

PROCEDURE DIVISION.
    ADD A-VAL TO B-VAL C-VAL.
    DISPLAY B-VAL " " C-VAL.
STOP RUN.
        "#)
        .expect_output("9 2\n")
        .run();
}

/// Tests that adding two integer values into one output resulting works as expected.
#[test]
fn add_int_resulting() {
    CommonTestRunner::new("add_int_resulting")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. ADD-INT-RESULTING-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 6.
    01 B-VAL PIC 9(6) COMP VALUE 2.
    01 C-VAL PIC 9(6) COMP VALUE 4.
    01 OUT-VAL PIC 9(6) COMP.

PROCEDURE DIVISION.
    ADD A-VAL B-VAL TO C-VAL GIVING OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("12\n")
        .run();
}

/// Tests that subtracting two integer values works as expected.
#[test]
fn sub_int() {
    CommonTestRunner::new("sub_int")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. SUB-INT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 2.
    01 B-VAL PIC 9(6) COMP VALUE 4.

PROCEDURE DIVISION.
    SUBTRACT A-VAL FROM B-VAL.
    DISPLAY B-VAL.
STOP RUN.
        "#)
        .expect_output("2\n")
        .run();
}

/// Tests that subtracting an integer value into two outputs works as expected.
#[test]
fn sub_int_two_outputs() {
    CommonTestRunner::new("sub_int_two_outputs")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. SUB-INT-MULT-OUT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 6.
    01 B-VAL PIC 9(6) COMP VALUE 3.
    01 C-VAL PIC 9(6) COMP VALUE 6.

PROCEDURE DIVISION.
    SUBTRACT A-VAL FROM B-VAL C-VAL.
    DISPLAY B-VAL " " C-VAL.
STOP RUN.
        "#)
        .expect_output("-3 0\n")
        .run();
}

/// Tests that subtracting two integer values from one output resulting works as expected.
#[test]
fn sub_int_resulting() {
    CommonTestRunner::new("sub_int_resulting")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. SUB-INT-RESULTING-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 6.
    01 B-VAL PIC 9(6) COMP VALUE 2.
    01 C-VAL PIC 9(6) COMP VALUE 10.
    01 OUT-VAL PIC 9(6) COMP.

PROCEDURE DIVISION.
    SUBTRACT A-VAL B-VAL FROM C-VAL GIVING OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("2\n")
        .run();
}

/// Tests that multiplying two integer values works as expected.
#[test]
fn mul_int() {
    CommonTestRunner::new("mul_int")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. MUL-INT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 6.
    01 B-VAL PIC 9(6) COMP VALUE 2.

PROCEDURE DIVISION.
    MULTIPLY A-VAL BY B-VAL.
    DISPLAY B-VAL.
STOP RUN.
        "#)
        .expect_output("12\n")
        .run();
}

/// Tests that multiplying an integer value into two outputs works as expected.
#[test]
fn mul_int_two_outputs() {
    CommonTestRunner::new("mul_int_two_outputs")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. MUL-INT-MULT-OUT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 6.
    01 B-VAL PIC 9(6) COMP VALUE 3.
    01 C-VAL PIC 9(6) COMP VALUE -4.

PROCEDURE DIVISION.
    MULTIPLY A-VAL BY B-VAL C-VAL.
    DISPLAY B-VAL " " C-VAL.
STOP RUN.
        "#)
        .expect_output("18 -24\n")
        .run();
}

/// Tests that multiplying two integer values into one output resulting works as expected.
#[test]
fn mul_int_resulting() {
    CommonTestRunner::new("mul_int_resulting")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. MUL-INT-RESULTING-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 6.
    01 B-VAL PIC 9(6) COMP VALUE 2.
    01 C-VAL PIC 9(6) COMP VALUE 4.
    01 OUT-VAL PIC 9(6) COMP.

PROCEDURE DIVISION.
    MULTIPLY A-VAL B-VAL BY C-VAL GIVING OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("48\n")
        .run();
}

/// Tests that dividing two integer values works as expected.
#[test]
fn div_int() {
    CommonTestRunner::new("div_int")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. DIV-INT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 2.
    01 B-VAL PIC 9(6) COMP VALUE 6.

PROCEDURE DIVISION.
    DIVIDE A-VAL INTO B-VAL.
    DISPLAY B-VAL.
STOP RUN.
        "#)
        .expect_output("3\n")
        .run();
}

/// Tests that dividing two integer values into a float works as expected.
#[test]
fn div_int_into_float() {
    CommonTestRunner::new("div_int_into_float")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. DIV-INT-INTO-FLT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 A-VAL PIC 9(6) COMP VALUE 4.
    01 B-VAL PIC 9(6) COMP VALUE 6.
    01 OUT-VAL PIC 9(6)P9(4) COMP.

PROCEDURE DIVISION.
    DIVIDE B-VAL BY A-VAL GIVING OUT-VAL.
    DISPLAY OUT-VAL.
STOP RUN.
        "#)
        .expect_output("1.5\n")
        .run();
}