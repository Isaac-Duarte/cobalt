use super::common::CommonTestRunner;

/// Tests that a simple integer MOVE instruction compiles.
#[test]
fn int_move() {
    CommonTestRunner::new("int_move")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. INITIAL-VALUE-TEST.

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