use super::common::CommonTestRunner;

/// Tests performing a single paragraph.
#[test]
fn perform_single() {
    CommonTestRunner::new("perform_single")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. PERFORM-SINGLE-TEST.

PROCEDURE DIVISION.
    PERFORM TEST-PARA.
    STOP RUN.

    TEST-PARA.
    DISPLAY "Hello".
        "#)
        .expect_output("Hello\n")
        .run();
}

/// Tests performing a set of paragraphs.
#[test]
fn perform_thru() {
    CommonTestRunner::new("perform_thru")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. PERFORM-THRU-TEST.

PROCEDURE DIVISION.
    PERFORM A-PARA THRU C-PARA.
    STOP RUN.

    A-PARA.
    DISPLAY "Hello".

    B-PARA.
    DISPLAY "COBOL".

    C-PARA.
    DISPLAY "World".
        "#)
        .expect_output("HelloCOBOLWorld\n\n\n")
        .run();
}

/// Tests performing until a condition is met.
#[test]
fn perform_until() {
    CommonTestRunner::new("perform_until")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. PERFORM-UNTIL-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 LIMIT-VAL PIC 9(6) COMP VALUE 0.

PROCEDURE DIVISION.
    PERFORM TEST-PARA UNTIL LIMIT-VAL >= 3.
    STOP RUN.

    TEST-PARA.
    DISPLAY "Hello".
    ADD 1 TO LIMIT-VAL.
        "#)
        .expect_output("HelloHelloHello\n\n\n")
        .run();
}

/// Tests performing a paragraph N times.
#[test]
fn perform_times() {
    CommonTestRunner::new("perform_times")
        .source(r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. PERFORM-TIMES-TEST.

PROCEDURE DIVISION.
    PERFORM TEST-PARA 3 TIMES.
    STOP RUN.

    TEST-PARA.
    DISPLAY "Hello".
        "#)
        .expect_output("HelloHelloHello\n\n\n")
        .run();
}

/// Tests backward GO TO loop program from the user example.
#[test]
fn goto_backward_loop() {
    CommonTestRunner::new("goto_backward_loop")
        .source(
            r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. BACKWARDS-GOTO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COUNTER          PIC 9(2) COMP VALUE 5.

       PROCEDURE DIVISION.
           DISPLAY "Starting COBOL Backwards GOTO Demonstration.".
           PERFORM 0002-LOOP.

       0002-LOOP.
           DISPLAY "Counter = " COUNTER.
           SUBTRACT 1 FROM COUNTER.

           IF COUNTER = 0 THEN
               PERFORM 0003-END
           END-IF.

           GOTO 0002-LOOP.

       0003-END.
           DISPLAY "Done looping via backward GOTO.".
           STOP RUN.
"#,
        )
        .expect_pass()
        .run();
}