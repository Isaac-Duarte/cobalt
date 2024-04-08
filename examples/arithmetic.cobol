IDENTIFICATION DIVISION.
PROGRAM-ID. DATA-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 INTEGER-A PIC 9(4) COMP VALUE 0.
    01 INTEGER-B PIC 9(4) COMP VALUE 0.
    01 INTEGER-C PIC 9(4) COMP VALUE 0.
    01 OUTPUT-INT PIC 9(4) COMP VALUE 0.

PROCEDURE DIVISION.
    MOVE 10 TO INTEGER-A.
    MOVE 20 TO INTEGER-B.
    MOVE 30 TO INTEGER-C.

    DISPLAY " --- Addition Tests ---".
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.
    ADD INTEGER-A INTEGER-B TO INTEGER-C GIVING OUTPUT-INT.
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.
    ADD INTEGER-A TO INTEGER-B INTEGER-C.
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.

    DISPLAY "\n--- Subtraction Tests ---".
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.
    SUBTRACT INTEGER-A FROM INTEGER-C GIVING OUTPUT-INT.
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.
    SUBTRACT INTEGER-A FROM INTEGER-C.
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.
    SUBTRACT INTEGER-A INTEGER-B FROM INTEGER-C GIVING OUTPUT-INT.
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.

    DISPLAY "\n--- Multiplication Tests ---".
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.
    MULTIPLY INTEGER-A BY INTEGER-C GIVING OUTPUT-INT.
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.
    MULTIPLY INTEGER-A BY INTEGER-B.
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.
    MULTIPLY INTEGER-A INTEGER-B BY INTEGER-C GIVING OUTPUT-INT.
    DISPLAY "A = " INTEGER-A ", B = " INTEGER-B ", C = " INTEGER-C ", OUT = " OUTPUT-INT.
STOP RUN.