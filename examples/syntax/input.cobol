IDENTIFICATION DIVISION.
PROGRAM-ID. INPUT-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 INPUT-STORE-STR PIC X(25) VALUE "Initial".
    01 INPUT-STORE-INT PIC 9(4) COMP VALUE 0.
    01 INPUT-STORE-FLOAT PIC 9(4)P9(4) COMP VALUE 0.0.

PROCEDURE DIVISION.
    DISPLAY "Please input some string test data (under 25 chars).".
    ACCEPT INPUT-STORE-STR.
    DISPLAY "Please input some integer test data (under 25 chars).".
    ACCEPT INPUT-STORE-INT.
    DISPLAY "Please input some float test data (under 25 chars).".
    ACCEPT INPUT-STORE-FLOAT.

    DISPLAY "\n--- Results ---".
    DISPLAY "String: " INPUT-STORE-STR.
    DISPLAY "Integer: " INPUT-STORE-INT.
    DISPLAY "Float: " INPUT-STORE-FLOAT.
    STOP RUN.