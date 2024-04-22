IDENTIFICATION DIVISION.
PROGRAM-ID. SUBSTRINGS-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 STRING-A PIC X(11).
    01 STRING-B PIC X(11) VALUE "Hello World".

PROCEDURE DIVISION.
    DISPLAY "-- Initial Values --".
    DISPLAY "String A: " STRING-A.
    DISPLAY "String B: " STRING-B.
    DISPLAY "".

    MOVE "Test" TO STRING-B(2:4).
    MOVE "Test" TO STRING-A(1:6).

    DISPLAY "-- Literal Moved Values --".
    DISPLAY "String A: " STRING-A.
    DISPLAY "String B: " STRING-B.
    DISPLAY "".

    MOVE STRING-B(0:5) TO STRING-A.
    MOVE STRING-A(2:6) TO STRING-B(2:6).

    DISPLAY "-- Variable Moved Values --".
    DISPLAY "String A: " STRING-A.
    DISPLAY "String B: " STRING-B.
    DISPLAY "".
STOP RUN.