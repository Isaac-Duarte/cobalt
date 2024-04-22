IDENTIFICATION DIVISION.
PROGRAM-ID. SUBSTRINGS-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 CUR-LEN PIC 9(5) COMP VALUE 1.
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

    DISPLAY "-- Iterative Substring --".
    MOVE "" TO STRING-A.
    DISPLAY "String A: " STRING-A.
    MOVE "Hello World" TO STRING-B.
    PERFORM ITER-SUBSTR WITH TEST AFTER UNTIL CUR-LEN > 11.
    STOP RUN.

    ITER-SUBSTR.
    MOVE STRING-B(0:CUR-LEN) TO STRING-A.
    DISPLAY "String A: " STRING-A " (len = " CUR-LEN ")".
    ADD 1 TO CUR-LEN.
