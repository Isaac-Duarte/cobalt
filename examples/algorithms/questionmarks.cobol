IDENTIFICATION DIVISION.
PROGRAM-ID. QUESTION-MARKS.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 INPUT-LEN PIC 9(5) COMP.
    01 INPUT-STR PIC X(1000).
    01 CUR-IDX PIC 9(5) COMP VALUE 1.
    01 CUR-CHAR PIC X(1).
    01 OUT-NUM PIC S9(1) COMP.
    01 LAST-NUM PIC S9(1) COMP VALUE -1.
    01 NUM-SUM PIC S9(2) COMP.
    01 NUM-QMARKS PIC 9(5) COMP VALUE 0.
    01 CONFORMANT PIC 9(1) COMP VALUE 1.

PROCEDURE DIVISION.
    DISPLAY "Please enter a string to analyse.".
    ACCEPT INPUT-STR.
    MOVE FUNCTION LENGTH(INPUT-STR) TO INPUT-LEN.
    PERFORM ANALYSIS-LOOP UNTIL CUR-IDX > INPUT-LEN.
    IF CONFORMANT = 1 THEN
        DISPLAY "The string is conformant with the specification."
    ELSE
        DISPLAY "The string is not conformant with the specification."
    END-IF.
    STOP RUN.

    ANALYSIS-LOOP.
    MOVE INPUT-STR(CUR-IDX:1) TO CUR-CHAR.
    IF CUR-CHAR = "?" THEN
        ADD 1 TO NUM-QMARKS
    ELSE
        PERFORM CHAR-TO-NUM
        IF NOT OUT-NUM = -1 THEN
            PERFORM CHECK-NUMBERS
        END-IF
    END-IF.
    ADD 1 TO CUR-IDX.

    CHECK-NUMBERS.
    MOVE 0 TO NUM-SUM.
    ADD LAST-NUM OUT-NUM TO NUM-SUM.
    IF NOT LAST-NUM = -1 THEN
        *> check if this is conformant with gnucobol!
        IF NUM-SUM = 10 AND NOT NUM-QMARKS = 3 THEN
            MOVE 0 TO CONFORMANT
            MOVE INPUT-LEN TO CUR-IDX
        END-IF
    END-IF.
    MOVE 0 TO NUM-QMARKS.
    MOVE OUT-NUM TO LAST-NUM.

    CHAR-TO-NUM.
    MOVE -1 TO OUT-NUM.
    IF CUR-CHAR = "0" THEN
        MOVE 0 TO OUT-NUM
    END-IF.
    IF CUR-CHAR = "1" THEN
        MOVE 1 TO OUT-NUM
    END-IF.
    IF CUR-CHAR = "2" THEN
        MOVE 2 TO OUT-NUM
    END-IF.
    IF CUR-CHAR = "3" THEN
        MOVE 3 TO OUT-NUM
    END-IF.
    IF CUR-CHAR = "4" THEN
        MOVE 4 TO OUT-NUM
    END-IF.
    IF CUR-CHAR = "5" THEN
        MOVE 5 TO OUT-NUM
    END-IF.
    IF CUR-CHAR = "6" THEN
        MOVE 6 TO OUT-NUM
    END-IF.
    IF CUR-CHAR = "7" THEN
        MOVE 7 TO OUT-NUM
    END-IF.
    IF CUR-CHAR = "8" THEN
        MOVE 8 TO OUT-NUM
    END-IF.
    IF CUR-CHAR = "9" THEN
        MOVE 9 TO OUT-NUM
    END-IF.
