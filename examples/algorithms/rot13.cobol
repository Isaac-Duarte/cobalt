IDENTIFICATION DIVISION.
PROGRAM-ID. ROT-ENCODER.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 INPUT-LEN PIC 9(18) COMP.
    01 INPUT-STR PIC X(5000).
    01 CUR-IDX PIC 9(18) COMP VALUE 0.
    01 CUR-CHAR PIC X(1).
    01 CUR-CODE PIC S9(3) COMP.

PROCEDURE DIVISION.
    DISPLAY "Please enter an uppercase string to encode.".
    ACCEPT INPUT-STR.
    MOVE FUNCTION LENGTH(INPUT-STR) TO INPUT-LEN.
    PERFORM CONVERT-LOOP UNTIL CUR-IDX = INPUT-LEN.
    DISPLAY INPUT-STR.
    STOP RUN.

    CONVERT-LOOP.
    MOVE INPUT-STR(CUR-IDX:1) TO CUR-CHAR.
    IF NOT CUR-CHAR = " " THEN
        PERFORM CHAR-TO-CODE
        IF NOT CUR-CODE = -1 THEN
            ADD 13 TO CUR-CODE
            PERFORM CODE-TO-CHAR
            MOVE CUR-CHAR TO INPUT-STR(CUR-IDX:)
        END-IF
    END-IF.
    ADD 1 TO CUR-IDX.

    CHAR-TO-CODE.
    MOVE -1 TO CUR-CODE.
    IF CUR-CHAR = "A" THEN
        MOVE 0 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "B" THEN
        MOVE 1 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "C" THEN
        MOVE 2 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "D" THEN
        MOVE 3 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "E" THEN
        MOVE 4 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "F" THEN
        MOVE 5 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "G" THEN
        MOVE 6 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "H" THEN
        MOVE 7 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "I" THEN
        MOVE 8 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "J" THEN
        MOVE 9 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "K" THEN
        MOVE 10 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "L" THEN
        MOVE 11 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "M" THEN
        MOVE 12 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "N" THEN
        MOVE 13 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "O" THEN
        MOVE 14 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "P" THEN
        MOVE 15 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "Q" THEN
        MOVE 16 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "R" THEN
        MOVE 17 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "S" THEN
        MOVE 18 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "T" THEN
        MOVE 19 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "U" THEN
        MOVE 20 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "V" THEN
        MOVE 21 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "W" THEN
        MOVE 22 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "X" THEN
        MOVE 23 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "Y" THEN
        MOVE 24 TO CUR-CODE
    END-IF.
    IF CUR-CHAR = "Z" THEN
        MOVE 25 TO CUR-CODE
    END-IF.

    CODE-TO-CHAR.
    MOVE FUNCTION MOD(CUR-CODE, 26) TO CUR-CODE.
    IF CUR-CODE = 0 THEN
        MOVE "A" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 1 THEN
        MOVE "B" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 2 THEN
        MOVE "C" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 3 THEN
        MOVE "D" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 4 THEN
        MOVE "E" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 5 THEN
        MOVE "F" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 6 THEN
        MOVE "G" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 7 THEN
        MOVE "H" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 8 THEN
        MOVE "I" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 9 THEN
        MOVE "J" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 10 THEN
        MOVE "K" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 11 THEN
        MOVE "L" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 12 THEN
        MOVE "M" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 13 THEN
        MOVE "N" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 14 THEN
        MOVE "O" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 15 THEN
        MOVE "P" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 16 THEN
        MOVE "Q" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 17 THEN
        MOVE "R" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 18 THEN
        MOVE "S" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 19 THEN
        MOVE "T" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 20 THEN
        MOVE "U" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 21 THEN
        MOVE "V" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 22 THEN
        MOVE "W" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 23 THEN
        MOVE "X" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 24 THEN
        MOVE "Y" TO CUR-CHAR
    END-IF.
    IF CUR-CODE = 25 THEN
        MOVE "Z" TO CUR-CHAR
    END-IF.
