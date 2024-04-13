IDENTIFICATION DIVISION.
PROGRAM-ID. LOOP-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 LOOP-IDX PIC 9(2) COMP VALUE 0.

PROCEDURE DIVISION.
    DISPLAY "Beginning loop test!".

    DISPLAY "------ Direct Call ------".
    PERFORM DIRECT-CALL.
    DISPLAY "".

    DISPLAY "------ Fallthrough Call ------".
    PERFORM FALLTHROUGH-LOOP THRU FALLTHROUGH-LOOP-3.
    DISPLAY "".

    DISPLAY "------ Basic Counted Loop ------".
    PERFORM BASIC-LOOP 5 TIMES.
    MOVE 0 TO LOOP-IDX.
    DISPLAY "".

    DISPLAY "------ Basic Conditional Loop ------".
    PERFORM COND-LOOP WITH TEST AFTER UNTIL LOOP-IDX > 6.
    MOVE 0 TO LOOP-IDX.
    DISPLAY "".
    
    STOP RUN.

    DIRECT-CALL.
    DISPLAY "This function has been directly called. Yay!".

    BASIC-LOOP.
    DISPLAY "Current loop value: " LOOP-IDX.
    ADD 1 TO LOOP-IDX.

    FALLTHROUGH-LOOP.
    DISPLAY "This is the first section of the fallthrough.".

    FALLTHROUGH-LOOP-2.
    DISPLAY "This is the second section of the fallthrough.".

    FALLTHROUGH-LOOP-3.
    DISPLAY "This is the final section of the fallthrough.".

    COND-LOOP.
    DISPLAY "Current loop value: " LOOP-IDX.
    ADD 1 TO LOOP-IDX.
