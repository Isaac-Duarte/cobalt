       IDENTIFICATION DIVISION.
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
