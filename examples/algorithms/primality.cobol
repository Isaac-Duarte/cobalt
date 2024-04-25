IDENTIFICATION DIVISION.
PROGRAM-ID. FIZZ-BUZZ.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 MAX-VAL PIC 9(9) COMP.
    01 CUR-VAL PIC 9(9) COMP VALUE 1.
    01 PREV-VAL PIC 9(9) COMP VALUE 0.
    01 MOD-RESULT PIC 9(4) COMP.
    01 IS-PRIME PIC 9(1) COMP.
    01 EVEN-VAL PIC 9(9) COMP.
    01 ODD-DIVISOR PIC 9(9) COMP.
    01 LOOP-IDX PIC 9(9) COMP.
    01 LOOP-LIMIT PIC 9(9) COMP.
    01 CERTAINTY PIC 9(2) COMP VALUE 5.
    01 CERTAINTY-ITER PIC 9(2) COMP.

    // I hate that this is required...
    01 TWO PIC 9(1) COMP VALUE 2.
    01 MINUS-ONE PIC 9(1) COMP VALUE -1.

    01 RANDOM-INT PIC 9(9) COMP.
    01 RANDOM-THRESH PIC 9(9) COMP.

    01 MOD-POW-BASE PIC 9(9) COMP.
    01 MOD-POW-EXP PIC 9(9) COMP.
    01 MOD-POW-MOD PIC 9(9) COMP.
    01 MOD-POW-LOOP-IDX PIC 9(9) COMP.
    01 MOD-POW-RESULT PIC 9(100) COMP.

PROCEDURE DIVISION.
    DISPLAY "Please enter a certainty (iter) value.".
    ACCEPT CERTAINTY.
    DISPLAY "Please enter a maximum value.".
    ACCEPT MAX-VAL.
    PERFORM TEST-LOOP UNTIL CUR-VAL > MAX-VAL.
    STOP RUN.

    TEST-LOOP.
    PERFORM MILLER-RABIN-CHECK.
    IF IS-PRIME = 1 THEN
        DISPLAY CUR-VAL " (p)".
    END IF.
    ADD 1 TO CUR-VAL.

    MILLER-RABIN-CHECK.
    // Pre-calculate variables for this check.
    SUBTRACT 1 FROM CUR-VAL GIVING PREV-VAL.
    MOVE 1 TO IS-PRIME.
    MOVE 0 TO CERTAINTY-ITER.

    // Filter out basic primes.
    IF CUR-VAL = 2 OR CUR-VAL = 3 OR CUR-VAL = 5 OR CUR-VAL = 7 OR CUR-VAL = 11 OR CUR-VAL = 13 OR CUR-VAL = 17 OR CUR-VAL = 19 OR CUR-VAL = 23 THEN
        EXIT PARAGRAPH.
    END IF.
    IF CUR-VAL <= 28 THEN
        MOVE 0 TO IS-PRIME.
        EXIT PARAGRAPH.
    END IF.

    // If below 2 or even, not a prime.
    MOVE FUNCTION MOD(CUR-VAL, 2) TO MOD-RESULT.
    IF CUR-VAL < 2 OR MOD-RESULT = 0 THEN
        MOVE 0 TO IS-PRIME.
        EXIT PARAGRAPH.
    END IF.

    // Find the even integer below the number.
    SUBTRACT 1 FROM CUR-VAL GIVING EVEN-VAL.

    // Find the lowest odd divisor.
    MOVE 0 TO ODD-DIVISOR.
    MOVE FUNCTION MOD(EVEN-VAL, 2) TO MOD-RESULT.
    PERFORM FIND-LOWEST-ODD-DIVISOR UNTIL NOT MOD-RESULT = 0.

    // Run N iterations of the test.
    SUBTRACT 2 FROM EVEN-VAL GIVING RANDOM-THRESH.
    PERFORM MILLER-RABIN-ITER UNTIL CERTAINTY-ITER >= CERTAINTY.
    
    MILLER-RABIN-ITER.
    // Generate a new valid random number.
    MOVE 0 TO RANDOM-INT.
    IF RANDOM-THRESH > 2 THEN
        PERFORM GENERATE-RANDOM-INT UNTIL RANDOM-INT >= 2 AND RANDOM-INT < RANDOM-THRESH.
    ELSE
        // CUR-VAL is one more than a power of two, therefore the lowest odd divisor of CUR-VAL-1 is... 1.
        // We can't really generate a random value here.
        MOVE 2 TO RANDOM-INT.
    END IF.

    // Check for x=1 or x=even-1.
    MOVE RANDOM-INT TO MOD-POW-BASE.
    MOVE EVEN-VAL TO MOD-POW-EXP.
    MOVE CUR-VAL TO MOD-POW-MOD.
    PERFORM MOD-POW.
    IF MOD-POW-RESULT = 1 OR MOD-POW-RESULT = PREV-VAL THEN
        // Continue outer loop.
        ADD 1 TO CERTAINTY-ITER.
        EXIT PARAGRAPH.
    END IF.

    // Iterate to check for prime.
    MOVE 1 TO LOOP-IDX.
    SUBTRACT 1 FROM ODD-DIVISOR GIVING LOOP-LIMIT.
    PERFORM MILLER-RABIN-INNER-ITER UNTIL LOOP-IDX >= LOOP-LIMIT.

    // Prime rejection?
    IF NOT MOD-POW-RESULT = PREV-VAL THEN
        MOVE 0 TO IS-PRIME.
        MOVE CERTAINTY TO CERTAINTY-ITER.
        EXIT PARAGRAPH.
    END IF.

    // Bump loop index.
    ADD 1 TO CERTAINTY-ITER.

    MILLER-RABIN-INNER-ITER.
    // Recursive modpow.
    MOVE MOD-POW-RESULT TO MOD-POW-BASE.
    MOVE 2 TO MOD-POW-EXP.
    MOVE CUR-VAL TO MOD-POW-MOD.
    PERFORM MOD-POW.

    // Check if this resulted in a prime rejection.
    IF MOD-POW-RESULT = 1 THEN
        MOVE 0 TO IS-PRIME.
        MOVE LOOP-LIMIT TO LOOP-IDX.
        EXIT PARAGRAPH.
    END IF.

    // Not a rejection, but are we done iterating?
    IF MOD-POW-RESULT = PREV-VAL THEN
        MOVE LOOP-LIMIT TO LOOP-IDX.
        EXIT PARAGRAPH.
    END IF.

    // Bump loop index.
    ADD 1 TO LOOP-IDX.

    ///////////////////////
    // UTILITY FUNCTIONS //
    ///////////////////////

    // Finds the lowest odd divisor of EVEN-VAL, modifying it.
    FIND-LOWEST-ODD-DIVISOR.
    DIVIDE EVEN-VAL BY TWO GIVING EVEN-VAL.
    ADD 1 TO ODD-DIVISOR.
    MOVE FUNCTION MOD(EVEN-VAL, 2) TO MOD-RESULT.

    // Generates a single random positive integer, always between 0 and RANDOM-THRESH-1.
    GENERATE-RANDOM-INT.
    MOVE FUNCTION RANDOM() TO RANDOM-INT.
    IF RANDOM-INT < 0 THEN
        MULTIPLY RANDOM-INT BY MINUS-ONE GIVING RANDOM-INT.
    END IF.
    MOVE FUNCTION MOD(RANDOM-INT, RANDOM-THRESH) TO RANDOM-INT.

    // Performs (x ^ y) % z.
    // Uses a memory-efficient method rather than direct to avoid horrific overflows.
    MOD-POW.
    MOVE 1 TO MOD-POW-RESULT.
    MOVE 0 TO MOD-POW-LOOP-IDX.
    PERFORM MOD-POW-ITER UNTIL MOD-POW-LOOP-IDX >= MOD-POW-EXP.

    MOD-POW-ITER.
    // c = (c * b) % m, where m = MOD-POW-MOD & b = MOD-POW-BASE.
    MULTIPLY MOD-POW-RESULT BY MOD-POW-BASE GIVING MOD-POW-RESULT.
    MOVE FUNCTION MOD(MOD-POW-RESULT, MOD-POW-MOD) TO MOD-POW-RESULT.
    ADD 1 TO MOD-POW-LOOP-IDX.
