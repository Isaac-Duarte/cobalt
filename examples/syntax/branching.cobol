IDENTIFICATION DIVISION.
PROGRAM-ID. DATA-TEST.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 INTEGER-A PIC 99(4)9 COMP VALUE 20.
    01 STRING-A PIC X(12) VALUE "Testing".

PROCEDURE DIVISION.
    // Simple equality test.
    IF STRING-A = "Testing" THEN
        DISPLAY "String A is currently set to 'Testing'!".
        DISPLAY "STRING-A = " STRING-A.
    ELSE
        DISPLAY "This shouldn't appear.".
    END IF.

    // Inequality test.
    MOVE 50 TO INTEGER-A.
    IF NOT INTEGER-A = 50 THEN
        DISPLAY "This shouldn't appear, inequality test failed.".
    END IF.

    // Less than test.
    IF INTEGER-A < 60 THEN
        DISPLAY "Integer A is less than 60.".
        DISPLAY "INTEGER-A = " INTEGER-A.
    END IF.

    // Greater than test.
    IF INTEGER-A > 20 THEN
        DISPLAY "Integer A is greater than 20.".
        DISPLAY "INTEGER-A = " INTEGER-A.
    END IF.

    // Combined condition test.
    IF INTEGER-A > 10 AND STRING-A = "Testing" THEN
        DISPLAY "Combined condition test successful!".
    END IF.
    IF INTEGER-A < 10 AND STRING-A = "Testing" THEN
        DISPLAY "Negative combined condition test failed...".
    END IF.

    // Alternate condition test.
    IF INTEGER-A < 10 OR STRING-A = "Testing" THEN
        DISPLAY "Alternate condition test successful!".
    END IF.
    IF INTEGER-A < 10 OR STRING-A = "Foo" THEN
        DISPLAY "Negative alternate condition test failed...".
    END IF.
STOP RUN.