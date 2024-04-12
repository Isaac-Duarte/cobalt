IDENTIFICATION DIVISION.
PROGRAM-ID. PARAGRAPH-TEST.

PROCEDURE DIVISION.
    PARAGRAPH-ONE.
    DISPLAY "This is the first paragraph in the program.".

    PARAGRAPH-TWO.
    DISPLAY "This is the second paragraph in the program, and is called via. fallthrough.".
    STOP RUN.

    PARAGRAPH-THREE.
    DISPLAY "This paragraph exists, but is never called.".
