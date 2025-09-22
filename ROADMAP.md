# Cobalt Compiler Roadmap

A living, prioritized plan for feature work. Scope is focused on pragmatic COBOL subsets that unlock real programs while keeping codegen simple and fast.

## Guiding Principles
- Keep it compilable: add syntax only when semantics + codegen are clear.
- Small PRs, strong tests: each feature ships with unit tests and one integration example.
- Prefer pure Rust + Cranelift solutions; keep intrinsics limited and explicit.

## Milestones

### M1 — Control Flow + Core Data (current)
- [x] IF / ELSE / END-IF (boolean ops, simple comparisons)
- [x] PERFORM Single / THRU / TIMES / UNTIL (WITH TEST BEFORE/AFTER)
- [x] EXIT PARAGRAPH, STOP RUN
- [x] GOTO paragraph (implemented as call + return)
- [x] DISPLAY / ACCEPT (ints, floats, strings)
- [x] PIC parsing: 9/A/X/V/P/S; COMP numeric; string lengths; literals
- [x] Intrinsics: LENGTH, INTEGER, RANDOM, MOD

Acceptance: All unit tests passing via `./test.sh`; examples under `examples/` compile and run.

### M2 — Data Model Expansion
- [ ] Group items & levels: 01/02-49 elementary + groups
- [ ] OCCURS (arrays), fixed-size
- [ ] REDEFINES (intra-storage overlay)
- [ ] 88-level condition names (boolean aliases)
- [ ] Figurative constants: ZERO[S], SPACE[S], HIGH-VALUES, LOW-VALUES, QUOTE, ALL
- [ ] Editing picture support for DISPLAY (Z, currency, sign, justification)

Acceptance:
- Parsing/AST support; codegen support where applicable.
- New tests for array moves, redef overlay copies, and 88-level conditions in IF/EVALUATE.

### M3 — Procedure Enhancements
- [ ] PERFORM VARYING (simple counters first; step and nested varying later)
- [ ] EVALUATE (single selector; basic WHEN; fall-through not required)
- [ ] COMPUTE (expressions, precedence, ROUNDED; basic ON SIZE ERROR handling)
- [ ] NEXT SENTENCE, CONTINUE
- [ ] GO TO DEPENDING ON (restricted variant)

Acceptance:
- End-to-end tests for loops/switch; arithmetic expression parser and evaluator.

### M4 — String and Text Processing
- [ ] STRING / UNSTRING
- [ ] INSPECT (TALLYING, REPLACING) — minimal subset
- [ ] Enhanced ACCEPT (date/time/env subsets) — optional

Acceptance:
- Compare outputs to expected byte-for-byte for typical transformations.

### M5 — Interop and Modularity
- [ ] CALL … USING (static first; simple value/by-reference conventions)
- [ ] LINKAGE SECTION, PROCEDURE DIVISION USING …
- [ ] LOCAL-STORAGE SECTION (zero-initialized per call)

Acceptance:
- Cross-program calls inside tests; argument passing validated.

### M6 — Files (Sequential)
- [ ] FILE SECTION, FD definitions (sequential line mode)
- [ ] OPEN/READ/WRITE/REWRITE/DELETE/CLOSE (basic status handling)

Acceptance:
- Read/write golden files in tests; simple record layouts.

### M7 — Codegen/Tooling Quality
- [ ] IR printing toggles by function pattern (already present) — expand
- [ ] Better diagnostics/spans across divisions
- [ ] Performance pass: hot paths, Cranelift opts, mold/ld flags sanity

---

## Detailed Task Backlog

1. PERFORM VARYING
   - Parse: `PERFORM <para> VARYING <var> FROM <start> BY <step> UNTIL <cond>` (restricted)
   - Codegen: loop header/body/trailer blocks; bound checks like UNTIL
   - Tests: nested varying not required initially

2. EVALUATE
   - Parse: `EVALUATE <value> WHEN <literal|range> ... WHEN OTHER` (simple scalar match)
   - Codegen: compare chain into blocks, else fall-through; no complex ranges initially

3. COMPUTE
   - Parse: infix expressions with parentheses, precedence, unary +/-
   - Semantics: integer/float types, ROUNDED flag; minimal ON SIZE ERROR
   - Codegen: expression tree to Cranelift; temp vars; conversions

4. Data Groups and OCCURS
   - Parse: nested levels; `OCCURS n TIMES`
   - Layout: contiguous byte layout; indexing for `MOVE`/substring
   - Tests: indexed moves, element copy, group MOVE

5. REDEFINES and 88-Level
   - Shared storage views; condition-name evaluation in IF/EVALUATE

6. STRING/UNSTRING and INSPECT
   - Implement via intrinsics where needed for simplicity

7. CALL/LINKAGE/USING
   - Conventions: i64/f64/pointer first; map COBOL BY REFERENCE/VALUE to Rust/CLIF calling conv

8. Files (Sequential)
   - Minimal line-sequential; map to libc `fopen/fgets/fputs/fclose`

9. Figurative Constants
   - Lexer + parser support; materialize values in codegen

10. Formatting/Editing
   - PIC editing for DISPLAY: signs, zero suppression

## Risks / Unknowns
- Ambiguity in classic COBOL vs pragmatic subset — document deviations.
- Packed decimal (COMP-3) requires custom runtime helpers.
- Full file system semantics are large; start minimal and opt-in.

## Tooling
- Commands
  ```sh
  ./build.sh
  ./test.sh
  ./benchmark.sh --help
  ```
- Dev notes: prefer adding tests under `crates/compiler/src/tests/`; examples under `examples/`.
