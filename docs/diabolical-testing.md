# Diabolical Testing Process

*Breaking tree-sitter's grammar with a diabolically fuzzy oracle.* In developing something as complicated as a grammar, failing test cases are useful.

## Overview[<img src="diabolically-fuzzy.avif" alt="diabolically fuzzy" style="zoom:50%; float:right" />](diabolically-fuzzy.avif)

Traditional human-written tests are often limited by the developer's or tester's own understanding of the grammar. This leads to *"happy path"* testing where the tests validate the implementation's assumptions rather than the language's true boundaries. AI/LLM are not much better in this regard. They tend to prefer making tests that *pass*. 

The **Diabolical Testing Process** decouples test generation from the implementation by using the actual Delphi compiler as the source of truth and a **Structural Oracle** to predict the correct AST.

## Targeted Features

For starters, we are systematically stress-testing the following modern Delphi features that I added to the tree-sitter grammar:

1. **Ternary `if` Expressions:** `Result := if A then B else C;` (Delphi 13)
2. **Extended Logical Operators:** `is not` and `not in` (Delphi 13)
3. **Custom Managed Records:** `Initialize`, `Finalize`, and `Assign` (Delphi 10.4)
4. **Multi-line String Literals:** Triple-quoted strings `''' ... '''` (Delphi 12)
5. **Numeric Separators:** Underscores in numbers, e.g., `1_000_000` (Delphi 12)
6. **Binary Literals:** e.g., `%1010` (Delphi 12)
7. **Inline Variable Declarations:** `var I := 10;` inside code blocks (Delphi 10.3)
8. **Inline Constant Declarations:** `const C = 42;` inside code blocks (Delphi 12)
9. **The `otherwise` Keyword:** Used in `case` statements (Free Pascal / ISO Pascal)
10. **Nested Preprocessor Directives:** Recursive `$ifdef` / `$if` blocks. (not new, but a discovered gap)

## The Workflow

1. **Oracle Generation:** A fuzzer script generates random permutations of a feature. For every permutation, it generates a **Code String** and an **Expected AST** (the Oracle's prediction).
2. **Compiler Validation (Ground Truth):** The code is fed to the real Delphi compiler (`dcc32.exe`).
    * If the compiler rejects the code, it is discarded.
    * If the compiler accepts the code, it is a **verified valid edge case**.
    * **De-duplication:** Only unique code strings are kept.
3. **The Showdown:** The compliable code and it's AST pair are added as test cases and run through the Tree-sitter parser.
4. **Incompetence Detection:** A failure occurs if:
    * Tree-sitter produces an `(ERROR)` or `(MISSING)` node.
    * Tree-sitter produces a valid tree that **does not match the Oracle's Expected AST**. This reveals logical flaws in precedence or associativity.

## Success Criteria

1. **Toolchain Proof:** The fuzzer must first produce known-good compiling code to prove the environment is valid.
2. **Fuzzer Yield:** At least 20% of generated code must be unique and compile successfully. If not, the fuzzer logic is too chaotic and must be revised.
3. **Creative failure:** At least 20% of the generated code must fail to compile or the fuzzer isn't being creative enough.
4. **Grammar Conflict:** At least 20% of the compiled code should fail to match the grammar's current implementation (either via syntax error or AST mismatch). If the parser passes everything, the fuzzer isn't being "diabolical" enough and must be improved.
5. **Three-Strike Rule:** If after 3 attempts the success criteria aren't met, the strategy must be reassessed.

## Fuzzer Scripts

* [`test/fuzz-delphi.js`](../test/fuzz-delphi.js): General fuzzer for multiple features.
* [`test/fuzz-ternary.js`](../test/fuzz-ternary.js): Deep-dive oracle fuzzer for ternary expression complexity and precedence.
* [`test/fuzz-multiline.js`](../test/fuzz-multiline.js): Oracle fuzzer for multi-line string literal edge cases.
* [`test/fuzz-logical.js`](../test/fuzz-logical.js): Oracle fuzzer for logical operators (`is not`, `not in`).
* [`test/fuzz-inline.js`](../test/fuzz-inline.js): Oracle fuzzer for inline variables and `case` structure.
* [`test/fuzz-numbers.js`](../test/fuzz-numbers.js): Oracle fuzzer for numeric separators and binary literals.
* [`test/fuzz-records.js`](../test/fuzz-records.js): Oracle fuzzer for custom managed records (`class operator`).
* [`test/fuzz-preprocessor.js`](../test/fuzz-preprocessor.js): Fuzzer for nested and split preprocessor directives.
