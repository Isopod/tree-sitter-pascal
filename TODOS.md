# Delphi Grammar Modernization TODOs

Based on the evolution outlined in `docs/Delphi-Grammar-Evolution.md`, here are the language features that are currently missing or require further verification in the grammar:

## Missing Features (To Be Implemented)

### 1. Nested Preprocessor Directives
*   **Status:** Missing
*   **Description:** The current `pp` wrapper in `grammar.js` only supports a single level of `$ifdef` and treats directives as extra/whitespace tokens. A test case demonstrating the failure is in `test/corpus/preprocessor_nested.txt`.
*   **Implementation Plan:**
    *   Refactor the preprocessor support in `grammar.js` to allow for recursive/nested conditional directives. This may involve moving `$ifdef` from a JavaScript wrapper/extra to a structural grammar rule that can nest recursively within `_definition` and other block contexts.

## Completed Features

### 1. Delphi 13 Florence: Ternary `if` Expression
*   **Status:** Complete
*   **Description:** Delphi 13 introduced `if` as an inline expression (e.g., `Result := if A > B then A else B;`). Currently, `if` is only defined as a statement.
*   **Implementation Plan:** 
    *   Create an `exprIf` rule in `grammar.js`.
    *   Add `exprIf` to the `_expr` choice.
    *   Assign it the correct (lowest) precedence so it interacts properly with mathematical operators, requiring parentheses for concatenation.

### 2. Delphi 13 Florence: `is not` and `not in` Operators
*   **Status:** Complete
*   **Description:** Delphi 13 added unified comparative operators `is not` and `not in`. Currently, the `exprBinary` rule only supports `is` and `in`.
*   **Implementation Plan:**
    *   Update `exprBinary` to include `seq($.kIs, $.kNot)` and `seq($.kNot, $.kIn)` at the same precedence level (1) as `is` and `in`.

### 3. Delphi 10.4 Sydney: Custom Managed Records
*   **Status:** Complete
*   **Description:** `class operator Initialize(out Dest: T);` and `Finalize`.
*   **Implementation Plan:** The `_operatorName` rule in the grammar already accepts standard identifiers (via `_genericName`), so these declarations *should* already parse correctly. We need to add explicit tests to the corpus (e.g., in `routines.txt` or a new `modern_delphi.txt` section) to guarantee the AST produces the correct `declOperator` nodes.

---

*Note: The Delphi 13 `noreturn` directive is already implemented in the grammar (`kNoreturn` exists in `procAttribute`).*
