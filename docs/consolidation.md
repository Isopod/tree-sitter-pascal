# Modern Delphi Support Plan

This consolidates unique features and fixes from `fschetterer`, `geraldozayit`, and `bolsen` repositories into the current `Tree-sitter-Delphi` grammar, along with new support for Delphi 12 features.

## Source Repositories & Contributors

- **[Isopod/tree-sitter-pascal](https://github.com/Isopod/tree-sitter-pascal)**: The original upstream repository.
- **[fschetterer/tree-sitter-pascal](https://github.com/fschetterer/tree-sitter-pascal)**: Frank Schetterer's fork, contributing critical parsing fixes for modern Delphi.
- **[bolsen/tree-sitter-pascal](https://github.com/bolsen/tree-sitter-pascal)**: Brian Olsen's fork, adding support for the `otherwise` keyword.
- **[wpostma/tree-sitter-pascal](https://github.com/wpostma/tree-sitter-pascal)**: Wouter Postma's upstream, providing bare `raise` support and Android/JNI identifier enhancements.
- **[jimmckeeth/Tree-sitter-Delphi](https://github.com/jimmckeeth/Tree-sitter-Delphi)**: Current repository, now acting as the unification point.

## Consolidation List (Issues, PRs, and Features)

| Item                                                                     | Source                                                                                    | Type     | Modern Delphi Relevance | Quality | Description                                                                                                                              |
| :----------------------------------------------------------------------- | :---------------------------------------------------------------------------------------- | :------- | :---------------------- | :------ | :--------------------------------------------------------------------------------------------------------------------------------------- |
| **[Isopod #18](https://github.com/Isopod/tree-sitter-pascal/issues/18)** | [tuncb](https://github.com/tuncb) / [fschetterer](https://github.com/fschetterer)         | Fix      | 10/10                   | 8/10    | **Nested generics fixes**: Resolved ambiguity where `<` was interpreted as generic start in comparison contexts (e.g., `Assert(X < Y)`). |
| **[Isopod #17](https://github.com/Isopod/tree-sitter-pascal/issues/17)** | [tuncb](https://github.com/tuncb) / [fschetterer](https://github.com/fschetterer)         | Fix      | 9/10                    | 9/10    | **Inline constants**: Support for `const` declarations within `begin...end` blocks.                                                      |
| **[Isopod #15](https://github.com/Isopod/tree-sitter-pascal/issues/15)** | [lystakata](https://github.com/lystakata) / [fschetterer](https://github.com/fschetterer) | Fix      | 9/10                    | 9/10    | **Inline variables enhancements**: Improved support for `for var Item in AList do` syntax.                                               |
| **[Isopod #19](https://github.com/Isopod/tree-sitter-pascal/issues/19)** | [wpostma](https://github.com/wpostma)                                                     | Fix      | 8/10                    | 9/10    | **$ in Identifiers**: Support for `$` in names for JNI/Android interop and special internal symbols.                                     |
| **[Isopod #20](https://github.com/Isopod/tree-sitter-pascal/pull/20)**   | [wpostma](https://github.com/wpostma)                                                     | Fix/Feat | 9/10                    | 9/10    | **Bare raise & re-publication**: Support for `raise;` without arguments and property re-publication without type redeclaration.          |
| **Multi-line Strings**                                                   | Delphi 12 (Athens)                                                                        | Feature  | 10/10                   | N/A     | **Triple quote strings** (`'''...'''`) with raw multi-line content support.                                                              |
| **Numeric Separators**                                                   | Delphi 11/12                                                                              | Feature  | 9/10                    | N/A     | **Underscores in literals**: Support for separators like `1_000_000` or `$FF_AA`.                                                        |
| **Binary Literals**                                                      | FPC / Modern Delphi                                                                       | Feature  | 8/10                    | N/A     | Support for `%1010` binary literals (FreePascal compatibility).                                                                          |
| **otherwise in case**                                                    | [bolsen](https://github.com/bolsen)                                                       | Feature  | 6/10                    | 7/10    | Support `otherwise` as a synonym for `else` in `case` statements.                                                                        |

## Feature Comparison with Current Repository

| Feature                  | Current Status      | Origin            | Action                                                   |
| :----------------------- | :------------------ | :---------------- | :------------------------------------------------------- |
| **Triple-quote Strings** | **Implemented**     | Delphi 12 Spec    | Added new rule `literalStringMultiline`                  |
| **Numeric Underscores**  | **Implemented**     | Delphi 11/12 Spec | Updated `_literalInt`/`_literalFloat` regex              |
| **Binary (%) Literals**  | **Implemented**     | FPC/Delphi Spec   | Updated `_literalInt` regex                              |
| **Inline Constants**     | **Implemented**     | Isopod #17        | Added `inlineConst` to `_statement`                      |
| **Nested Generics fix**  | **Implemented**     | Isopod #18        | Added conflict between `exprTpl` and `exprBinary`        |
| **otherwise in case**    | **Implemented**     | bolsen fork       | Updated `case` rule to allow `choice(kElse, kOtherwise)` |
| **Bare raise**           | **Already Present** | wpostma upstream  | Verified in `grammar.js`                                 |
| **$ in Identifiers**     | **Already Present** | wpostma upstream  | Verified in `identifier` regex                           |

## Implementation Details

### 1. Multi-line Strings (Delphi 12)

- **Rule:** `literalStringMultiline`.
- **Syntax:** `'''` followed by newline, then content, then `'''` on a new line.
- **Note:** The position of the closing triple-quote determines the common indentation to strip.

### 2. Numeric Separators & Binary Literals

- **Rule:** `_literalInt`, `_literalFloat`.
- **Action:** Update regexes to allow `_` after the first digit and add `%` support for binary.

### 3. Inline Variables & Constants

- **Rule:** `_statement`.
- **Action:** Add `seq($.varDef, ...semicolon)` and `seq($.declConst, ...semicolon)` to the choice.
- **Action:** Update `foreach` rule to allow `varAssignDef` as an iterator.

### 4. Nested Generics (Issue #18)

- **Strategy:** Add dynamic precedence or a conflict rule `[$.exprTpl, $.exprBinary]` to allow the parser to backtrack when a `<` is not followed by a valid template argument list.

### 5. Preprocessor (PR #16)

- **Strategy:** Update the `pp` wrapper to be more permissive about its location (e.g., between procedure header and `begin`) and investigate support for nested conditionals if performance allows.

## Verification Plan

- **New Tests:** Create `test/corpus/modern_delphi.txt` covering all new features.
- **Regression:** Ensure `test/corpus/generics-delphi.txt` still passes with the #18 fix.
