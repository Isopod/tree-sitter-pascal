# Contributing to Tree-Sitter-Pascal

Pull requests are welcome! This project doesn't have a strict process — reasonable changes with a test or two are enough to get started. Please read our [Code of Conduct](CODE_OF_CONDUCT.md) before participating.

## Contents

- [Getting Started](#getting-started)
- [Pull Request Guidelines](#pull-request-guidelines)
- [Written in JavaScript and not Delphi?](#written-in-javascript-and-not-delphi)
- [Repository Organization](#repository-organization)
- [Grammar Conventions](#grammar-conventions)
- [Test Status](#test-status)
- [Diabolical Testing](#diabolical-testing)
- [Testing](#testing)
- [Pre-built Binaries](#pre-built-binaries)

## Getting Started

1. Fork and clone the repository, then install the Node.js dependencies used to generate and test the parser:

   ```bash
   cd bindings/node
   npm install
   ```

2. Make your change in `grammar.js` (the grammar source — never hand-edit the generated `src/parser.c`).
3. Regenerate the parser and run the test suite from the repository root:

   ```bash
   npx tree-sitter generate
   npx tree-sitter test
   ```

4. If you're not sure where to start, look for:
   - Rules marked _No explicit test found_ in [`docs/rules.md`](docs/rules.md) — these are grammar rules without corpus coverage yet.
   - Failing tests noted in the [Test Status](#test-status) table below.
   - Open issues on [GitHub](https://github.com/jimmckeeth/tree-sitter-pascal/issues), especially ones covering a Pascal/Delphi language feature that isn't parsed correctly yet.

If you're new to tree-sitter grammars generally, the [official tree-sitter documentation](https://tree-sitter.github.io/tree-sitter/creating-parsers) is the best starting point for understanding how `grammar.js` compiles down to a parser.

## Pull Request Guidelines

Nothing here is a hard blocker — use your judgment:

- Add or update a [corpus test](#testing) for any grammar change so we can see it working and keep it from regressing later.
- Small, focused PRs are easier to review than large ones, but if a change naturally touches several rules, that's fine too.
- Run `npx tree-sitter test` locally before opening the PR; CI will run it again on every push.
- If your change affects the public rule set, `docs/rules.md` and this file's test summary are regenerated automatically by CI — no need to update them by hand.
- Not sure if an approach is right? Open a draft PR or an issue and ask — happy to talk it through before you invest a lot of time.

## Written in JavaScript and not Delphi?

I get this question a lot. You could certainly rewrite the whole Tree-Sitter stack in Delphi, but I don't think that makes sense. First of all I'm a pragmatist and just want to use what works. It could be an interesting exercise to rewrite it (or even have an AI do it) but what does that gain us? The main goal for this is compatibility in the wider ecosystem, so there is an advantage of having it written in the same language as the other grammars. If we re-wrote it in Delphi then we could end up with a two different forks, which just divides the effort.

## Repository Organization

To keep the root directory clean, the repository is organized as follows:

- **`bindings/`**: Contains language-specific bindings and their package manager files (e.g., `package.json`, `setup.py`, `Cargo.toml`). Includes bindings for C, Go, Node.js, Python, Rust, Swift, and [Delphi](https://github.com/jimmckeeth/delphi-tree-sitter) (submodule at `bindings/delphi/`).
- **`docs/`**: Documentation, auto-generated rule coverage (`rules.md`), branding assets, and the [Delphi Win64 binding guide](docs/delphi-win64-binding.md).
- **`Libs/`**: Local cache of pre-built native libraries (populated by `scripts/build.ps1`). Release artifacts are published to [GitHub Releases](https://github.com/jimmckeeth/tree-sitter-pascal/releases/latest).
- **`scripts/`**: Contains utility scripts for building (`build.ps1`), cleaning (`clean.ps1`), and checking prerequisites (`ensure-prereq.ps1`).
- **`src/`**: The generated C parser and header files. Edit `grammar.js`, not these files directly.
- **`queries/`**: Tree-sitter query files for syntax highlighting and local variables.
- **`test/`**: The test corpus and fuzzing scripts.
- **`examples/`**: Example Pascal files for testing and demonstration.

## Grammar Conventions

Rule names in `grammar.js` follow a loose prefix convention that the tooling (and `docs/rules.md`) relies on to categorize rules. When adding a new rule, try to match the existing pattern for its kind:

| Prefix / pattern                                | Category                                            | Example                |
| :---------------------------------------------- | :-------------------------------------------------- | :--------------------- |
| `k...` (starts with lowercase `k`)              | Keywords & terminals                                | `kBegin`, `kEnd`       |
| `decl...`, `def...`, `typeref...`, `generic...` | Declarations & definitions                          | `declConst`, `defProc` |
| `expr...`                                       | Expressions                                         | `exprBinary`           |
| `literal...`                                    | Literals                                            | `literalNumber`        |
| `_...` (leading underscore)                     | Internal helper rules (not part of the public tree) | `_statement`           |

Other rules (statements, high-level structure like `program`/`unit`) don't need a prefix — see `getCategory()` in `docs/export_rules.js` for the full classification logic if you want the details.

Feature flags at the top of `grammar.js` (e.g. `delphi`, `fpc`, `rtti`, `lambda`) gate dialect-specific syntax. If you're adding a feature that's specific to one Pascal dialect, guard it behind the relevant flag rather than enabling it unconditionally.

## Test Status

Using a [fuzzy diabolical testing](#diabolical-testing) process to produce more failing tests to improve the grammar accuracy. Currently all the failing tests are related to multiline strings.

<!-- TEST_SUMMARY_START -->

| Category                                                             |  Rules  | Tested  | Untested | Total Tests | Passing  | Failing |
| :------------------------------------------------------------------- | :-----: | :-----: | :------: | :---------: | :------: | :-----: |
| [Declarations & Definitions](docs/rules.md#declarations-definitions) |   51    |   42    |    9     |    2753     |   2753   |    0    |
| [Expressions](docs/rules.md#expressions)                             |   13    |   11    |    2     |    1667     |   1667   |    0    |
| [High-Level Structure](docs/rules.md#high-level-structure)           |    9    |    9    |    0     |    2759     |   2759   |    0    |
| [Internal Helpers](docs/rules.md#internal-helpers)                   |   26    |    0    |    26    |      0      |    0     |    0    |
| [Keywords & Terminals](docs/rules.md#keywords-terminals)             |   163   |   112   |    51    |    2758     |   2758   |    0    |
| [Literals](docs/rules.md#literals)                                   |    7    |    6    |    1     |    1717     |   1717   |    0    |
| [Other](docs/rules.md#other)                                         |   12    |    5    |    7     |    2759     |   2759   |    0    |
| [Statements](docs/rules.md#statements)                               |   24    |   23    |    1     |    2655     |   2655   |    0    |
| **TOTAL**                                                            | **305** | **208** |  **97**  |  **2760**   | **2760** |  **0**  |

<!-- TEST_SUMMARY_END -->

## Diabolical Testing

To ensure the grammar's robustness beyond simple "happy path" scenarios, we use a [Diabolical Testing Process](docs/diabolical-testing.md). This involves fuzzing valid Delphi code through the actual compiler and comparing the resulting Tree-sitter AST against a structural oracle. This process specifically targets complex edge cases in modern Delphi features to identify logical flaws in precedence, associativity, and structure.

## Testing

Corpus tests live in `test/corpus/*.txt`. Each test is a `===`-delimited block with a name, the Pascal source to parse, and the expected S-expression AST below a `---` separator:

```text
===
Integers
===

const
  a = 1234567890;

---

(root
  (declConsts (kConst)
    (declConst (identifier) (defaultValue (kEq) (literalNumber)))))
```

You don't have to hand-write the AST — after adding the source snippet, run `npx tree-sitter parse --debug <file>` or use `npx tree-sitter test -u` to auto-generate the expected output, then read it over to confirm it's actually correct before committing it. Add new cases to an existing file that matches the feature area (e.g. `literals.txt`, `generics-delphi.txt`) or start a new one if nothing fits.

To run the full test suite, which includes grammar validation and parsing example files, use:

```powershell
cd bindings/node
npm install
npm test
```

### Individual Test Commands

From the root of the repository:

- **Run corpus tests:** `npx tree-sitter test`
- **Parse example files:** `npx tree-sitter parse examples/*`
- **Test syntax highlighting:** `npx tree-sitter highlight <path_to_file>`

## Pre-built Binaries

Pre-built native libraries for all supported platforms are published with each [GitHub Release](https://github.com/jimmckeeth/tree-sitter-pascal/releases/latest):

| Platform             | File                                   |
| :------------------- | :------------------------------------- |
| Windows x86 (32-bit) | `tree-sitter-pascal-windows-x86.dll`   |
| Windows x64 (64-bit) | `tree-sitter-pascal-windows-x64.dll`   |
| Linux x64            | `tree-sitter-pascal-linux-x64.so`      |
| macOS Intel          | `tree-sitter-pascal-macos-x64.dylib`   |
| macOS Apple Silicon  | `tree-sitter-pascal-macos-arm64.dylib` |
| WebAssembly          | `tree-sitter-pascal.wasm`              |

The `tree-sitter.wasm` core runtime (required for WASM use) is available from the [tree-sitter releases](https://github.com/tree-sitter/tree-sitter/releases).

The committed `tree-sitter-pascal.wasm` at the repository root is the release asset copy. The npm package also ships a second copy from `bindings/node/tree-sitter-pascal.wasm` because `bindings/node/package.json` includes `*.wasm` in its published files list.

To refresh both copies before a release, run:

```powershell
pwsh -ExecutionPolicy Bypass -File scripts/update-wasm.ps1
```
