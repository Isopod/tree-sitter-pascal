# Tree-Sitter-Pascal (and Delphi)

[![CI and Release](https://github.com/jimmckeeth/tree-sitter-pascal/actions/workflows/ci.yml/badge.svg)](https://github.com/jimmckeeth/tree-sitter-pascal/actions/workflows/ci.yml)
[![Python Wheels](https://github.com/jimmckeeth/tree-sitter-pascal/actions/workflows/python-wheels.yml/badge.svg)](https://github.com/jimmckeeth/tree-sitter-pascal/actions/workflows/python-wheels.yml)

[<img src="https://raw.githubusercontent.com/jimmckeeth/tree-sitter-pascal/main/docs/Tree-sitter-Delphi-512.avif" style="zoom:50%; float:right;" alt="tree sitter Delphi logo - a hoplite/spartan warrior sitting in a tree next to a treehouse"/>](https://github.com/jimmckeeth/Tree-sitter-Delphi)  
A [Tree-sitter](https://github.com/tree-sitter/tree-sitter) grammar supporting Pascal with focus on [Delphi's Object Pascal](https://delphi.embarcadero.com/), updated and focused on the latest language features. Support of other Pascal dialects like [Free Pascal](https://www.freepascal.org/) and [Oxygene](https://www.remobjects.com/elements/oxygene/) are also planned, and **[pull requests are welcome](https://github.com/jimmckeeth/tree-sitter-pascal/blob/main/CONTRIBUTING.md)**. 

See also [Delphi-tree-sitter bindings](https://github.com/jimmckeeth/delphi-tree-sitter) for consuming tree-sitter grammars from Delphi.

## Contents

- [What is Tree-sitter?](#what-is-tree-sitter)
- [Supported language features](#supported-language-features)
- [Tree-sitter features](#tree-sitter-features)
- [Python Package](#python-package)
- [Pre-built Binaries](#pre-built-binaries)
- [Contributing](https://github.com/jimmckeeth/tree-sitter-pascal/blob/main/CONTRIBUTING.md)
- [Contributors](#contributors)
- [License](#license)

## What is Tree-sitter?

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/) is a incremental parsing system for programming tools. It is currently one of the most popular systems and grammars across all programming languages. It is useful for [syntax highlighting](https://tree-sitter.github.io/tree-sitter/3-syntax-highlighting.html), [code navigation](https://tree-sitter.github.io/tree-sitter/4-code-navigation.html), and most recently for powering [AST-based semantic code search](https://github.com/cocoindex-io/cocoindex-code) (my next project).

## Supported language features

- Classes, records, interfaces, class helpers
- Nested declarations
- Variant records
- Generics (Delphi & FPC flavored)
- Anonymous procedures & functions
- Inline assembler (but no highlighting)
- Extended RTTI attributes
- FPC PasCocoa extensions
- Ternary `if` Expressions (Delphi 13)
- Extended Logical Operators (`is not` and `not in`) (Delphi 13)
- Custom Managed Records (Delphi 10.4)
- Multi-line String Literals (Delphi 12)
- Numeric Separators (Delphi 12)
- Binary Literals (Delphi 12)
- Inline Variable & Constant Declarations (Delphi 10.3 / 12)
- Case `otherwise` Keyword (Free Pascal / ISO Pascal)
- Nested Preprocessor Directives

## Tree-sitter features

- Syntax highlighting
- Scopes

## Python Package

The Python binding is published as `tree-sitter-pascal` and imported as `tree_sitter_pascal`.

```bash
python -m pip install tree-sitter-pascal
```

Python wheels are built with `cibuildwheel` and published through PyPI Trusted Publishing. Use the manual TestPyPI workflow first when validating a new release path. Production PyPI publishing runs only when a GitHub Release is published for a non-hyphenated release tag such as `v0.11.0`; auto-generated tags like `v0.11.0-abc1234` are ignored.

Before the first upload, configure PyPI trusted publishers for the `publish-testpypi.yml` and `publish-pypi.yml` workflows, using the `testpypi` and `pypi` GitHub environments respectively.

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

## Contributors

This is ultimately based on an original implementation by [Isopod](https://github.com/Isopod/tree-sitter-pascal), with updates by [Warren Postma](https://github.com/wpostma/tree-sitter-pascal). See our [contributing guide](https://github.com/jimmckeeth/tree-sitter-pascal/blob/main/CONTRIBUTING.md).

## License

I've migrated my updates to [AGPL](https://github.com/jimmckeeth/tree-sitter-pascal/blob/main/LICENSE.md). I'm a big fan of open source. Unfortunately, I've seen too many companies take advantage of permissive licenses and turn an open source project into a closed source one. This is why I prefer AGPL. At the same time, I'm also a big fan of commercial software, which might seem incompatible. That is why I'm happy to provide a dual license. I'll set up a pricing structure later, but I just want to announce it is an option. Let me know if you are interested.

[![Object Pascal](https://raw.githubusercontent.com/jimmckeeth/tree-sitter-pascal/main/docs/object-pascal.webp)](https://objectpascal.foundation/)
