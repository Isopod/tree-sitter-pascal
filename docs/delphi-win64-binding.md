# Consuming tree-sitter-pascal from Delphi (Win64)

These notes document the non-obvious work required to consume the
tree-sitter-pascal grammar from a Delphi 13.1 / Win64 application.

> **Provenance:** The DLL build toolchain work (sections 1 and 2) was first
> carried out against the wpostma grammar. Once the export shim approach was
> working there, the same build process was applied to this grammar without
> change, confirming that the issues and solutions are toolchain-level, not
> grammar-specific. Sections 3–6 are tree-sitter C API and Delphi interop
> facts that apply to any grammar. Section 7 is specific to this grammar's
> confirmed ABI version.
>
> *All of the below was confirmed against tree-sitter v0.24.6 runtime source,
> zig cc (0.16.0-dev), Delphi 13.1 (Athens), Win64.*

---

## 1. DLL build — zig linker rejects all DEF file approaches

The natural first approach for exporting symbols from a zig-built DLL on
Windows is a `.def` file. None of the standard forms work with the zig cc
toolchain:

- `/DEF:MDParser.def` → `unsupported linker arg`
- `-Wl,/DEF:MDParser.def` → `unsupported linker arg`
- `-Wl,--output-def,...` → `unsupported linker arg`
- `-Wl,--help` → `unsupported linker arg` (linker does not expose its own help)

The root cause is that zig's linker backend operates in LLD COFF mode, which
does not implement the MSVC DEF file flags. This is not a configuration issue
or a zig version issue — the flags simply do not exist in LLD's COFF driver.
Upgrading zig will not change this.

The working solution is a thin C wrapper (`mdparser_exports.c`) that
re-exports every needed symbol explicitly via `__declspec(dllexport)`, using
an `mdp_`-prefixed name for each. No linker flags are required. The build
command is:

```bat
zig cc -target x86_64-windows -shared ^
  mdparser_exports.c tree-sitter\lib\src\lib.c parser.c ^
  -Itree-sitter\lib\include -Itree-sitter\lib\src -Isrc ^
  -o MDParser.dll
```

The `mdp_` prefix also prevents symbol collisions at the Windows loader level
if the host process already loads a different tree-sitter DLL (for example,
one embedded in a Delphi IDE plugin).

---

## 2. Static `external` declarations vs. `LoadLibrary`

Delphi supports two ways to bind to a DLL: static `external` declarations
(resolved at load time by the Windows loader) and explicit
`LoadLibrary` / `GetProcAddress` calls at runtime.

The initial attempt using static `external` declarations failed because the
tree-sitter runtime functions were compiled into the DLL without being
exported — the `TREE_SITTER_API` decoration in `api.h` did not produce
exports under this zig build configuration. Switching temporarily to
`LoadLibrary` / `GetProcAddress` works regardless of export visibility.

However, `LoadLibrary` / `GetProcAddress` has a cost: in Delphi 13.1,
`delayhlp.obj` (the Delphi runtime support object for delayed-load DLLs) is
not available. Delayed loading is therefore not an option, and explicit
`LoadLibrary` calls require more boilerplate.

Once the `mdparser_exports.c` shim (section 1) solves the export problem,
static `external` declarations are cleaner and require no Delphi-side
lifecycle management:

```pascal
function mdp_ts_parser_new: TSParserHandle;
    cdecl; external MDParserDLL;
```

This is the recommended approach once exports are correctly in place via the
shim. The `LoadLibrary` path remains viable as a fallback if the shim
approach ever becomes impractical.

---

## 3. Win64 struct layout — TSNode and TSTreeCursor

Both structs exceed 8 bytes and are therefore returned via a hidden
caller-allocated pointer under the Microsoft x64 ABI. Delphi's 64-bit
compiler handles this correctly for record return types — no manual
intervention is needed — but the field layout must match the C structs
exactly. Any mismatch produces silent stack corruption with no diagnostic.

**TSNode — 32 bytes:**

```pascal
TSNode = record
  Context : array[0..3] of UInt32;  // uint32_t context[4]  — 16 bytes
  Id      : Pointer;                 // const void *id       —  8 bytes
  Tree    : TSTreeHandle;            // const TSTree *tree   —  8 bytes
end;
```

**TSTreeCursor — 28 bytes natural, padded to 32:**

```pascal
TSTreeCursor = record
  Tree    : Pointer;                 // const void *tree     —  8 bytes
  Id      : Pointer;                 // const void *id       —  8 bytes
  Context : array[0..2] of UInt32;  // uint32_t context[3]  — 12 bytes
  Pad     : UInt32;                  // ABI trailing padding —  4 bytes
end;
```

The `Pad` field is not present in the C header — it is an ABI consequence of
the Microsoft x64 requirement that structs returned via hidden pointer be
padded to a multiple of 8 bytes. A developer reading `tree_cursor.h`
alongside this Pascal record will see an apparent mismatch and may be tempted
to remove `Pad`. Do not: the padding is required for correct stack alignment
and its absence produces the same silent corruption as any other layout error.

Cursor mutation functions (`goto_first_child` etc.) take a pointer to
`TSTreeCursor`, matching the C convention of `TSTreeCursor *`. Callers pass a
pointer to a local record variable.

---

## 4. Delphi type mappings

| C type | Delphi type | Notes |
|---|---|---|
| `_Bool` / `stdbool.h bool` | `ByteBool` | 1-byte boolean. `WordBool` or `LongBool` would misread the return register on Win64. |
| `const char *` (node type, field name) | `PAnsiChar` | Grammar names are ASCII. The returned pointer is owned by the grammar — do not free. |
| `uint32_t` | `UInt32` | Byte offsets, child counts, ABI version. |
| Opaque handles | `type Pointer` | Declared as distinct types (`TSParserHandle`, `TSTreeHandle`, `TSLanguageHandle`) to prevent silent misuse across the call surface. |

On Win64, Delphi's `PChar` is `PWideChar`. Every string return from
tree-sitter must be declared `PAnsiChar` or silent data corruption and AVs
result.

---

## 5. `ts_node_is_error` does not exist

There is no such function in the tree-sitter C API. To test whether a
specific node is an ERROR node, compare `ts_node_type(node)` against the
string `'ERROR'`. This is distinct from `ts_node_has_error`, which tests
whether the node *or any descendant* contains an error — a subtree predicate,
not a node identity check.

A walker that tracks ERROR ancestry needs both: `NodeType = 'ERROR'` to
identify entering an error subtree, and `ts_node_has_error` for quick subtree
pruning decisions.

---

## 6. Source buffer ownership

`ts_parser_parse_string` does **not** copy or retain the source string. The
tree stores only byte-offset metadata. To recover the text of a node you must
hold the original UTF-8 buffer alive and slice it yourself using
`ts_node_start_byte` / `ts_node_end_byte`.

In a Delphi service layer this means the parse result object must hold a
`TBytes` (UTF-8) buffer for exactly as long as any node derived from that
parse is live. `ICSTNode.Text` slices that buffer and decodes via
`TEncoding.UTF8.GetString` on each call.

`ts_node_start_point` / `ts_node_end_point` are not strictly necessary for
structural analysis: line/column recovery from byte offsets requires a
separate newline-index scan. The intended approach is to build a newline
index once at parse time — a single pass over the UTF-8 buffer recording the
byte offset of each `\n` — and store it alongside the buffer in the parse
result. Line and column for any node then become two binary searches: one to
locate the line, one to compute the column offset.

---

## 7. ABI version

This grammar compiles at ABI version **14** (tree-sitter CLI targeting
ABI 14). The tree-sitter 0.24 runtime declares a compatible range of
`[13..15]`. Version 14 falls cleanly within that range.

Validate this at startup before any parse operation:

```pascal
LVersion := mdp_ts_language_version(mdp_tree_sitter_pascal);
if (LVersion < TSMinCompatibleVersion) or (LVersion > TSLanguageVersion) then
  raise EParserServiceError.CreateFmt(
      'MDParser.dll grammar ABI version %u is outside the ' +
      'compatible range [%u..%u].',
      [LVersion, TSMinCompatibleVersion, TSLanguageVersion]);
```

---

## 8. `TREE_SITTER_API` does not produce exports under zig cc

This is the single most disorienting discovery in the build process.

The `TREE_SITTER_API` macro in `tree-sitter/lib/include/tree_sitter/api.h` is
defined as `__declspec(dllexport)` when building a shared library on Windows.
The expectation from reading the header is that all public tree-sitter
functions will appear as exports in the resulting DLL — and that static
`external` declarations will resolve against them.

Under zig cc they do not. The functions are compiled and linked into the DLL,
but they are not placed in the export directory. Attempting to bind with
static `external` produces load-time failures; the symbols are not visible to
the Windows loader.

The cause is that zig cc, operating in Clang-over-LLD mode, does not honour
`__declspec(dllexport)` on functions compiled into a shared library in all
configurations — in particular when the source files were not compiled with
`-DBUILDING_DLL` or equivalent guards that the zig build system does not
inject by default.

The `mdparser_exports.c` shim (section 1) is the fix. It compiles as a
separate translation unit that explicitly declares each symbol
`__declspec(dllexport)`, which zig cc does honour at that boundary. This is
the step that will trip up every developer who approaches the build from first
principles.
