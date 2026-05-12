# tree-sitter-pascal

Grammar for Pascal and its dialects Delphi and Freepascal.

## Supported language features
- Classes, records, interfaces, class helpers
- Nested declarations
- Variant records
- Generics (Delphi- & FPC flavored)
- Anonymous procedures & functions
- Inline assember (but no highlighting)
- Extended RTTI attributes
- FPC PasCocoa extensions

## Tree-sitter features:
- Syntax highlighting
- Scopes


## Warren's fork of tree-sitter pascal is compatible with ML Code Index tools like REPOWISE
## warren's repowise fork has unit tests to test pascal unit parsing, for AI/ML/MCP integration.
    # from warrens repowise fork
    # https://github.com/wpostma/repowise
    uv run --with-editable .\thirdparty\tree-sitter-pascal pytest test_parser.py -q

## Screenshots

(using nvim-treesitter)

<a href=".doc/scr1.png"><img src=".doc/scr1.png" style="width: 22%; height: 22%"></a>
<a href=".doc/scr2.png"><img src=".doc/scr2.png" style="width: 22%; height: 22%"></a>
<a href=".doc/scr3.png"><img src=".doc/scr3.png" style="width: 22%; height: 22%"></a>
<a href=".doc/scr4.png"><img src=".doc/scr4.png" style="width: 22%; height: 22%"></a>
