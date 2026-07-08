<#
.SYNOPSIS
    Cleans up build artifacts and temporary files from the tree-sitter-pascal 
    repository.

.DESCRIPTION
    Deletes:
    1. Output libraries (Libs/)
    2. Grammar cache (GrammarsCache/)
    3. Build artifacts (parser.exp, parser.lib, parser.obj, tree-sitter-pascal.wasm)
    4. Delphi temporary folders (__history, __recovery, Win32, Win64, etc.)
#>

Set-StrictMode -Version Latest
$ErrorActionPreference = 'SilentlyContinue'

$RepoRoot = Split-Path $PSScriptRoot -Parent

Write-Host "=== Cleaning Build Artifacts ===" -ForegroundColor Cyan

function Remove-Dir($Path) {
    if (Test-Path $Path) {
        Write-Host "  Removing $Path ..." -ForegroundColor Gray
        Remove-Item $Path -Recurse -Force
    }
}

function Remove-File($Path) {
    if (Test-Path $Path) {
        Write-Host "  Deleting $(Split-Path $Path -Leaf) ..." -ForegroundColor Gray
        Remove-Item $Path -Force
    }
}

# 1. Libs and GrammarsCache
Remove-Dir (Join-Path $RepoRoot "Libs")
Remove-Dir (Join-Path $RepoRoot "GrammarsCache")

# 2. Root artifacts
Remove-File (Join-Path $RepoRoot "parser.exp")
Remove-File (Join-Path $RepoRoot "parser.lib")
Remove-File (Join-Path $RepoRoot "parser.obj")
Remove-File (Join-Path $RepoRoot "tree-sitter-pascal.wasm")

# 3. Delphi Artifacts in examples/
$examplesDir = Join-Path $RepoRoot "examples"
if (Test-Path $examplesDir) {
    Remove-Dir (Join-Path $examplesDir "__history")
    Remove-Dir (Join-Path $examplesDir "__recovery")
    Remove-Dir (Join-Path $examplesDir "Win32")
    Remove-Dir (Join-Path $examplesDir "Win64")
    Remove-Dir (Join-Path $examplesDir "Linux64")
}

# 4. node_modules (optional: uncomment if you want full clean)
# Remove-Dir (Join-Path $RepoRoot "node_modules")

Write-Host "Done!" -ForegroundColor Green
