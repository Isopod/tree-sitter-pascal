<#
.SYNOPSIS
    Rebuilds the committed WebAssembly artifact used for GitHub Releases and npm
    publishing.

.DESCRIPTION
    This script builds tree-sitter-pascal.wasm in a temporary workspace, then
    copies the result to both:
      1. the repository root, for release assets
      2. bindings/node/tree-sitter-pascal.wasm, for npm packaging

    Run this before tagging a release whenever the grammar or generated parser
    changes.
#>

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$RepoRoot = Split-Path $PSScriptRoot -Parent
$TempRoot = Join-Path $env:TEMP 'tree-sitter-pascal-wasm-update'
$RootWasm = Join-Path $RepoRoot 'tree-sitter-pascal.wasm'
$PackageWasm = Join-Path $RepoRoot 'bindings\node\tree-sitter-pascal.wasm'
$LocalTreeSitter = Join-Path $RepoRoot 'bindings\node\node_modules\.bin\tree-sitter.cmd'
$EmscriptenDir = Join-Path $RepoRoot 'emsdk\upstream\emscripten'
$EmsdkDir = Join-Path $RepoRoot 'emsdk'

function Copy-SourceTree {
    param(
        [string]$Source,
        [string]$Destination
    )

    if (-not (Test-Path $Source)) {
        throw "Missing required source path: $Source"
    }

    New-Item -ItemType Directory -Path $Destination -Force | Out-Null
    Get-ChildItem -Path $Source -Force | ForEach-Object {
        $target = Join-Path $Destination $_.Name
        if ($_.PSIsContainer) {
            Copy-Item -Path $_.FullName -Destination $target -Recurse -Force
        } else {
            Copy-Item -Path $_.FullName -Destination $target -Force
        }
    }
}

function Get-TreeSitterCommand {
    if (Test-Path $LocalTreeSitter) {
        return $LocalTreeSitter
    }

    $cmd = Get-Command tree-sitter -ErrorAction SilentlyContinue
    if ($null -ne $cmd) {
        return $cmd.Source
    }

    throw 'tree-sitter CLI not found. Run npm install in bindings/node first.'
}

function Invoke-TreeSitterBuild {
    param(
        [string]$WorkingDirectory
    )

    $treeSitter = Get-TreeSitterCommand
    $oldPath = $env:PATH
    try {
        $env:PATH = "$EmscriptenDir;$EmsdkDir;$env:PATH"
        Push-Location $WorkingDirectory
        & $treeSitter build --wasm
        if ($LASTEXITCODE -ne 0) {
            throw 'tree-sitter build --wasm failed.'
        }
    } finally {
        Pop-Location
        $env:PATH = $oldPath
    }
}

Remove-Item -Path $TempRoot -Recurse -Force -ErrorAction SilentlyContinue
New-Item -ItemType Directory -Path $TempRoot -Force | Out-Null

try {
    Copy-SourceTree -Source (Join-Path $RepoRoot 'src') -Destination (Join-Path $TempRoot 'src')
    Copy-Item -Path (Join-Path $RepoRoot 'grammar.js') -Destination $TempRoot -Force
    Copy-Item -Path (Join-Path $RepoRoot 'tree-sitter.json') -Destination $TempRoot -Force
    Copy-Item -Path (Join-Path $RepoRoot 'src\parser.c') -Destination (Join-Path $TempRoot 'parser.c') -Force

    Invoke-TreeSitterBuild -WorkingDirectory $TempRoot

    $builtWasm = Join-Path $TempRoot 'tree-sitter-pascal.wasm'
    if (-not (Test-Path $builtWasm)) {
        throw 'tree-sitter build completed but tree-sitter-pascal.wasm was not created.'
    }

    Copy-Item -Path $builtWasm -Destination $RootWasm -Force
    Copy-Item -Path $builtWasm -Destination $PackageWasm -Force

    Write-Host 'Updated tree-sitter-pascal.wasm in the repo root and bindings/node/' -ForegroundColor Green
} finally {
    Remove-Item -Path $TempRoot -Recurse -Force -ErrorAction SilentlyContinue
}
