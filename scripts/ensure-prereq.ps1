<#
.SYNOPSIS
    Checks and optionally installs prerequisites for building the 
    tree-sitter-pascal libraries on Windows.

.DESCRIPTION
    Checks for:
    1. Git
    2. Zig (for native builds)
    3. Node.js & npm
    4. Emscripten (for WASM builds)
    5. tree-sitter-cli (via npm)

    If Emscripten is missing, it offers to clone and set up emsdk in the 
    repository root and activate it for the current session.
#>

param(
    [switch]$AutoInstall = $false
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Continue'

$RepoRoot = Split-Path $PSScriptRoot -Parent
$EmsdkDir = Join-Path $RepoRoot "emsdk"

function Check-Command($Name) {
    $cmd = Get-Command $Name -ErrorAction SilentlyContinue
    if ($null -eq $cmd) {
        return $false
    }
    return $true
}

function Write-Step($Name, $Status, $Color = "Gray") {
    $s = if ($Status) { "[+]" } else { "[-]" }
    $c = if ($Status) { "Green" } else { "Red" }
    Write-Host "$s $Name" -ForegroundColor $c
}

Write-Host "=== Checking Prerequisites ===" -ForegroundColor Cyan

$allOk = $true

# 1. Git
$hasGit = Check-Command "git"
Write-Step "Git" $hasGit
if (-not $hasGit) {
    Write-Host "  Please install Git from https://git-scm.com/" -ForegroundColor Yellow
    $allOk = $false
}

# 2. Zig
$hasZig = Check-Command "zig"
Write-Step "Zig" $hasZig
if (-not $hasZig) {
    Write-Host "  Please install Zig (0.13.0 recommended) from https://ziglang.org/download/" -ForegroundColor Yellow
    Write-Host "  Or use: winget install zig.zig" -ForegroundColor Gray
    $allOk = $false
}

# 3. Node.js & npm
$hasNode = Check-Command "node"
$hasNpm = Check-Command "npm"
Write-Step "Node.js" $hasNode
Write-Step "npm" $hasNpm
if (-not $hasNode) {
    Write-Host "  Please install Node.js from https://nodejs.org/" -ForegroundColor Yellow
    $allOk = $false
}

# 4. Emscripten (emsdk)
$hasEmcc = Check-Command "emcc"

if (-not $hasEmcc) {
    $envScript = Join-Path $EmsdkDir "emsdk_env.ps1"
    if (Test-Path $envScript) {
        Write-Host "  emsdk found in $EmsdkDir, activating..." -ForegroundColor Yellow
        . $envScript
        $hasEmcc = Check-Command "emcc"
    } else {
        $install = $false
        if ($AutoInstall) {
            $install = $true
        } else {
            $choice = Read-Host "  Emscripten is missing. Would you like to clone and install emsdk into the repo? (y/N)"
            if ($choice -eq 'y') { $install = $true }
        }

        if ($install) {
            Write-Host "  Cloning emsdk..." -ForegroundColor Gray
            git clone https://github.com/emscripten-core/emsdk.git $EmsdkDir
            if ($LASTEXITCODE -eq 0) {
                Push-Location $EmsdkDir
                Write-Host "  Installing latest emscripten..." -ForegroundColor Gray
                .\emsdk.bat install latest
                Write-Host "  Activating latest emscripten..." -ForegroundColor Gray
                .\emsdk.bat activate latest
                Pop-Location
                
                if (Test-Path $envScript) {
                    Write-Host "  Applying environment to current session..." -ForegroundColor Gray
                    . $envScript
                }
                $hasEmcc = Check-Command "emcc"
            } else {
                Write-Host "  Failed to clone emsdk." -ForegroundColor Red
                $allOk = $false
            }
        }
    }
}

Write-Step "Emscripten (emcc)" $hasEmcc
if (-not $hasEmcc) {
    Write-Host "  WASM builds will fail without Emscripten." -ForegroundColor Yellow
    $allOk = $false
}

# 5. tree-sitter-cli
Write-Host "Checking tree-sitter-cli..." -ForegroundColor Gray
& npx tree-sitter --version 2>&1 | Out-Null
if ($LASTEXITCODE -eq 0) {
    Write-Step "tree-sitter-cli" $true
} else {
    Write-Step "tree-sitter-cli" $false
    Write-Host "  tree-sitter-cli not found via npx. Run 'npm install' in bindings/node/." -ForegroundColor Red
    $allOk = $false
}

Write-Host ""
if ($allOk) {
    Write-Host "All prerequisites are met. You are ready to build!" -ForegroundColor Green
    Write-Host "Note: build.ps1 will automatically activate emsdk if it is in the root." -ForegroundColor Gray
} else {
    Write-Host "Some prerequisites are missing or could not be activated." -ForegroundColor Red
    Write-Host "If you just installed emsdk, you may need to run: . .\scripts\ensure-prereq.ps1 (dot-sourced)" -ForegroundColor Yellow
}
