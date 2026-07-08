<#
.SYNOPSIS
    Builds tree-sitter and grammar shared libraries for all Delphi-supported
    platforms using the Zig cross-compiler, and WASM using tree-sitter-cli.
#>
param(
    [ValidateSet('Win32','Win64','Linux64','macOS-x64','macOS-arm64','Android','Android64','iOSDevice64','WASM')]
    [string[]]$Platforms,
    [switch]$Clean,
    [switch]$Grammars,
    [string]$GrammarsCache = ''
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$TsPascalDir   = Split-Path $PSScriptRoot -Parent
$RepoRoot      = Split-Path $TsPascalDir -Parent
$OutRoot       = Join-Path $TsPascalDir 'Libs'
$TsCoreDir     = Join-Path $TsPascalDir 'tree-sitter'
$TsCoreSrc     = Join-Path $TsCoreDir 'lib\src\lib.c'
$TsCoreInclude = Join-Path $TsCoreDir 'lib\include'
$TsCoreSrcDir  = Join-Path $TsCoreDir 'lib\src'
$PascalSrc     = Join-Path $TsPascalDir 'src\parser.c'
$PascalInclude = Join-Path $TsPascalDir 'src'

if ($GrammarsCache -eq '') {
    $GrammarsCache = Join-Path $TsPascalDir 'GrammarsCache'
}

function Get-TreeSitterCmd {
    $localTs = Join-Path $TsPascalDir "bindings/node/node_modules/.bin/tree-sitter"
    if (Test-Path $localTs) { return $localTs }
    $cmd = Get-Command tree-sitter -ErrorAction SilentlyContinue
    if ($null -ne $cmd) { return "tree-sitter" }
    return "npx tree-sitter"
}

$TS_CMD = Get-TreeSitterCmd
Write-Host "Using tree-sitter command: $TS_CMD" -ForegroundColor Gray

function Activate-Emsdk {
    $envScript = Join-Path $TsPascalDir "emsdk\emsdk_env.ps1"
    if (Test-Path $envScript) {
        Write-Host "  Activating emsdk environment..." -ForegroundColor Gray
        . $envScript
    }
}

if (-not (Test-Path $TsCoreSrc)) {
    Write-Error "tree-sitter submodule not found."
}
if (-not (Test-Path $PascalSrc)) {
    Write-Error "tree-sitter-pascal source not found."
}

$AllPlatforms = [ordered]@{
    'Win32'       = @{ Target = 'x86-windows-gnu';    Core = 'tree-sitter.dll';         Pascal = 'tree-sitter-pascal.dll'     }
    'Win64'       = @{ Target = 'x86_64-windows-gnu'; Core = 'tree-sitter.dll';         Pascal = 'tree-sitter-pascal.dll'     }
    'Linux64'     = @{ Target = 'x86_64-linux-gnu';   Core = 'libtree-sitter.so';       Pascal = 'libtree-sitter-pascal.so'   }
    'macOS-x64'   = @{ Target = 'x86_64-macos-none';  Core = 'libtree-sitter.dylib';    Pascal = 'libtree-sitter-pascal.dylib'}
    'macOS-arm64' = @{ Target = 'aarch64-macos-none'; Core = 'libtree-sitter.dylib';    Pascal = 'libtree-sitter-pascal.dylib'}
    'Android'     = @{ Target = 'arm-linux-musleabi'; Core = 'libtree-sitter.so';       Pascal = 'libtree-sitter-pascal.so'   }
    'Android64'   = @{ Target = 'aarch64-linux-musl'; Core = 'libtree-sitter.so';       Pascal = 'libtree-sitter-pascal.so'   }
    'iOSDevice64' = @{ Target = 'aarch64-ios-none';   Core = 'libtree-sitter.dylib';    Pascal = 'libtree-sitter-pascal.dylib'}
    'WASM'        = @{ Target = 'wasm32-unknown-emscripten'; Core = 'tree-sitter.wasm'; Pascal = 'tree-sitter-pascal.wasm' }
}

$ExtraGrammars = [ordered]@{
    'c'          = @{ Repo = 'tree-sitter/tree-sitter-c';          SrcSubDir = 'src'            }
    'python'     = @{ Repo = 'tree-sitter/tree-sitter-python';     SrcSubDir = 'src'            }
    'javascript' = @{ Repo = 'tree-sitter/tree-sitter-javascript'; SrcSubDir = 'src'            }
    'json'       = @{ Repo = 'tree-sitter/tree-sitter-json';       SrcSubDir = 'src'            }
    'typescript' = @{ Repo = 'tree-sitter/tree-sitter-typescript'; SrcSubDir = 'typescript/src' }
}

if (-not $Platforms) {
    $Platforms = $AllPlatforms.Keys | Where-Object { $_ -ne 'iOSDevice64' }
}

if ($Clean) {
    foreach ($key in $AllPlatforms.Keys) {
        $dir = Join-Path $OutRoot $key
        if (Test-Path $dir) { Remove-Item $dir -Recurse -Force }
    }
}

function Get-GrammarLibName([string]$Lang, [hashtable]$PlatInfo) {
    $core = $PlatInfo.Core
    if ($core -like '*.dll')   { return "tree-sitter-$Lang.dll"          }
    if ($core -like '*.dylib') { return "libtree-sitter-$Lang.dylib"     }
    if ($core -like '*.wasm')  { return "tree-sitter-$Lang.wasm"          }
    return "libtree-sitter-$Lang.so"
}

function Ensure-GrammarRepo([string]$Lang, [string]$Repo) {
    $dest = Join-Path $GrammarsCache $Lang
    if (Test-Path (Join-Path $dest '.git')) { return $true }
    & git clone --depth 1 "https://github.com/$Repo.git" $dest 2>&1
    return ($LASTEXITCODE -eq 0)
}

function Build-Grammar([string]$Lang, [string]$SrcSubDir, [string]$OutDir, [hashtable]$PlatInfo) {
    $repoDir = Join-Path $GrammarsCache $Lang
    $srcDir  = Join-Path $repoDir (($SrcSubDir -replace '/', [System.IO.Path]::DirectorySeparatorChar))
    $parser  = Join-Path $srcDir 'parser.c'
    if (-not (Test-Path $parser)) {
        Write-Warning "    No parser.c at $srcDir"
        return $false
    }

    $outFile = Get-GrammarLibName $Lang $PlatInfo
    $outPath = Join-Path $OutDir $outFile

    if ($PlatInfo.Target -eq 'wasm32-unknown-emscripten') {
        Activate-Emsdk
        Push-Location $repoDir
        if ($TS_CMD -eq "npx tree-sitter") { & npx tree-sitter build --wasm 2>&1 }
        else { & $TS_CMD build --wasm 2>&1 }
        $wasmFile = "tree-sitter-$Lang.wasm"
        if (Test-Path $wasmFile) {
            Move-Item $wasmFile $outPath -Force
            Pop-Location
            return $true
        }
        Pop-Location
        return $false
    }

    $sources = New-Object System.Collections.Generic.List[string]
    $sources.Add($parser)
    $scannerC  = Join-Path $srcDir 'scanner.c'
    $scannerCC = Join-Path $srcDir 'scanner.cc'
    if (Test-Path $scannerC)  { $sources.Add($scannerC) }
    if (Test-Path $scannerCC) { $sources.Add($scannerCC) }

    $args = @('cc', '-shared', '-o', $outPath) + $sources.ToArray() + @("-I$srcDir", "-I$TsCoreInclude", '-target', $PlatInfo.Target, '-O2')
    & zig @args 2>&1
    return ($LASTEXITCODE -eq 0)
}

function Deploy-Libs([string]$Plat, [string]$SrcDir) {
    $platMap = @{ 'Win32' = 'Win32'; 'Win64' = 'Win64' }
    if (-not $platMap.ContainsKey($Plat)) { return }
    $dp = $platMap[$Plat]
    $ext = switch -Wildcard ($AllPlatforms[$Plat].Core) {
        '*.dll'   { '*.dll'   }
        '*.dylib' { '*.dylib' }
        '*.wasm'  { '*.wasm'  }
        default   { '*.so'    }
    }
    $libs = Get-ChildItem $SrcDir -Filter $ext -ErrorAction SilentlyContinue
    if ($null -eq $libs) { return }
    $targets = @(
        "$RepoRoot\Examples\bin\$dp\Debug",
        "$RepoRoot\Examples\bin\$dp\Release",
        "$RepoRoot\Tests\$dp\Debug",
        "$RepoRoot\Tests\$dp\Release",
        "$TsPascalDir\examples\bin\$dp\Debug",
        "$TsPascalDir\examples\bin\$dp\Release"
    )
    foreach ($dir in $targets) {
        if (-not (Test-Path (Split-Path $dir -Parent))) { continue }
        if (-not (Test-Path $dir)) { New-Item -ItemType Directory -Path $dir -Force | Out-Null }
        foreach ($lib in $libs) { Copy-Item $lib.FullName (Join-Path $dir $lib.Name) -Force }
    }
}

$failed = New-Object System.Collections.Generic.List[string]

foreach ($plat in $Platforms) {
    $info   = $AllPlatforms[$plat]
    $outDir = Join-Path $OutRoot $plat
    if (-not (Test-Path $outDir)) { New-Item -ItemType Directory -Path $outDir -Force | Out-Null }

    Write-Host "=== Building $plat ($($info.Target)) ===" -ForegroundColor Cyan

    if ($plat -eq 'WASM') {
        Activate-Emsdk
        if ($TS_CMD -eq "npx tree-sitter") { & npx tree-sitter build --wasm 2>&1 }
        else { & $TS_CMD build --wasm 2>&1 }
        $wasmFile = "tree-sitter-pascal.wasm"
        if (-not (Test-Path $wasmFile)) {
            $foundWasm = Get-ChildItem "*.wasm" | Select-Object -First 1
            if ($null -ne $foundWasm) { $wasmFile = $foundWasm.Name }
        }
        if (Test-Path $wasmFile) { 
            Move-Item $wasmFile (Join-Path $outDir "tree-sitter-pascal.wasm") -Force 
            Write-Host "  OK" -ForegroundColor Green
        }
        else { 
            Write-Host "  FAILED: pascal (wasm) - No WASM file generated." -ForegroundColor Red
            $failed.Add("$plat/pascal"); continue 
        }
    } else {
        $coreOut = Join-Path $outDir $info.Core
        & zig cc -shared -o $coreOut $TsCoreSrc "-I$TsCoreInclude" "-I$TsCoreSrcDir" -target $info.Target -O2 2>&1
        if ($LASTEXITCODE -ne 0) { $failed.Add("$plat/core"); continue }
        $pascalOut = Join-Path $outDir $info.Pascal
        & zig cc -shared -o $pascalOut $PascalSrc "-I$PascalInclude" -target $info.Target -O2 2>&1
        if ($LASTEXITCODE -ne 0) { $failed.Add("$plat/pascal"); continue }
        Write-Host "  OK" -ForegroundColor Green
    }

    if ($Grammars) {
        if (-not (Test-Path $GrammarsCache)) { New-Item -ItemType Directory -Path $GrammarsCache -Force | Out-Null }
        foreach ($lang in $ExtraGrammars.Keys) {
            $g = $ExtraGrammars[$lang]
            $success = Ensure-GrammarRepo $lang $g.Repo
            if ($success) {
                $success = Build-Grammar $lang $g.SrcSubDir $outDir $info
            }
            if (-not $success) { $failed.Add("$plat/$lang") }
        }
    }
    Deploy-Libs $plat $outDir
}

if ($failed.Count -gt 0) {
    Write-Host "Failed builds: $($failed -join ', ')" -ForegroundColor Red
    exit 1
} else {
    Write-Host "All builds completed successfully." -ForegroundColor Green
    exit 0
}
