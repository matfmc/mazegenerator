param(
    [ValidateSet('method-rows', 'grid')]
    [string]$Layout = 'method-rows',
    [int]$Columns = 4,
    [double]$Hold = 4
)
$ErrorActionPreference = 'Stop'
$projectRoot = Split-Path -Parent $PSScriptRoot
$bundledPython = Join-Path $env:USERPROFILE '.cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe'
if (Test-Path -LiteralPath $bundledPython) {
    $collagePython = $bundledPython
} else {
    $collagePython = 'python'
}
& $collagePython (Join-Path $PSScriptRoot 'collage.py') --input (Join-Path $projectRoot 'mazes') --output (Join-Path $projectRoot 'collages') --layout $Layout --columns $Columns --hold $Hold
if ($LASTEXITCODE -ne 0) { throw 'Falha ao gerar colagem. Verifique Python e Pillow.' }
