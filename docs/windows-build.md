# Build PATHj for Windows

Build on 64-bit Windows with the R bundled with the target jamovi installation.
The macOS `.jmo` files contain native macOS libraries and cannot be used as Windows builds.

Install jamovi, Node.js (available on PATH), and Rtools matching jamovi's bundled
R version if dependencies need compilation. Start PowerShell in this repository:

```powershell
$env:JAMOVI_HOME = 'C:\Program Files\jamovi 2.7.0.0'
$rscript = Join-Path $env:JAMOVI_HOME 'Frameworks\R\bin\Rscript.exe'
# Some installations place Rscript.exe in bin\x64 instead.
if (!(Test-Path $rscript)) {
    $rscript = Join-Path $env:JAMOVI_HOME 'Frameworks\R\bin\x64\Rscript.exe'
}
$env:R_LIBS_USER = Join-Path $env:LOCALAPPDATA 'pathj-build-library'
New-Item -ItemType Directory -Force $env:R_LIBS_USER | Out-Null
& $rscript -e 'install.packages("jmvtools", repos=c("https://repo.jamovi.org", "https://cloud.r-project.org"))'
if ($LASTEXITCODE -ne 0) { throw 'Installing jmvtools failed' }
& $rscript scripts/build_windows_jmo.R
if ($LASTEXITCODE -ne 0) { throw 'Building PATHj failed' }
```

Adjust `JAMOVI_HOME` to your installed version. The script prepares dependencies,
builds `pathj_<version>_windows.jmo`, and checks that included native libraries
are Windows x64 DLLs. Install that file using jamovi's module menu, **Sideload**.
Open PATHj and run an analysis to check it in the target Windows installation;
archive validation alone does not confirm runtime compatibility.

The `Build Windows jamovi module` GitHub Actions workflow performs the same
build on a native Windows runner using the portable Windows jamovi 2.7.26
distribution. It checks DLL architecture, then loads the package from the
finished archive, runs the workplace example and exports both group diagrams.
Its downloadable artifact is named `pathj-windows`.
