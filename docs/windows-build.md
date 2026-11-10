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

The `Build Windows jamovi module` GitHub Actions workflow builds on a native
Windows runner with R 4.5.0 and Rtools45, matching jamovi 2.7's R ABI. This
avoids relying on the unavailable `dl.jamovi.org` download host. The explicit
`PATHJ_MATCHED_R=true` CI mode requires exactly R 4.5.0; ordinary manual builds
continue to require jamovi's bundled R.

The workflow checks DLL architecture, loads the package from the finished
archive, runs the workplace example and exports both group diagrams. Its
artifact is named `pathj-windows`. These native-R checks do not replace a live
jamovi GUI installation check.
