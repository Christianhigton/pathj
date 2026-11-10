# Apple Silicon update metadata

The original fixed build reused version 1.1.7.9013 and omitted `build-time`.
The installed jamovi client compares module versions and build timestamps when
deciding whether to refresh a sideloaded module. It ignores `build-hash` in that
comparison, so replacing code without changing those markers can retain the
previous interface in a running session.

Version 1.1.7.9014 gives the update a distinct version. The Apple Silicon build
script also adds a UTC build timestamp, including when an older jamovi compiler
does not add one itself. No model calculations are changed.

Run `Rscript scripts/build_macos_arm64_jmo.R` from the repository root. It uses
the installed jamovi app and the existing arm64 dependency cache, works in an
isolated staging tree, and verifies the archive's version, timestamp, R ABI and
native architectures. Then smoke-test the finished archive using jamovi's R
with `scripts/smoke_module.R` and its base/jmv runtime libraries on the R library
path.

Manual installation check: sideload the 1.1.7.9014 arm64 file, quit jamovi fully,
reopen it, and check **SEM → pathj → Path Analysis**. The module list should show
the new version. A different installation error, incompatible jamovi version,
or an analysis-specific failure still needs its exact message to diagnose.
