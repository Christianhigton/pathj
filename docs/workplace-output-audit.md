# Workplace output audit

Repro: `data/workplace_wellbeing.csv`, 480 rows, Control/Training (240 each),
FIML, indirect effects, modification indices and intelligent reporting enabled.
The model definitions are in `tests/testthat/helper-workplace.R`.
Tests use lavaan 0.7-2 from `build/R4.5.0-arm64-macos` with R 4.4.3
(the bundled lavaan package is pure R). No fitted coefficient calculation was changed.

| Item | Reproduced | Cause and resolution | Files | Verification |
| --- | --- | --- | --- | --- |
| 1 | Partial | IE11 shift did not occur through the R entry point; totals retained bare IE references, and descriptions were initialized by row position. Build named descriptions with the definitions and rejoin by lhs after fitting. | R/syntax.R, R/estimate.R; workplace test/helper | Model B has 12 IEs and 8 TEs; check IE11 pathway and TE1 description, including group. |
