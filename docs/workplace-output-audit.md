# Workplace output audit

Repro: `data/workplace_wellbeing.csv`, 480 rows, Control/Training (240 each),
FIML, indirect effects, modification indices and intelligent reporting enabled.
The model definitions are in `tests/testthat/helper-workplace.R`.
Tests use lavaan 0.7-2 from `build/R4.5.0-arm64-macos` with R 4.4.3
(the bundled lavaan package is pure R). No fitted coefficient calculation was changed.

| Item | Reproduced | Cause and resolution | Files | Verification |
| --- | --- | --- | --- | --- |
| 1 | Partial | IE11 shift did not occur through the R entry point; totals retained bare IE references, and descriptions were initialized by row position. Build named descriptions with the definitions and rejoin by lhs after fitting. | R/syntax.R, R/estimate.R; workplace test/helper | Model B has 12 IEs and 8 TEs; check IE11 pathway and TE1 description, including group. |
| 2 | Yes | Constraint-count df was zero, so real group statistics were replaced with 0 and p=1. Show standard chi-square contributions with group names; leave df/p blank. Omit group rows for robust/scaled tests, whose corrections need not decompose. | R/estimate.R; workplace test | Model A contributions sum to user chi-square (tolerance 1e-7); no group df/p. |
| 3 | Yes | Squaring a negative Fisher lower bound made a positive lower limit above tiny R². Clip the correlation lower limit at zero before squaring; use each group's N for multigroup intervals. | R/estimate.R; workplace test | Model B Control wellbeing R²≈0.000025 previously had CI [0.00715,0.00892]; corrected lower bound is 0 and every interval contains its estimate. |
| 4 | Yes | Mediation narrative called a standardized value beta while quoting the unstandardized interval. Pair B with its CI and report beta separately. | R/intelligent_reporting.R; workplace test | Fit an indirect effect and match narrative B, CI endpoints and beta to the parameter table. |
| 5 | Yes | Defined lhs/rhs entered the model-variable list; removing absent columns without the corresponding display names shifted labels (including IE1). Select observed-variable operators, excluding definitions, constraints and latent names. | R/missing.R; workplace test | Model B missing-summary names exactly match six modeled variables plus condition; definition-only parameter excluded from unit fixture. |
| 6 | Yes (exports); no numbering shift | Exporter appended one covariance per group to a group-collapsed syntax preview. Deduplicate covariance statements before feeding all four exporters. Fitted syntax/parameter numbering is unaffected. | R/estimate.R; workplace test | All four Model B previews have unique lines; IE7–IE12 equal products of their fitted group-2 paths. |
