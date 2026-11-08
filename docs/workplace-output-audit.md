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
| 7 | No | Current decomposition already uses NA for absent direct paths and guards opposite signs, unstable ratios and near-zero totals. No calculation changed. | workplace test | Model A has blank direct and percent-mediated values; opposite-sign/near-zero unit cases return NA; ordinary case remains 40%. |
| 8 | Yes (narrative/table); NaN is renderer-dependent | Single-model analysis called compare_models and displayed a comparison with a logical winner in a text column. Omit the selection section/table in this one-model UI; the multi-fit comparison API remains available. | R/estimate.R, R/pathj.b.R; workplace test | Model A has no Model Selection narrative or comparison rows. |
| 9 | Yes | Path narratives ignored group IDs; insights selected one global strongest path without identifying its group. Label each path and each group's strongest association; expose Group in the insights table. | R/intelligent_reporting.R, R/estimate.R, R/pathj.h.R, jamovi/pathj.r.yaml; workplace test | Direct Effects and Model Insights contain Control and Training; insights rows carry group labels. |
| 10 | Yes | Screen included paths and derived effects but inferred evidential value. Retain a descriptive screen, explicitly note dependence/derived effects, and label the denominator of the last column. | R/intelligent_reporting.R, R/pathj.h.R, jamovi/pathj.r.yaml; workplace test | Model A p-curve text contains the dependence caveat and makes no evidential-value claim. |
| 11 | Yes | Group columns were beta but difference was B; logical significance was passed to a text column. Add both group Bs, label Difference (B), and serialize significance explicitly from p. | R/intelligent_reporting.R, R/estimate.R, R/pathj.h.R, jamovi/pathj.r.yaml; workplace test | Difference equals Group 1 B minus Group 2 B; TRUE/FALSE matches p<.05. |
| 12 | Yes | jmvcore translation treats square brackets in lavaan warnings as markup and discards text from the first bracket onward, including variable names. Convert brackets to parentheses and collapse whitespace before attaching engine notes. | R/jamovi.R, R/pathj.b.R; workplace test | Model B Models Info note contains resilience and the complete explanation on one line. |
| 13 | Yes | Narrative formatter retained leading zeros; intercept p lacked pvalue formatting; default table formatting also retains zeros. Format p cells at the jamovi boundary as APA text, with numeric engine/report values retained. | R/intelligent_reporting.R, R/jamovi.R, R/pathj.h.R, jamovi/pathj.r.yaml; workplace and fit tests | Test .425, .001 and 0; every Model B path p is APA text; narrative has no p=0.xxx. Display-table asDF p columns are now strings; use the fitted model/report API for numeric p. |
| 14 | Yes | Logical Ordered values serialize as NOT_A_NUMBER in jmvcore text cells. Convert to TRUE/FALSE strings only in the display table. | R/estimate.R; workplace test | Model B Ordered cells are character FALSE; raw binary/continuous detection remains logical TRUE/FALSE for estimation. |
| 15 | Yes | Per-node automatic font fitting shrank long names; fixed-height boxes and large centred edge labels crowded intervening nodes. Wrap full names, keep a common font size, grow node height for wrapped lines, reduce edge-label size and move labels away from intervening node centres. | R/plotter.R, R/pathj.b.R; workplace test | Rendered and visually inspected Model B before/after at 1000×750 and native 700×500; names fit, fonts agree and labels clear boxes in these configurations. Character-preservation test includes a long unbroken name. |

## Diagram manual check

In jamovi, fit A and B above, enable the diagram, turn abbreviation off, and
use the default tree/rectangle layout at its native 700×500 size. Check both
groups: complete support_person and sleep_quality labels, a common font size,
and edge values outside boxes. Repeat with small/large label settings and
rotations. Very dense custom layouts may still need rotation, abbreviation,
or the existing label-offset control; the placement heuristic does not claim
to solve arbitrary graph layouts.
