# Multigroup effect-difference audit

Implemented the twelve requested items in separate commits. The calculation of
existing IE, TE and DIE estimates is preserved. No large refactor was needed.

Verification used lavaan 0.7-2 and the repository's dependency cache. The full
suite passed **875 assertions across 98 tests**, with zero failures/errors or
warnings and one existing CRAN-only skip. It ran in an isolated copy with the
committed workplace dataset: the working copy of `data/workplace_wellbeing.csv`
contains pasted task text and was deliberately left untouched.

The requested `students_phones_sleep_exams` dataset was not available. Automated
checks use reproducible generated Model 3 data (seed 812, N = 420, 210 per group),
plus a three-group version and a bootstrap fixture. All original fitted
estimates, SEs, CIs and p values, in both free and constrained Model 3, matched the
pre-change fits to tolerance 1e-10. This is a preservation check, not a claim to
have reproduced the supplied student-data values.

[Before/after text output for items 1, 2, 9 and 10](die-reporting-before-after.txt)
comes from the generated fixture and actual result tables. Numeric output in
that transcript is rounded for readability. Baseline commit:
`cbcc311220f851e12ac836b81c342682cc6a1d54`.

All items have numbered automated tests in
`tests/testthat/test_die_reporting.R`, with shared data generation in
`tests/testthat/helper-die.R`.

| Item | Status | Implementation files | Verification / uncertainty |
|---|---|---|---|
| 1 | Done | `R/mediation_topology.R`, `R/estimate.R`, `R/pathj.h.R`, `jamovi/pathj.r.yaml` | Eight index rows equal fitted DIE estimates, SEs and intervals; routes and group order checked. |
| 2 | Done | `R/syntax.R` | Four DTEs; DTE3 = TE3 − TE7; independent-group variance check; placed after DIEs. |
| 3 | Done | `R/mediation_topology.R`, `R/estimate.R`, `R/pathj.h.R`, `jamovi/pathj.r.yaml` | Three groups yield 24 pairwise DIEs and eight omnibus Wald rows (df = 2), checked against lavaan. Omnibus rows share the index table and have blank index/CI cells. |
| 4 | Done | `R/syntax.R`, `R/mediation_topology.R`, `R/pathj.b.R` | Exact route/group label and direction note assertions. |
| 5 | Done | `R/estimate.R`, `R/mediation_topology.R` | DIE/DTE β cells blank; IE7 β preserved. |
| 6 | Done | `R/estimate.R`, `R/intelligent_reporting.R`, `R/mediation_topology.R`, `R/pathj.b.R` | Percentile, adjusted bias-corrected, normal and basic bootstrap intervals checked against lavaan, including a 90% level. Delta-method and bootstrap recommendation notes checked. |
| 7 | Done | `R/mediation_topology.R`, `R/pathj.b.R` | Caution absent at three DIEs, present at four. |
| 8 | Done | `R/intelligent_reporting.R`, `R/mediation_topology.R` | CI-supported DIEs, B/CI/p, nonsupported count and DTE3 verified. CI support takes precedence over p. All DTEs are reported because PATHj has no main-predictor selector. |
| 9 | Done | `R/estimate.R`, `R/mediation_topology.R`, `R/pathj.b.R`, `R/pahtj.f.R`, `man/pathj.Rd` | Constrained differences hidden; IE rows retained; index explains zero by design. Detects GUI constraints and explicit equality links. R wrapper now exposes existing `group.equal` option. |
| 10 | Done | `R/intelligent_reporting.R`, `R/jamovi.R`, `R/pathj.b.R`, `R/pathj.h.R`, `jamovi/pathj.r.yaml` | Constrained comparison has zero displayed rows; tiny difference/z cells round to zero for display only; raw statistics and p values preserved. |
| 11 | Done | `R/intelligent_reporting.R` | Exact requested Constraints Score Test narrative checked in the report API and rendered result table. |
| 12 | Done | `R/intelligent_reporting.R`, `R/estimate.R` | Both Models Info and N/parameter use lavaan's equality-adjusted `fitMeasures(..., "npar")`. Seven independent regression constraints reduce fixture count by seven; redundant constraints also checked. Supplied saturated-model count of 30 awaits its dataset/settings. |

## Repeat the automated checks

With lavaan >= 0.7-2 and the package dependencies installed:

```r
pkgload::load_all(".", quiet = TRUE)
testthat::test_dir("tests/testthat", reporter = "summary")
```

On the development Mac, prepend
`.libPaths(c("build/R4.5.0-arm64-macos", .libPaths()))` to use the cached dependencies.
The workplace integration tests require the actual CSV, not the task-text file
currently at that working-tree path.

## Manual check with the requested student dataset

1. Fit the four Model 3 equations from the request, grouping by `condition`,
   using FIML, Indirect Effects and Standard SEs. Check IE7 = −.095,
   IE15 = −.030, DIE7 = −.065, CI [−.109, −.020], p = .004; TE3 = −.132,
   TE7 = −.039; DTE3 = −.093. Compare unrounded output when available.
2. Confirm DIE7 appears in the Index of Moderated Mediation table with the same
   SE, interval and p value. Check the direction, CI-method, β and multiplicity
   notes and the Moderated mediation narrative.
3. Select each bootstrap CI type and confirm index/defined-parameter intervals
   agree. Use enough bootstrap draws for substantive analysis; the 80 draws in
   the automated fixture test wiring, not inferential stability.
4. Constrain regression coefficients equal. IE/TE rows should remain, DIE/DTE
   rows should disappear, the index should show the zero-by-design note, and
   Multigroup Path Comparison should be hidden. The narrative should refer to
   the Constraints Score Test. Verify the supplied χ²(21) = 46.3 and constraint
   score χ²(7) = 24.0, then the saturated-model free count of 30 and its N/parameter
   calculation under the corresponding settings.
5. For three groups, check each group pair and the omnibus rows in the index
   table. Wald tests use the fitted covariance matrix; a singular contrast
   covariance is reported as unavailable rather than given a misleading p value.

The omnibus metadata is generated for PATHj's automatic indirect routes. Custom
imported definitions without that route metadata are not automatically assigned
to route-level omnibus tests. Arbitrary nonlinear custom constraints beyond
explicit parameter-equality links are not inferred from numerical equality.
