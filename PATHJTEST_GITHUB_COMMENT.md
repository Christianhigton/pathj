# pathJTEST standalone test module

This project is a standalone jamovi test module for the PATHj/path analysis work.
It is intentionally separate from the original PATHj repository while the module
is still being tested. Do not comment on or push these changes to the original
project until the module is ready to merge back.

## Current build

- Module: `pathJTEST`
- Version: `1.1.7.9006`
- Standalone jamovi file: `pathJTEST-standalone-1.1.7.9006.jmo`

## Main changes included

- Added an intelligent reporting system for APA-style model fit, paths,
  mediation, moderation, multigroup effects, missing data, and model insights.
- Added diagnostics and recommendations, including assumptions, modification
  indices, p-curve screening, variable type detection, common issues, and
  actionable next steps.
- Added model comparison and selection output.
- Added mediation decomposition and multigroup path comparison tables.
- Added ordinal/binary detection with WLSMV/ordered-variable support.
- Added copyable APA report paragraph as a styled callout box rather than a
  table, with a clear warning to check reviewer expectations, theory, and common
  sense before manuscript use.
- Added lavaan and Mermaid syntax import:
  - Model input selector: GUI builder, lavaan syntax, Mermaid syntax
  - Multi-line syntax editor in the jamovi GUI
  - Import syntax button
  - GUI-population attempt after import so users can edit paths in jamovi
  - One-line and multi-line Mermaid parsing
  - Sample syntax dropdown
  - Copy sample syntax and insert sample syntax buttons
- Added path diagram improvements:
  - Optional significance stars
  - Non-overlapping legend
  - Legend appears directly under the path diagram and only when the path
    diagram and legend options are selected
  - Optional Mermaid syntax below the diagram
- Added tests and validation scaffolding:
  - `testthat` intelligent-reporting tests
  - Syntax-import smoke tests
  - GitHub Actions scaffold
  - `covr` suggested for coverage
- Added standalone build helper:
  - `scripts/build_pathjtest_jmo.R`
- Added merge-back instructions:
  - `PATHJTEST_MERGE_BACK.md`

## Validation completed locally

- Built standalone `.jmo` successfully.
- Installed the R package locally with `R CMD INSTALL`.
- Ran one-line Mermaid import with path diagram enabled successfully.
- Ran intelligent reporting tests successfully: 26 passing tests.

## Merge-back signal

When this standalone test module is ready to fold back into PATHj, use:

```text
Merge pathJTEST back into PATHj
```

At merge-back time, keep the production identity as:

- R package: `pathj`
- jamovi module name: `pathj`
- jamovi module title: `Path Analysis`

The standalone `pathJTEST` identity is only for testing.
