# pathJTEST Merge Back Signal

This repository can build a standalone jamovi test module named `pathJTEST`.
The standalone build is only for testing and avoids replacing an installed
`pathj` module in jamovi.

The original upstream project should not be commented on or pushed to while
this package is still being tested. Keep the test work in a separate GitHub
repository/remote, for example:

```bash
git remote add pathjtest git@github.com:<your-user-or-org>/pathJTEST.git
git push -u pathjtest main
```

When the `pathJTEST` module is working and you want the changes folded back
into the normal PATHj module, send this exact message:

```text
Merge pathJTEST back into PATHj
```

At that point, the standalone-test wrapper should not be merged as the product
identity. The production module should remain:

- R package: `pathj`
- jamovi module name: `pathj`
- jamovi module title: `Path Analysis`

The substantive code changes to merge are the reporting, diagnostics, model
comparison, and testing changes in the main project files.

## Changes included in pathJTEST

- Standalone jamovi module identity for testing:
  - Module name/title: `pathJTEST`
  - Separate jamovi menu group, not a submenu of the installed PATHj module
  - Build script: `scripts/build_pathjtest_jmo.R`
  - Output format: `pathJTEST-standalone-<version>.jmo`
- Intelligent reporting system:
  - Automatic APA narrative sections
  - Copyable APA report paragraph as a styled callout box
  - Warning that generated reporting must be checked against theory, reviewer expectations, and common sense
  - Teaching/reporting levels: basic, APA, advanced
- Diagnostics and recommendations:
  - Model fit interpretation
  - Normality, multicollinearity, residual, sample-size, missing-data, recursive-structure, and related checks
  - Recommendations table
  - Modification-index warnings
  - P-curve screen
- Model insight and effect interpretation:
  - Strongest predictors
  - Key mediation summaries
  - Risk/protective direction labels where inferable
  - Small/medium/large effect labels
  - Suppression/sign-reversal and common issue screens
- Mediation, moderation, and multigroup outputs:
  - Mediation decomposition table
  - Moderation/interaction detection
  - Multigroup comparison table
  - Model comparison and selection table
- Syntax import:
  - `Model input` selector for GUI builder, lavaan syntax, and Mermaid syntax
  - Multi-line custom syntax editor in the jamovi GUI
  - Import button that confirms imported syntax and tries to populate GUI model fields
  - Mermaid parser accepts multi-line and one-line Mermaid graphs
  - Sample syntax dropdown with lavaan and Mermaid examples
  - Copy sample and insert sample buttons
  - Optional generated lavaan/Mermaid syntax output tables
  - Optional Mermaid syntax below the path diagram
- Path diagram updates:
  - Optional significance stars on path labels
  - Non-overlapping path diagram legend
  - Legend is shown directly under the path diagram output and only when the path diagram and legend options are selected
- Ordinal/binary handling:
  - Automatic variable-type detection
  - WLSMV/ordered-variable support when ordinal or binary model variables are detected
  - Estimator choice reporting
- Missing-data handling:
  - FIML, multiple imputation, listwise, and pairwise handling paths
  - Missing-data statement in automatic reporting
  - Missing-data diagnostics
- Testing/validation:
  - `testthat` tests for intelligent reporting helpers
  - Smoke tests for lavaan/Mermaid syntax import
  - GitHub Actions workflow scaffold
  - Coverage tooling suggested through `covr`

## Current standalone build

The latest local standalone build should be installed into jamovi for testing:

```text
pathJTEST-standalone-1.1.7.9006.jmo
```
