# SEMLj and PathJ Integration Plan

This document records the local inspection of `semlj/semlj` and the intended architecture for a SEMJ / PathJ-SEM module. No commercial syntax manuals or proprietary source code are used. SEMLj is GPL-2, and any adapted code must retain compatible licensing and attribution to Marcello Gallucci and Sebastian Jentschke.

## SEMLj Architecture Findings

SEMLj exposes two jamovi analyses:

- `semljsyn`: syntax-first SEM input.
- `semljgui`: interactive GUI input that generates lavaan syntax.

The useful shared backend is:

- `Datamatic`: inspects syntax/data, detects observed variables, ordered variables, multigroup variables, cluster variables, and prepares data for lavaan.
- `Initer`: builds user syntax, optional indirect effects, lavaanified parameter structure, and initial result tables.
- `Runner`: calls `lavaan::lavaan()`, extracts fit measures, parameter tables, reliability, HTMT, Mardia diagnostics, ICC, modification indices, predictions, and residuals.
- `SmartTable` / `SmartArray`: reduce repeated jamovi result-table boilerplate and keep init/run behaviour centralised.
- `Plotter`: prepares semPlot diagrams from the fitted lavaan model.
- `jamovi/js/main.js` and `jamovi/js/gui.events.js`: provide a syntax editor and GUI-to-syntax syncing.

This is the right base architecture for SEMJ because GUI and syntax already meet at lavaan syntax before estimation.

## PathJ Features To Merge

PathJ contributes:

- path-model-focused GUI ergonomics
- automatic mediation path discovery
- syntax import/export across Mermaid, Mplus-style, OpenMx RAM-style, and lavaan-style inputs
- richer missing-data diagnostics
- APA-style reporting and plain-English warnings
- model comparison and p-curve style summaries
- multilevel planning concepts such as cluster, within, and between variables

The key change is to avoid making lavaan syntax the only shared model representation. GUI, syntax import, templates, diagrams, and future engines should use an engine-agnostic schema first, then export to lavaan or another backend.

## Proposed Runtime Flow

1. User edits GUI, visual canvas, template, or syntax.
2. Input parser converts the source into `pathj_sem_schema`.
3. Schema validator checks nodes, edges, metadata, data variables, and selected engine capabilities.
4. Exporter generates lavaan syntax for Phase 1 estimation.
5. SEMLj-style `Datamatic -> Runner -> SmartTable -> Plotter` executes and reports.
6. PathJ-style diagnostics and APA reporting consume the fitted model, schema, and warnings.

## Initial Schema Boundary

The local implementation now has `R/sem_schema.R`, which defines:

- node types: observed, latent, residual, intercept, constant, group, cluster, time, random effect, composite, formative
- edge types: regression, loading, covariance, residual covariance, variance, intercept, indirect, moderation, cross-lagged, autoregressive, random intercept, random slope, formative
- metadata: source, estimator, missing method, bootstrap, standardisation, robust correction, categorical variables, groups, constraints, indirect effects, unsupported features, engine support, generated syntax
- validation: missing ids, duplicate nodes, unsupported types, missing edge endpoints, data-variable warnings, lavaan capability warnings

## Phased Implementation

Phase 1 should remain lavaan-first:

- keep SEMLj's estimation pattern
- add schema validation before lavaanify
- add generated lavaan, Mermaid, OpenMx RAM, and Mplus-style syntax tables
- add APA report panels
- add unit tests for schema validation and syntax round trips

Phase 2 should add visual modelling:

- PathJ-style canvas and auto-layouts
- GUI-to-schema and schema-to-GUI syncing
- live syntax generation from schema

Phase 3 should broaden import/export:

- improve Mplus-style parser coverage
- improve OpenMx RAM parser coverage without evaluating R code
- add parser warning/result tables in jamovi

Phase 4 should add workflows:

- mediation, moderation, moderated mediation templates
- CFA, second-order CFA, bifactor, formative/composite templates
- multigroup invariance dashboard

Phase 5 should add advanced model families:

- multilevel SEM templates
- longitudinal templates, CLPM, RI-CLPM, growth models
- OpenMx backend prototype

Phase 6 should add Bayesian and non-Gaussian planning:

- brms/Stan capability maps
- blavaan backend exploration
- outcome-type warnings and engine suggestions

## Implementation Rules

- Do not manually edit generated `.h.R` files for new work; change YAML and regenerate with `jmvtools`.
- Keep all imported syntax as data, never executable code.
- Preserve generated syntax for reproducibility.
- Use UK English in user-facing text.
- Keep advanced controls behind advanced panels.
- Add tests for every parser, exporter, validator, diagnostic, and report generator.
