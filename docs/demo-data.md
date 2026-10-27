# PATHj demonstration data

The `workplace_wellbeing` datasets are synthetic. They describe 80 fictional
employees measured on six occasions (480 rows), so they are safe to use in
teaching, screenshots, tests, and documentation.

## Variables

| Variable | Level | Description |
| --- | --- | --- |
| `person_id` | Cluster | Employee identifier |
| `occasion` | Within | Measurement occasion, 1 to 6 |
| `condition` | Between | Control or Training group |
| `age` | Between | Age in years |
| `support_person` | Between | Stable perceived support score |
| `resilience` | Between | Stable resilience score |
| `workload` | Within | Occasion-specific workload |
| `stress` | Within | Occasion-specific stress |
| `sleep_quality` | Within | Occasion-specific sleep quality |
| `wellbeing` | Within | Occasion-specific wellbeing |
| `productivity` | Within | Occasion-specific productivity |
| `stress_x_support` | Cross-level | Precomputed stress-by-support product |

Scores other than identifiers, age, occasion, and condition are scaled to
approximately M = 50 and SD = 10. The data-generating model makes workload
increase stress, stress reduce sleep quality, and sleep quality improve
wellbeing. Person-level support buffers adverse effects, and the Training group
has modestly better average outcomes.

## Suggested demonstrations

### Multilevel reporting and ICCs

- Cluster variable: `person_id`
- Within-person variables: `workload`, `stress`, `sleep_quality`, `wellbeing`,
  `productivity`
- Between-person variables: `support_person`, `resilience`, `age`

### Serial mediation

Use `workload` as the predictor, `stress` and `sleep_quality` as sequential
mediators, and `wellbeing` as the outcome. A lavaan-style model is:

```text
stress ~ a*workload
sleep_quality ~ d*stress
wellbeing ~ b*sleep_quality + cp*workload
serial_indirect := a*d*b
```

### Moderation

Predict `wellbeing` from `stress`, `support_person`, and
`stress_x_support`. The interaction is expected to show that support softens
the adverse association between stress and wellbeing.

### Group comparison

Use `condition` as the grouping variable and compare the principal paths
between the Control and Training groups.

### Missing data

Use `workplace_wellbeing_missing` with FIML or multiple imputation and enable
the missing-data diagnostics. The complete dataset is available as a reference.

Because these data were generated for demonstration, estimates should not be
reported as empirical findings.

