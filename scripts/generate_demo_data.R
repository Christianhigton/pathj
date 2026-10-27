#!/usr/bin/env Rscript

# Reproducibly generate synthetic data used by the PATHj demonstrations.
# The data are entirely fictional and are designed to contain interpretable
# within-person, between-person, mediation, moderation, and group effects.

set.seed(20261026)

n_people <- 80L
n_occasions <- 6L
n <- n_people * n_occasions

person_id <- rep(sprintf("P%03d", seq_len(n_people)), each = n_occasions)
occasion <- rep(seq_len(n_occasions), times = n_people)
condition_person <- sample(rep(c("Control", "Training"), length.out = n_people))
condition <- rep(condition_person, each = n_occasions)
age_person <- round(pmin(pmax(rnorm(n_people, 39, 9), 21), 62))
age <- rep(age_person, each = n_occasions)

# Stable person characteristics (level 2).
person_resilience <- rnorm(n_people)
person_support_z <- 0.30 * person_resilience + rnorm(n_people, sd = 0.95)
support_person <- round(50 + 10 * as.numeric(scale(person_support_z)), 2)
support_z <- rep(as.numeric(scale(support_person)), each = n_occasions)
resilience <- round(50 + 10 * as.numeric(scale(person_resilience)), 2)
resilience <- rep(resilience, each = n_occasions)

# Person-specific baselines create realistic clustering.
stress_intercept <- rep(rnorm(n_people, sd = 5.5), each = n_occasions)
sleep_intercept <- rep(rnorm(n_people, sd = 4.0), each = n_occasions)
wellbeing_intercept <- rep(rnorm(n_people, sd = 4.5), each = n_occasions)

# Workload varies within people and is mildly lower after training.
workload_raw <- rnorm(n) + rep(rnorm(n_people, sd = 0.45), each = n_occasions)
workload_raw <- workload_raw - 0.18 * (condition == "Training") * (occasion - 1)
workload <- round(50 + 10 * as.numeric(scale(workload_raw)), 2)
workload_z <- as.numeric(scale(workload))

# Higher workload raises stress. Support buffers the workload effect.
stress_latent <- 0.62 * workload_z - 0.22 * support_z -
    0.20 * workload_z * support_z - 0.18 * (condition == "Training") +
    stress_intercept / 10 + rnorm(n, sd = 0.55)
stress <- round(50 + 10 * as.numeric(scale(stress_latent)), 2)
stress_z <- as.numeric(scale(stress))

# Stress impairs sleep; support and training are protective.
sleep_latent <- -0.58 * stress_z + 0.24 * support_z +
    0.18 * (condition == "Training") + sleep_intercept / 10 +
    rnorm(n, sd = 0.60)
sleep_quality <- round(50 + 10 * as.numeric(scale(sleep_latent)), 2)
sleep_z <- as.numeric(scale(sleep_quality))

# Sleep partially mediates the stress-wellbeing association.
wellbeing_latent <- 0.53 * sleep_z - 0.24 * stress_z + 0.20 * support_z +
    0.16 * stress_z * support_z + 0.14 * (condition == "Training") +
    wellbeing_intercept / 10 +
    rnorm(n, sd = 0.62)
wellbeing <- round(50 + 10 * as.numeric(scale(wellbeing_latent)), 2)
wellbeing_z <- as.numeric(scale(wellbeing))

productivity_latent <- 0.42 * wellbeing_z - 0.18 * stress_z +
    0.12 * workload_z + rnorm(n, sd = 0.75)
productivity <- round(50 + 10 * as.numeric(scale(productivity_latent)), 2)

demo <- data.frame(
    person_id,
    occasion,
    condition,
    age,
    support_person = rep(support_person, each = n_occasions),
    resilience,
    workload,
    stress,
    sleep_quality,
    wellbeing,
    productivity,
    stress_x_support = round(stress_z * support_z, 3),
    stringsAsFactors = FALSE
)

utils::write.csv(demo, "data/workplace_wellbeing.csv", row.names = FALSE,
                 na = "")

# A companion dataset for FIML/MI and missingness-diagnostics demonstrations.
# Missingness is somewhat more likely under high stress and in later occasions.
demo_missing <- demo
missing_probability <- stats::plogis(-3.25 + 0.32 * stress_z +
                                      0.12 * (occasion - 1))
for (variable in c("stress", "sleep_quality", "wellbeing", "productivity")) {
    missing_rows <- stats::runif(n) < missing_probability
    demo_missing[missing_rows, variable] <- NA_real_
}
demo_missing$stress_x_support <- NA_real_
complete_interaction <- !is.na(demo_missing$stress)
demo_missing$stress_x_support[complete_interaction] <- round(
    as.numeric(scale(demo$stress))[complete_interaction] * support_z[complete_interaction],
    3
)

utils::write.csv(demo_missing, "data/workplace_wellbeing_missing.csv",
                 row.names = FALSE, na = "")
