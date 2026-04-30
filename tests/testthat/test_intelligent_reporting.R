context("intelligent reporting")

set.seed(123)

sim_data <- data.frame(
  x = rnorm(250),
  z = rnorm(250)
)
sim_data$m <- 0.55 * sim_data$x + rnorm(250, sd = 0.80)
sim_data$y <- 0.35 * sim_data$m + 0.25 * sim_data$x + 0.20 * sim_data$x * sim_data$z + rnorm(250, sd = 0.90)
sim_data$x_z <- sim_data$x * sim_data$z
sim_data$group <- ifelse(sim_data$z > 0, "high", "low")
sim_data$id <- factor(rep(seq_len(50), each = 5))
sim_data$ord <- ordered(cut(sim_data$x, breaks = 4, labels = FALSE))
sim_data$bin <- as.integer(sim_data$z > 0)

med_model <- "
  m ~ a*x
  y ~ b*m + cp*x + int*x_z
  indirect := a*b
  total := cp + (a*b)
"

testthat::test_that("run_model estimates and reports APA sections", {
  fit <- run_model(med_model, sim_data, estimator = "MLR")
  testthat::expect_true(lavaan::inspect(fit, "converged"))

  report <- generate_report(fit, sim_data, teaching_mode = "advanced")
  testthat::expect_true(grepl("CFI", report$text))
  testthat::expect_true(grepl("RMSEA", report$text))
  testthat::expect_true(grepl("Missing data handling", report$text))
  testthat::expect_true(nrow(report$paths$table) >= 4)
  testthat::expect_true(nrow(report$mediation$table) >= 1)
  testthat::expect_true(nrow(report$moderation$table) >= 1)
})

testthat::test_that("variable detection triggers WLSMV for ordinal and binary variables", {
  types <- detect_variable_types(sim_data[, c("x", "ord", "bin")])
  testthat::expect_equal(types$type[types$variable == "ord"], "ordinal")
  testthat::expect_equal(types$type[types$variable == "bin"], "binary")

  fit <- suppressWarnings(run_model("y ~ ord + bin", sim_data))
  testthat::expect_equal(attr(fit, "pathj_estimator"), "WLSMV")
  testthat::expect_true(all(c("ord", "bin") %in% attr(fit, "pathj_ordered")))
})

testthat::test_that("assumption diagnostics return statuses and recommendations", {
  fit <- run_model("m ~ x + z\ny ~ m + x + z", sim_data, estimator = "MLR")
  diagnostics <- check_assumptions(fit, sim_data)
  testthat::expect_true(all(c("check", "status", "explanation", "recommendation") %in% names(diagnostics)))
  testthat::expect_true("Model fit" %in% diagnostics$check)
  testthat::expect_true(all(diagnostics$status %in% c("Met", "Warning", "Violated")))
  testthat::expect_type(generate_recommendations(diagnostics), "character")
})

testthat::test_that("multilevel interpretation uses cluster, within, and between variables", {
  fit <- run_model("m ~ x + z\ny ~ m + x + z", sim_data, estimator = "MLR")
  ml <- report_multilevel(
    sim_data,
    cluster = "id",
    within = c("x", "m"),
    between = "z")
  testthat::expect_true(all(c("variable", "clusters", "icc", "interpretation") %in% names(ml$table)))
  testthat::expect_true(grepl("Within-person variables", ml$text))
  testthat::expect_true(grepl("Between-person variables", ml$text))

  report <- generate_report(
    fit,
    sim_data,
    teaching_mode = "advanced",
    cluster = "id",
    within = c("x", "m"),
    between = "z")
  testthat::expect_true("multilevel" %in% names(report))
  testthat::expect_true(grepl("Cluster variable: id", report$text))
  testthat::expect_true("Independence" %in% report$diagnostics$check)
})

testthat::test_that("multilevel interpretation wording is stable", {
  testthat::local_edition(3)
  ml <- report_multilevel(
    sim_data,
    cluster = "id",
    within = c("x", "m"),
    between = "z")
  testthat::expect_snapshot(ml$text)
})

testthat::test_that("model insight engine identifies strongest predictors", {
  fit <- run_model(med_model, sim_data, estimator = "MLR")
  insights <- generate_model_insights(fit)
  testthat::expect_true("strongest_predictors" %in% names(insights))
  testthat::expect_true(nrow(insights$strongest_predictors) > 0)
  testthat::expect_true(grepl("strongest direct predictor", insights$text))
})

testthat::test_that("multigroup comparison table is produced", {
  fit <- run_model("y ~ x + m", sim_data, group = "group", estimator = "MLR")
  mg <- compare_groups(fit)
  testthat::expect_true(all(c("path", "group_1_beta", "group_2_beta", "difference", "z", "pvalue") %in% names(mg$table)))
  testthat::expect_true(grepl("group", mg$text, ignore.case = TRUE))
})

testthat::test_that("compare_models selects the model with lowest BIC", {
  fit1 <- run_model("y ~ x", sim_data, estimator = "MLR")
  fit2 <- run_model("y ~ x + m + z", sim_data, estimator = "MLR")
  cmp <- compare_models(model_1 = fit1, model_2 = fit2)
  testthat::expect_true(all(c("model", "cfi", "rmsea", "srmr", "aic", "bic", "best_fit") %in% names(cmp$table)))
  testthat::expect_equal(sum(cmp$table$best_fit), 1)
  testthat::expect_true(grepl("lowest", cmp$text))
})

testthat::test_that("diagnostic helper tables are stable", {
  fit <- run_model(med_model, sim_data, estimator = "MLR")
  mi <- diagnose_modification_indices(fit, threshold = 1000)
  pc <- p_curve_analysis(fit)
  pc_med <- p_curve_analysis(fit, target = "mediation")
  pc_mod <- p_curve_analysis(fit, target = "moderation")
  pc_ml <- p_curve_analysis(fit, target = "multilevel", within = c("x", "m"), between = "z")
  testthat::expect_true("table" %in% names(mi))
  testthat::expect_true("summary" %in% names(pc))
  testthat::expect_true(pc$summary$n_tests > 0)
  testthat::expect_true(pc_med$summary$n_tests >= 1)
  testthat::expect_true(pc_mod$summary$n_tests >= 1)
  testthat::expect_true(pc_ml$summary$n_tests >= 1)
  testthat::expect_true(grepl("mediation", pc_med$text))
  testthat::expect_true(grepl("moderation", pc_mod$text))
  testthat::expect_true(grepl("multilevel", pc_ml$text))
})
