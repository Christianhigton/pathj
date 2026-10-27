testthat::context("reporting safeguards")

make_mediation_data <- function(n = 300L, seed = 42L, a = .6, b = .5, direct = .2) {
  set.seed(seed)
  x <- stats::rnorm(n)
  m <- a * x + stats::rnorm(n)
  y <- b * m + direct * x + stats::rnorm(n)
  data.frame(x = x, m = m, y = y)
}

mediation_model <- paste(
  "m ~ a*x",
  "y ~ b*m + cp*x",
  "indirect := a*b",
  "total := cp + a*b",
  sep = "\n")

testthat::test_that("df = 0 is reported as just-identified rather than good fit", {
  d <- make_mediation_data()
  fit <- run_model(mediation_model, d, estimator = "MLR")
  out <- report_fit(fit)
  testthat::expect_equal(out$table$df, 0)
  testthat::expect_equal(out$identification$status, "just_identified")
  testthat::expect_match(out$text, "not be meaningfully evaluated", ignore.case = TRUE)
  testthat::expect_false(grepl("good fit|excellent fit", out$text, ignore.case = TRUE))

  diagnostic <- check_assumptions(fit, d)
  global <- diagnostic[diagnostic$check == "Model fit", ]
  testthat::expect_equal(global$status, "Warning")
  testthat::expect_equal(global$result, "Not assessable (saturated model)")
  testthat::expect_match(global$explanation, "zero degrees of freedom")
  testthat::expect_match(global$recommendation, "individual parameter estimates")
  testthat::expect_match(global$recommendation, "do not use global fit indices")
  testthat::expect_true(grepl("saturated (df = 0)", out$text, fixed = TRUE))
  testthat::expect_match(out$text, "reflect model saturation")

  automatic <- generate_report(fit, data = d)
  testthat::expect_length(automatic$warnings, 1L)
  testthat::expect_match(automatic$warnings, "saturated.*df = 0", ignore.case = TRUE)
  testthat::expect_match(pathj:::saturated_model_notice(), "Saturated Model Detected", fixed = TRUE)
  testthat::expect_match(automatic$text, "Global model fit could therefore not be meaningfully evaluated")
  testthat::expect_gt(nrow(automatic$paths$table), 0L)
})

testthat::test_that("df > 0 retains ordinary global-fit evaluation", {
  d <- make_mediation_data()
  fit <- run_model("m ~ a*x\ny ~ b*m\nindirect := a*b", d, estimator = "MLR")
  out <- report_fit(fit)
  testthat::expect_gt(out$table$df, 0)
  testthat::expect_equal(out$identification$status, "overidentified")
  testthat::expect_length(generate_report(fit, data = d)$warnings, 0L)
  global <- check_assumptions(fit, d)
  global <- global[global$check == "Model fit", ]
  testthat::expect_true(global$status %in% c("Met", "Warning"))
  testthat::expect_false(grepl("saturat", out$text, ignore.case = TRUE))
})

testthat::test_that("negative df is not by itself classified as underidentified", {
  testthat::expect_equal(
    pathj:::classify_model_identification(-1, converged = TRUE,
                                          information_ok = TRUE,
                                          standard_errors_ok = TRUE),
    "estimation_problem"
  )
  testthat::expect_equal(
    pathj:::classify_model_identification(-1, converged = TRUE,
                                          information_ok = FALSE,
                                          standard_errors_ok = FALSE),
    "underidentified"
  )
  testthat::expect_equal(
    pathj:::classify_model_identification(0, converged = FALSE,
                                          information_ok = TRUE,
                                          standard_errors_ok = TRUE),
    "estimation_problem"
  )
})

testthat::test_that("nonconvergence takes precedence over a zero df value", {
  d <- make_mediation_data()
  not_fitted <- lavaan::sem("m ~ x\ny ~ m + x", data = d, do.fit = FALSE)
  identification <- pathj:::model_identification(not_fitted, df = 0)
  testthat::expect_equal(identification$status, "estimation_problem")
  testthat::expect_false(identification$converged)
  testthat::expect_false(identical(identification$status, "just_identified"))
})

testthat::test_that("saturated narratives handle unavailable fit indices", {
  d <- make_mediation_data()
  fit <- run_model(mediation_model, d, estimator = "MLR")
  unavailable <- data.frame(
    chisq = 0, df = 0, pvalue = NaN, cfi = NaN, tli = NA_real_,
    rmsea = NaN, rmsea.ci.lower = NaN, rmsea.ci.upper = NA_real_,
    srmr = NaN, aic = NA_real_, bic = NA_real_
  )
  out <- report_fit(fit, fit_summary = unavailable,
                    fit_summary_type = "test summary")
  testthat::expect_equal(out$identification$status, "just_identified")
  testthat::expect_true(all(is.na(out$table[c("pvalue", "cfi", "tli", "rmsea",
                                               "rmsea.ci.lower", "rmsea.ci.upper", "srmr")])))
  testthat::expect_false(grepl("NaN", out$text, fixed = TRUE))
  testthat::expect_true(grepl("saturated (df = 0)", out$text, fixed = TRUE))
})

testthat::test_that("model comparison does not treat saturation as superior global fit", {
  d <- make_mediation_data()
  saturated <- run_model("m ~ x\ny ~ m + x", d, estimator = "MLR")
  overidentified <- run_model("m ~ x\ny ~ m", d, estimator = "MLR")
  comparison <- compare_models(saturated = saturated,
                               overidentified = overidentified)
  saturated_row <- comparison$table[comparison$table$model == "saturated", ]
  testthat::expect_equal(saturated_row$identification, "just_identified")
  testthat::expect_match(saturated_row$fit_note, "do not demonstrate theoretical adequacy")
  testthat::expect_match(comparison$text, "not statistical evidence of superior theoretical fit")
  testthat::expect_match(comparison$text, "parsimony and theoretical justification")
})

testthat::test_that("unstable and ordinary mediated proportions are distinguished", {
  unstable <- pathj:::mediation_proportion(indirect = .20, direct = -.19, total = .01)
  testthat::expect_false(unstable$interpretable)
  testthat::expect_true(is.na(unstable$value))
  testthat::expect_match(unstable$reason, "opposite|close to zero", ignore.case = TRUE)

  ordinary <- pathj:::mediation_proportion(indirect = .20, direct = .30, total = .50)
  testthat::expect_true(ordinary$interpretable)
  testthat::expect_equal(ordinary$value, 40)
})

testthat::test_that("significant negative paths use neutral language", {
  set.seed(4)
  d <- data.frame(x = stats::rnorm(250))
  d$y <- -.7 * d$x + stats::rnorm(250, sd = .6)
  fit <- run_model("y ~ x", d, estimator = "MLR")
  paths <- report_paths(fit)
  insights <- generate_model_insights(fit)
  combined <- paste(paths$text, paths$table$interpretation, insights$text)
  testthat::expect_match(combined, "lower outcome values|lower y values|negative", ignore.case = TRUE)
  testthat::expect_false(grepl("protective|buffering|beneficial|harmful|risk factor", combined, ignore.case = TRUE))
})

testthat::test_that("one significant group is not treated as a significant difference", {
  set.seed(9)
  group <- factor(c(rep("large", 180), rep("small", 15)))
  x <- stats::rnorm(length(group))
  y <- .22 * x + stats::rnorm(length(group))
  fit <- run_model("y ~ x", data.frame(x, y, group), group = "group", estimator = "MLR")
  out <- compare_groups(fit)
  row <- out$table[1, ]
  testthat::expect_true(xor(row$group_1_pvalue < .05, row$group_2_pvalue < .05))
  testthat::expect_false(row$significant)
  testthat::expect_match(row$interpretation, "one group.*however.*did not", ignore.case = TRUE)
})

testthat::test_that("small multigroup samples are displayed and warned about", {
  set.seed(9)
  group <- factor(c(rep("large", 180), rep("small", 15)))
  x <- stats::rnorm(length(group)); y <- .22 * x + stats::rnorm(length(group))
  out <- compare_groups(run_model("y ~ x", data.frame(x, y, group),
                                  group = "group", estimator = "MLR"))
  testthat::expect_true(any(out$group_sizes$n < 20))
  testthat::expect_match(out$text, "fewer than 20")
  testthat::expect_true(all(c("group_1_n", "group_2_n") %in% names(out$table)))
})

testthat::test_that("indirect-effect support follows its confidence interval", {
  supported <- report_mediation(run_model(mediation_model, make_mediation_data(), estimator = "MLR"))
  testthat::expect_true(supported$table$ci_supported[[1]])
  testthat::expect_match(supported$text, "statistically supported")

  null_data <- make_mediation_data(seed = 101, a = 0, b = 0, direct = .2)
  unsupported <- report_mediation(run_model(mediation_model, null_data, estimator = "MLR"))
  testthat::expect_false(unsupported$table$ci_supported[[1]])
  testthat::expect_match(unsupported$text, "not statistically supported")
  testthat::expect_match(unsupported$text, "mediation model was tested")
})

testthat::test_that("simulation power is the proportion with p below alpha", {
  sims <- data.frame(
    effect = rep(c("a", "b"), each = 5),
    n = rep(c(50, 100), each = 5),
    pvalue = c(.01, .02, .049, .05, .80, .001, .20, .03, .70, .90)
  )
  out <- simulation_power_summary(sims, alpha = .05)
  testthat::expect_equal(out$power[out$effect == "a"], 3 / 5)
  testthat::expect_equal(out$power[out$effect == "b"], 2 / 5)
  testthat::expect_equal(out$valid_simulations, c(5L, 5L))
})
