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
  testthat::expect_match(out$text, "not informative", ignore.case = TRUE)
  testthat::expect_false(grepl("good fit|excellent fit", out$text, ignore.case = TRUE))

  diagnostic <- check_assumptions(fit, d)
  global <- diagnostic[diagnostic$check == "Global model fit", ]
  testthat::expect_equal(global$status, "Info")
  testthat::expect_match(global$explanation, "Not evaluable")
})

testthat::test_that("df > 0 retains ordinary global-fit evaluation", {
  d <- make_mediation_data()
  fit <- run_model("m ~ a*x\ny ~ b*m\nindirect := a*b", d, estimator = "MLR")
  out <- report_fit(fit)
  testthat::expect_gt(out$table$df, 0)
  testthat::expect_equal(out$identification$status, "overidentified")
  global <- check_assumptions(fit, d)
  global <- global[global$check == "Global model fit", ]
  testthat::expect_true(global$status %in% c("Met", "Warning"))
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
