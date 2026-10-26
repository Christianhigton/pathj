testthat::context("confirmed bug fixes")

testthat::test_that("ordinal path models fit without threshold-table crashes", {
  set.seed(410)
  d <- data.frame(x = stats::rnorm(180))
  latent <- .8 * d$x + stats::rnorm(180)
  d$y <- as.numeric(latent > 0)

  mod <- pathj::pathj(data = d, formula = list("y ~ x"), autoOrdinal = TRUE)
  testthat::expect_true(nrow(mod$models$coefficients$asDF) == 1L)
})

testthat::test_that("robust fit reporting prefers robust and scaled measures", {
  set.seed(411)
  d <- as.data.frame(matrix(stats::rnorm(400 * 4), ncol = 4))
  names(d) <- c("x1", "x2", "y1", "y2")
  d$y1 <- .5 * d$x1 + stats::rnorm(400)
  d$y2 <- .4 * d$x2 + stats::rnorm(400)
  fit <- run_model("y1 ~ x1\ny2 ~ x2\ny1 ~~ y2", d, estimator = "MLR")
  out <- report_fit(fit)
  expected <- lavaan::fitMeasures(fit, c("chisq.scaled", "cfi.robust"))

  testthat::expect_equal(out$table$chisq, unname(expected[["chisq.scaled"]]))
  testthat::expect_equal(out$table$cfi, unname(expected[["cfi.robust"]]))
  testthat::expect_equal(out$table$fit_variant, "robust")
})

testthat::test_that("ICC one-way ANOVA estimate is not inflated by small clusters", {
  set.seed(412)
  d <- data.frame(cluster = rep(seq_len(300), each = 2), y = stats::rnorm(600))
  out <- calculate_icc(d, "cluster", "y")
  testthat::expect_lt(abs(out$icc), .15)
  testthat::expect_match(out$interpretation, "Little clustering")
})

testthat::test_that("outcome correlation is not mistaken for predictor collinearity", {
  set.seed(413)
  d <- data.frame(x = stats::rnorm(300), z = stats::rnorm(300))
  d$y <- d$x + stats::rnorm(300, sd = .03)
  fit <- run_model("y ~ x + z", d, estimator = "MLR")
  row <- check_assumptions(fit, d)
  row <- row[row$check == "Multicollinearity", ]
  testthat::expect_equal(row$status, "Met")
  testthat::expect_match(row$explanation, "Maximum predictor correlation")
})

testthat::test_that("independence is unassessed without cluster information", {
  d <- data.frame(x = stats::rnorm(100), y = stats::rnorm(100))
  fit <- run_model("y ~ x", d, estimator = "MLR")
  row <- check_assumptions(fit, d)
  row <- row[row$check == "Independence", ]
  testthat::expect_equal(row$status, "Info")
  testthat::expect_match(row$explanation, "not assessed")
})

testthat::test_that("generic defined contrasts are not called mediation", {
  d <- data.frame(x = stats::rnorm(250), z = stats::rnorm(250))
  d$y <- .4 * d$x - .2 * d$z + stats::rnorm(250)
  fit <- run_model("y ~ a*x + b*z\ncontrast := a-b", d, estimator = "MLR")
  out <- report_mediation(fit)
  testthat::expect_equal(nrow(out$table), 0L)
  testthat::expect_match(out$text, "other defined parameters")
})

testthat::test_that("uninformative model comparisons select no winner", {
  set.seed(414)
  d <- data.frame(x = stats::rnorm(250))
  d$y <- ordered(ifelse(.4 * d$x + stats::rnorm(250) > 0, "yes", "no"))
  fit1 <- run_model("y ~ x", d)
  fit2 <- run_model("y ~ x", d)
  out <- compare_models(first = fit1, second = fit2)
  testthat::expect_false(any(out$table$best_fit))
  testthat::expect_match(out$text, "No defensible automatic winner")
})

testthat::test_that("automatic reports accept pooled parameter and fit overrides", {
  set.seed(415)
  d <- data.frame(x = stats::rnorm(250), y = stats::rnorm(250))
  fit <- run_model("y ~ x", d, estimator = "MLR")
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)
  pe$est[pe$op == "~"] <- .777
  pooled_fit <- data.frame(chisq = 9, df = 4, pvalue = .06, cfi = .95,
                           tli = .93, rmsea = .06, rmsea.ci.lower = .01,
                           rmsea.ci.upper = .10, srmr = .04, aic = NA, bic = NA)
  out <- generate_report(fit, parameter_estimates = pe, fit_summary = pooled_fit,
                         fit_summary_type = "pooled MI (D4)")
  testthat::expect_equal(out$paths$table$est, .777)
  testthat::expect_equal(out$fit$table$chisq, 9)
  testthat::expect_equal(out$fit$table$fit_variant, "pooled MI (D4)")
})

testthat::test_that("MI status excludes nonconverged fits", {
  d <- data.frame(x = stats::rnorm(100), y = stats::rnorm(100))
  fit <- lavaan::sem("y ~ x", data = d)
  results <- list(
    list(obj = fit, error = FALSE, warning = FALSE),
    list(obj = list(dummy = TRUE), error = FALSE, warning = "did not converge"),
    list(obj = NULL, error = "fit failed", warning = FALSE)
  )
  status <- pathj:::mi_status_table(results)
  testthat::expect_equal(status$status, c("Used", "Excluded", "Failed"))
})

testthat::test_that("imported regressions expose R-squared and CFA exposes loadings", {
  set.seed(416)
  d <- data.frame(x = stats::rnorm(300))
  d$y <- .6 * d$x + stats::rnorm(300)
  regression <- pathj::pathj(data = d, syntaxSource = "lavaan",
                             syntaxText = "y ~ x", syntaxApply = TRUE,
                             syntaxVars = c("x", "y"))
  testthat::expect_true("y" %in% regression$models$r2$asDF$lhs)

  latent <- stats::rnorm(300)
  cfa_data <- data.frame(y1 = latent + stats::rnorm(300, sd = .5),
                         y2 = .8 * latent + stats::rnorm(300, sd = .5),
                         y3 = .7 * latent + stats::rnorm(300, sd = .5))
  cfa <- pathj::pathj(data = cfa_data, syntaxSource = "lavaan",
                      syntaxText = "F =~ y1 + y2 + y3", syntaxApply = TRUE,
                      syntaxVars = names(cfa_data), showSyntax = TRUE,
                      intelligentReport = TRUE)
  testthat::expect_equal(nrow(cfa$models$loadings$asDF), 3L)
  testthat::expect_true("F =~ y1 + y2 + y3" %in% cfa$intelligent$lavaanSyntax$asDF$code)
  testthat::expect_true("Measurement Model" %in% cfa$intelligent$apaText$asDF$section)
})
