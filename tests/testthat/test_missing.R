testthat::context("missing data diagnostics")

testthat::test_that("missing pattern plot data describes unique patterns", {
  d <- data.frame(
    x = c(1, NA, 3, NA),
    m = c(1, 2, NA, 2),
    y = c(2, 3, 4, 5)
  )

  plot_data <- pathj:::missing_pattern_plot_data(d, c("x", "m", "y"))

  testthat::expect_true(all(c("pattern", "variable", "missing", "n", "percent") %in% names(plot_data)))
  testthat::expect_equal(length(unique(plot_data$pattern)), 3)
  testthat::expect_equal(nrow(plot_data), 9)
  testthat::expect_true(any(plot_data$missing))
  testthat::expect_equal(max(plot_data$n), 2)
})

testthat::test_that("missing pattern plot data keeps complete-data pattern", {
  d <- data.frame(
    x = c(1, 2, 3),
    y = c(2, 3, 4)
  )

  plot_data <- pathj:::missing_pattern_plot_data(d, c("x", "y"))

  testthat::expect_equal(length(unique(plot_data$pattern)), 1)
  testthat::expect_false(any(plot_data$missing))
  testthat::expect_equal(unique(plot_data$n), 3)
})

testthat::test_that("MCAR is skipped when there are no recognised missing values", {
  d <- data.frame(x = 1:3, y = 4:6)

  result <- pathj:::mcar_diagnostic(d, c("x", "y"))

  testthat::expect_true(all(is.na(result[c("statistic", "df", "pvalue")])))
  testthat::expect_match(result$note[[1]], "No recognised missing values were detected")
  testthat::expect_match(result$note[[1]], "blank cells and user-defined missing-value codes")
})

testthat::test_that("recognised NA values are counted and MCAR can be estimated", {
  set.seed(1)
  d <- as.data.frame(matrix(rnorm(200), ncol = 4))
  names(d) <- c("x", "m", "z", "y")
  d$x[c(1, 3, 5)] <- NA
  d$m[c(2, 4, 6)] <- NA
  d$z[7] <- NA

  summary <- pathj:::missing_data_summary(d, names(d))
  result <- pathj:::mcar_diagnostic(d, names(d))

  testthat::expect_equal(sum(summary$missing), 7)
  testthat::expect_true(is.finite(result$statistic[[1]]))
  testthat::expect_gt(result$df[[1]], 0)
  testthat::expect_true(is.finite(result$pvalue[[1]]))
})

testthat::test_that("empty strings and user-defined codes are not silently recoded", {
  empty <- data.frame(x = c("", "1", "2"), y = c(1, 2, 3), z = c(4, 5, 6), stringsAsFactors = FALSE)
  coded <- data.frame(x = c(99, 1, 2), y = c(-99, 2, 3), z = c(4, 5, 6))

  empty_result <- pathj:::mcar_diagnostic(empty, names(empty))
  coded_result <- pathj:::mcar_diagnostic(coded, names(coded))

  testthat::expect_equal(sum(pathj:::missing_data_summary(empty, names(empty))$missing), 0)
  testthat::expect_equal(sum(pathj:::missing_data_summary(coded, names(coded))$missing), 0)
  testthat::expect_match(empty_result$note[[1]], "correctly defined as missing in jamovi")
  testthat::expect_match(coded_result$note[[1]], "99, -99")
})

testthat::test_that("invalid MCAR results are suppressed", {
  d <- data.frame(x = c(NA, 1, 2), y = c(NA, 2, 3))

  result <- pathj:::mcar_diagnostic(d, names(d))

  testthat::expect_true(all(is.na(result[c("statistic", "df", "pvalue")])))
  testthat::expect_match(result$note[[1]], "could not be estimated")
})
