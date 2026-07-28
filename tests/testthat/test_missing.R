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
