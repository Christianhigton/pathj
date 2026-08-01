testthat::context("mediation topology and conditional process")

make_topology_fit <- function(model, n = 250) {
  set.seed(42)
  d <- data.frame(x = rnorm(n), m1 = rnorm(n), m2 = rnorm(n), y = rnorm(n), w = rnorm(n))
  d$x__XX__XX__w <- d$x * d$w
  d$m1__XX__XX__w <- d$m1 * d$w
  pathj:::run_model(model, d, estimator = "MLR", se = "standard", missing = "fiml")
}

testthat::test_that("simple, parallel, and serial mediation are classified", {
  simple <- make_topology_fit("m1 ~ a*x\ny ~ b*m1 + cp*x\nind := a*b")
  parallel <- make_topology_fit("m1 ~ a1*x\nm2 ~ a2*x\ny ~ b1*m1 + b2*m2 + cp*x\nind1 := a1*b1\nind2 := a2*b2")
  serial <- make_topology_fit("m1 ~ a*x\nm2 ~ d*m1\ny ~ b*m2 + cp*x\nind := a*d*b")

  testthat::expect_equal(pathj:::detect_mediation_topology(simple)$model_class, "simple_mediation")
  testthat::expect_equal(pathj:::detect_mediation_topology(parallel)$model_class, "parallel_mediation")
  testthat::expect_equal(pathj:::detect_mediation_topology(serial)$model_class, "serial_mediation")
})

testthat::test_that("first-stage moderated mediation is detected and probed", {
  d <- data.frame(x = rnorm(250), w = rnorm(250))
  d$x__XX__XX__w <- d$x * d$w
  d$m <- rnorm(250)
  d$y <- rnorm(250)
  fit <- pathj:::run_model(
    "m ~ a*x + aw*x__XX__XX__w\ny ~ b*m + cp*x\nind := a*b",
    d, estimator = "MLR", se = "standard", missing = "fiml")
  topology <- pathj:::detect_mediation_topology(fit)
  conditional <- pathj:::conditional_process_table(fit, topology, d)

  testthat::expect_equal(topology$model_class, "first_stage_moderated_mediation")
  testthat::expect_equal(topology$process_equivalent, "Model 7")
  testthat::expect_equal(nrow(conditional), 3)
  testthat::expect_true(all(is.finite(conditional$indirect)))
})

testthat::test_that("unrelated interactions are not called mediated moderation", {
  fit <- make_topology_fit("m1 ~ a*x\ny ~ b*m1 + cp*x + dw*x__XX__XX__w\nind := a*b")
  topology <- pathj:::detect_mediation_topology(fit)

  testthat::expect_equal(topology$model_class, "moderated_direct_effect_with_mediation")
  testthat::expect_false(topology$model_class %in% c("mediated_moderation", "first_stage_moderated_mediation", "second_stage_moderated_mediation"))
})
