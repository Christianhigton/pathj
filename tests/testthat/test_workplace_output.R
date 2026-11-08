testthat::test_that("item 1: defined descriptions follow labels through double digits", {
  tab <- workplace_result("B")$models$defined$asDF
  testthat::expect_true(all(c("IE11", "IE12", "TE8") %in% tab$lhs))
  testthat::expect_false(any(grepl("^IE[0-9]+$", tab$desc)))
  testthat::expect_match(tab$desc[tab$lhs == "IE11"], "Training|₂")
  testthat::expect_match(tab$desc[tab$lhs == "TE1"], "⇒")
  testthat::expect_match(tab$desc[tab$lhs == "IE11"], "resilience ⇒ wellbeing ⇒ support_person ⇒ stress ⇒ sleep_quality", fixed=TRUE)
  testthat::expect_match(tab$desc[tab$lhs == "TE1"], "wellbeing ⇒ sleep_quality", fixed=TRUE)
})

testthat::test_that("item 2: group contributions sum to the user chi-square", {
  tab <- workplace_result("A")$fit$main$asDF
  groups <- tab[grepl("^Group: ", tab$label), ]
  testthat::expect_equal(nrow(groups), 2L)
  testthat::expect_equal(sum(groups$chisq), tab$chisq[1], tolerance=1e-7)
  testthat::expect_true(all(is.na(groups$df)))
  testthat::expect_true(all(is.na(groups$pvalue)))
})

testthat::test_that("item 3: Fisher R2 intervals include near-zero estimates", {
  tab <- workplace_result("B")$models$r2$asDF
  testthat::expect_true(all(tab$ci.lower <= tab$r2 & tab$r2 <= tab$ci.upper))
  tiny <- tab[tab$lgroup == "Control" & tab$lhs == "wellbeing", ]
  testthat::expect_lt(tiny$r2, .0001)
  testthat::expect_equal(tiny$ci.lower, 0)
})

testthat::test_that("item 4: indirect narrative pairs B with its confidence interval", {
  d <- read.csv(testthat::test_path("..", "..", "data", "workplace_wellbeing.csv"))
  fit <- run_model("stress ~ a*workload\nsleep_quality ~ b*stress\nIE1 := a*b", d, estimator="ML")
  out <- report_mediation(fit)
  r <- out$table[1, ]
  expected <- paste0("B = ", apa_num(r$indirect), ", 95% CI [", apa_num(r$ci.lower), ", ", apa_num(r$ci.upper), "], beta = ", apa_num(r$beta))
  testthat::expect_match(out$text, expected, fixed=TRUE)
})

testthat::test_that("item 5: missing summary contains data variables only", {
  d <- read.csv(testthat::test_path("..", "..", "data", "workplace_wellbeing.csv"))
  tab <- workplace_result("B")$diagnostics$missingSummary$asDF
  expected <- c("sleep_quality", "stress", "support_person", "wellbeing", "resilience", "workload", "condition")
  testthat::expect_setequal(as.character(tab$variable), expected)
  pt <- lavaan::lavaanify("y ~ a*x\nIE1 := a*a")
  testthat::expect_setequal(model_variable_names(pt), c("x", "y"))
})
