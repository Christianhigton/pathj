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
