testthat::test_that("item 1: defined descriptions follow labels through double digits", {
  tab <- workplace_result("B")$models$defined$asDF
  testthat::expect_true(all(c("IE11", "IE12", "TE8") %in% tab$lhs))
  testthat::expect_false(any(grepl("^IE[0-9]+$", tab$desc)))
  testthat::expect_match(tab$desc[tab$lhs == "IE11"], "Training|₂")
  testthat::expect_match(tab$desc[tab$lhs == "TE1"], "⇒")
  testthat::expect_match(tab$desc[tab$lhs == "IE11"], "resilience ⇒ wellbeing ⇒ support_person ⇒ stress ⇒ sleep_quality", fixed=TRUE)
  testthat::expect_match(tab$desc[tab$lhs == "TE1"], "wellbeing ⇒ sleep_quality", fixed=TRUE)
})
