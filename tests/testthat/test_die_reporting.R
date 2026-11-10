testthat::test_that("DIE item 1: two-group index reuses each fitted contrast", {
  m <- die_result()
  pe <- lavaan::parameterEstimates(m$model)
  die <- pe[pe$op==":=" & grepl("^DIE",pe$lhs),]
  index <- m$intelligent$moderatedIndex$asDF
  testthat::expect_equal(nrow(index),8L)
  testthat::expect_equal(index$index,die$est)
  testthat::expect_equal(index$se,die$se)
  testthat::expect_equal(index$ci.lower,die$ci.lower)
  testthat::expect_match(index$route[7],"night_phone.*sleep_quality.*exam_score")
  testthat::expect_match(index$note[1],"first group minus second group",fixed=TRUE)
})
