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

testthat::test_that("DIE item 2: DTEs match totals and preserve existing effects", {
  m <- die_result()
  pe <- lavaan::parameterEstimates(m$model)
  dte <- pe[pe$op==":=" & grepl("^DTE",pe$lhs),]
  testthat::expect_equal(nrow(dte),4L)
  testthat::expect_equal(dte$rhs[3],"TE3-TE7")
  for (i in seq_len(nrow(dte))) {
    labs <- strsplit(dte$rhs[i],"-",fixed=TRUE)[[1]]
    rows <- pe[match(labs,pe$lhs),]
    testthat::expect_equal(dte$est[i],rows$est[1]-rows$est[2])
    testthat::expect_equal(dte$se[i],sqrt(sum(rows$se^2)),tolerance=1e-6)
  }
  defs <- m$models$defined$asDF
  testthat::expect_true(min(which(grepl("^DTE",defs$lhs))) > max(which(grepl("^DIE",defs$lhs))))
})
