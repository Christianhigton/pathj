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

testthat::test_that("DIE item 3: all group pairs and route omnibus tests", {
  m <- die_result(groups=3L)
  pe <- lavaan::parameterEstimates(m$model)
  testthat::expect_equal(sum(grepl("^DIE",pe$lhs)),24L)
  tab <- conditional_process_index(m$model,detect_mediation_topology(m$model))
  omnibus <- tab[is.finite(tab$wald),]
  testthat::expect_equal(nrow(omnibus),8L)
  testthat::expect_equal(omnibus$df,rep(2,8))
  independent <- lavaan::lavTestWald(m$model,"IE1 == IE9; IE1 == IE17")
  testthat::expect_equal(omnibus$wald[1],independent$stat)
  testthat::expect_equal(omnibus$pvalue[1],independent$p.value)
})

testthat::test_that("DIE item 4: labels and direction note name actual groups", {
  m <- die_result()
  tab <- m$models$defined$asDF
  testthat::expect_equal(tab$desc[tab$lhs=="DIE7"],
    "Δ indirect (B): night_phone ⇒ sleep_quality ⇒ exam_score (Control − Digital curfew)")
  notes <- paste(vapply(m$models$defined$notes,function(x)x$note,character(1)),collapse=" ")
  testthat::expect_match(notes,effect_difference_note(n_die=8L),fixed=TRUE)
})

testthat::test_that("DIE item 5: differences omit beta while IE beta is unchanged", {
  tab <- die_result()$models$defined$asDF
  testthat::expect_true(all(is.na(tab$std.all[grepl("^D(IE|TE)",tab$lhs)])))
  pe <- lavaan::parameterEstimates(die_result()$model,standardized=TRUE)
  testthat::expect_equal(tab$std.all[tab$lhs=="IE7"],pe$std.all[pe$lhs=="IE7"])
  testthat::expect_match(effect_difference_note(),"β not reported for differences",fixed=TRUE)
})

testthat::test_that("DIE item 6: bootstrap selections propagate to the index", {
  set.seed(914)
  d <- data.frame(x=rnorm(160),g=rep(c("A","B"),each=80))
  d$m <- .5*d$x+rnorm(160); d$y <- .6*d$m+rnorm(160)
  fit <- suppressWarnings(lavaan::sem(
    "m ~ c(a1,a2)*x; y ~ c(b1,b2)*m; IE1 := a1*b1; IE2 := a2*b2; DIE1 := IE1-IE2; TE1 := IE1; TE2 := IE2; DTE1 := TE1-TE2",
    data=d,group="g",se="bootstrap",bootstrap=80))
  for (type in c("perc","bca.simple","norm","basic")) {
    attr(fit,"pathj_bootci") <- type
    attr(fit,"pathj_ci_level") <- .90
    expected <- suppressWarnings(lavaan::parameterEstimates(fit,boot.ci.type=type,level=.90))
    actual <- suppressWarnings(conditional_process_index(fit,detect_mediation_topology(fit)))
    testthat::expect_equal(actual$ci.lower,expected$ci.lower[expected$lhs=="DIE1"])
    testthat::expect_equal(actual$ci.upper,expected$ci.upper[expected$lhs=="DIE1"])
    total <- suppressWarnings(multigroup_effect_differences(fit,prefix="DTE"))
    testthat::expect_equal(total$ci.lower,expected$ci.lower[expected$lhs=="DTE1"])
    testthat::expect_match(effect_difference_note("boot",type),paste0("bootstrap (",type,")"),fixed=TRUE)
  }
  testthat::expect_match(effect_difference_note(),"delta method")
  testthat::expect_match(effect_difference_note(),"bootstrap CIs are recommended")
})


testthat::test_that("DIE item 7: caution starts above three contrasts", {
  testthat::expect_false(grepl("Several differences",effect_difference_note(n_die=3L)))
  testthat::expect_match(effect_difference_note(n_die=4L),
    "Several differences are tested; interpret unpredicted differences with caution.",fixed=TRUE)
})

testthat::test_that("DIE item 8: narrative reports CI-supported DIEs and all DTEs", {
  fit <- die_result()$model
  text <- report_mediation(fit)$text
  die <- multigroup_effect_differences(fit)
  supported <- die$ci.lower > 0 | die$ci.upper < 0
  testthat::expect_match(text,"Moderated mediation:",fixed=TRUE)
  for (i in which(supported)) {
    testthat::expect_match(text,paste0(die$lhs[i]," ("),fixed=TRUE)
    testthat::expect_match(text,paste0("B = ",apa_num(die$est[i]),", 95% CI [",
      apa_num(die$ci.lower[i]),", ",apa_num(die$ci.upper[i]),"], ",apa_p(die$pvalue[i])),fixed=TRUE)
  }
  testthat::expect_match(text,paste0(sum(!supported)," of 8 indirect-effect differences"),fixed=TRUE)
  testthat::expect_match(text,"DTE3 (",fixed=TRUE)
  altered <- report_parameters(fit)
  changed <- grepl("^DIE",altered$lhs)
  altered$ci.lower[changed] <- -1; altered$ci.upper[changed] <- 1
  altered$pvalue[changed] <- .001
  testthat::expect_match(report_mediation(fit,altered)$text,"8 of 8 indirect-effect differences",fixed=TRUE)
})

testthat::test_that("DIE item 9: equal paths suppress degenerate differences", {
  m <- die_result(equal=TRUE)
  testthat::expect_true(regressions_constrained_equal(m$model))
  testthat::expect_false(regressions_constrained_equal(die_result()$model))
  f <- m$model; attr(f,"pathj_equal_regressions") <- NULL
  testthat::expect_true(regressions_constrained_equal(f))
  tab <- m$models$defined$asDF
  testthat::expect_false(any(grepl("^D(IE|TE)",tab$lhs)))
  testthat::expect_equal(sum(grepl("^IE",tab$lhs)),16L)
  index <- m$intelligent$moderatedIndex$asDF
  testthat::expect_true(all(is.na(index$index)))
  testthat::expect_equal(index$note,constrained_difference_note())
  testthat::expect_false(grepl("Moderated mediation:",report_mediation(m$model)$text,fixed=TRUE))
})

testthat::test_that("DIE item 10: constrained comparisons are hidden and tiny display cells are zero", {
  m <- die_result(equal=TRUE)
  testthat::expect_equal(nrow(m$intelligent$groupComparison$asDF),0L)
  testthat::expect_equal(nrow(compare_groups(m$model)$table),0L)
  raw <- data.frame(difference=c(1e-16,-1e-16,.2),z=c(8.43e-8,-8.43e-8,2),pvalue=c(.001,.1,.03))
  displayed <- j.comparison_display(raw)
  testthat::expect_equal(displayed$difference,c(0,0,.2))
  testthat::expect_equal(displayed$z,c(0,0,2))
  testthat::expect_equal(displayed$pvalue,raw$pvalue)
  testthat::expect_equal(raw$z[1],8.43e-8)
})
