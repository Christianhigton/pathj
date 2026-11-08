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

testthat::test_that("item 6: covariance exports are unique and IE labels match fitted paths", {
  m <- workplace_result("B")
  for (nm in c("lavaanSyntax", "mermaidSyntax", "mplusSyntax", "openmxSyntax")) {
    code <- as.character(m$intelligent[[nm]]$asDF$code)
    testthat::expect_equal(anyDuplicated(code[nzchar(code)]), 0L, info=nm)
  }
  # Exports are assembled after fitting: check the group-2 IE against the fit.
  fit <- m$model
  pt <- lavaan::parameterTable(fit)
  pe <- lavaan::parameterEstimates(fit)
  for (name in paste0("IE", 7:12)) {
    def <- pt[pt$op == ":=" & pt$lhs == name, ]
    labs <- strsplit(def$rhs, "*", fixed=TRUE)[[1]]
    paths <- pe[pe$label %in% labs & pe$op == "~", ]
    testthat::expect_true(all(paths$group == 2L))
    testthat::expect_equal(prod(paths$est), pe$est[pe$lhs == name & pe$op == ":="], tolerance=1e-8)
  }
})

testthat::test_that("item 7: inapplicable mediated proportions remain blank", {
  a <- workplace_result("A")$intelligent$mediationDecomp$asDF
  testthat::expect_true(all(is.na(a$direct)))
  testthat::expect_true(all(is.na(a$percent_mediated)))
  testthat::expect_true(is.na(mediation_proportion(.2, -.1, .1)$value))
  testthat::expect_true(is.na(mediation_proportion(.2, -.2, 1e-10)$value))
  testthat::expect_equal(mediation_proportion(.2, .3, .5)$value, 40)
})

testthat::test_that("item 8: single-model reports omit model selection", {
  m <- workplace_result("A")
  testthat::expect_false("Model Selection" %in% m$intelligent$apaText$asDF$section)
  testthat::expect_equal(nrow(m$intelligent$modelComparison$asDF), 0L)
})

testthat::test_that("item 9: direct effects and insights identify each group", {
  m <- workplace_result("A")
  for (section in c("Direct Effects", "Model Insights")) {
    text <- m$intelligent$apaText$asDF$text[m$intelligent$apaText$asDF$section == section]
    testthat::expect_match(text, "In Control,", fixed=TRUE)
    testthat::expect_match(text, "In Training,", fixed=TRUE)
  }
  testthat::expect_setequal(as.character(m$intelligent$insights$asDF$lgroup), c("Control", "Training"))
})

testthat::test_that("item 10: p-value screen makes no evidential claim", {
  text <- p_curve_analysis(workplace_result("A")$model)$text
  testthat::expect_false(grepl("evidential value", text, fixed=TRUE))
  testthat::expect_match(text, "dependent|independent")
  testthat::expect_match(text, "descriptive", ignore.case=TRUE)
})

testthat::test_that("item 11: multigroup comparison exposes B and text significance", {
  tab <- workplace_result("B")$intelligent$groupComparison$asDF
  testthat::expect_true(all(c("group_1_est", "group_2_est") %in% names(tab)))
  testthat::expect_equal(tab$difference, tab$group_1_est - tab$group_2_est)
  testthat::expect_identical(tab$significant, ifelse(compare_groups(workplace_result("B")$model)$table$pvalue < .05, "TRUE", "FALSE"))
})

testthat::test_that("item 12: model-info notes keep variable names on one line", {
  notes <- vapply(workplace_result("B")$info$notes, function(note) note$note, character(1))
  text <- paste(notes, collapse=" ")
  testthat::expect_match(text, "exogenous variable")
  testthat::expect_match(text, "resilience")
  testthat::expect_false(grepl("[\r\n]", text))
})

testthat::test_that("item 13: narrative and displayed p values use APA notation", {
  testthat::expect_identical(apa_p(.425), "p = .425")
  testthat::expect_identical(apa_p(0), "p < .001")
  testthat::expect_identical(apa_p(.001), "p = .001")
  tab <- workplace_result("B")$models$coefficients$asDF
  testthat::expect_true(all(grepl("^(< \\.001|\\.[0-9]{3}|1\\.000)$", tab$pvalue)))
  testthat::expect_false(any(grepl("p = 0\\.", workplace_result("B")$intelligent$apaText$asDF$text)))
})

testthat::test_that("item 14: Ordered is explicit text for jamovi cells", {
  tab <- workplace_result("B")$intelligent$variableTypes$asDF
  testthat::expect_identical(tab$ordered, rep("FALSE", nrow(tab)))
  # Type detection itself stays logical for estimator selection.
  types <- detect_variable_types(data.frame(binary=rep(0:1,5),continuous=seq_len(10)))
  testthat::expect_identical(types$ordered, c(TRUE,FALSE))
})

testthat::test_that("item 15: wrapped diagram labels retain all name characters", {
  names <- c("support_person", "sleep_quality", "a_very_long_variable_name", "averylongunbrokenvariablename")
  labels <- diagram_node_labels(names)
  testthat::expect_equal(gsub("[ _\n]", "", labels), gsub("_", "", names))
  testthat::expect_true(all(nchar(unlist(strsplit(labels,"\n",fixed=TRUE))) <= 12))
})

testthat::test_that("item 16: MI suggestions exclude exogenous outcomes and group alternatives", {
  m <- workplace_result("A")
  tab <- m$diagnostics$modindices$asDF
  exogenous <- c("support_person", "resilience", "workload")
  testthat::expect_false(any(tab$op == "~" & tab$lhs %in% exogenous))
  pairs <- vapply(seq_len(nrow(tab)), function(i) paste(sort(c(tab$lhs[i],tab$rhs[i])),collapse="~"), character(1))
  testthat::expect_equal(anyDuplicated(paste(tab$lgroup,pairs)), 0L)
  testthat::expect_true("alternatives" %in% names(tab))
})

testthat::test_that("MI alternatives retain the representative statistic and group", {
  mi <- data.frame(lhs=c("stress","wellbeing","support_person","wellbeing"),
    op=c("~","~~","~","~~"), rhs=c("wellbeing","stress","resilience","stress"),
    group=c(1,1,1,2), mi=c(11,15,50,17), epc=c(.1,.2,.3,.4))
  paths <- data.frame(lhs=c("stress","stress"),op="~",rhs=c("support_person","resilience"))
  out <- screen_modification_indices(mi,paths)
  testthat::expect_equal(sort(out$mi),c(15,17))
  testthat::expect_equal(out$epc[out$group==1],.2)
  testthat::expect_match(out$alternatives[out$group==1], "stress ~ wellbeing; wellbeing ~~ stress",fixed=TRUE)
})

testthat::test_that("item 17: group indirect differences have lavaan delta intervals", {
  m <- workplace_result("B")
  pe <- lavaan::parameterEstimates(m$model,ci=TRUE)
  contrasts <- pe[grepl("^DIE[0-9]+$",pe$lhs) & pe$op == ":=", ]
  testthat::expect_equal(nrow(contrasts), 6L)
  for (i in seq_len(nrow(contrasts))) {
    labels <- strsplit(contrasts$rhs[i],"-",fixed=TRUE)[[1]]
    first <- pe[pe$op == ":=" & pe$lhs == labels[1], ]
    second <- pe[pe$op == ":=" & pe$lhs == labels[2], ]
    testthat::expect_equal(contrasts$est[i],first$est-second$est,tolerance=1e-8)
    # These unconstrained groups are independent; this supplies an external
    # check of lavaan's delta-method contrast variance, including its CI.
    se <- sqrt(first$se^2+second$se^2)
    testthat::expect_equal(contrasts$se[i],se,tolerance=1e-6)
    testthat::expect_equal(contrasts$ci.lower[i],contrasts$est[i]-qnorm(.975)*se,tolerance=1e-6)
    testthat::expect_equal(contrasts$ci.upper[i],contrasts$est[i]+qnorm(.975)*se,tolerance=1e-6)
  }
  notes <- vapply(m$intelligent$conditionalEffects$notes,function(note) note$note,character(1))
  testthat::expect_match(paste(notes,collapse=" "),"moderator")
  testthat::expect_match(paste(notes,collapse=" "),"DIE")
})
