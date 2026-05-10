context("sem syntax import")

testthat::test_that("Mplus-style syntax imports to unified graph and lavaan", {
  syntax <- "
  VARIABLE:
    CATEGORICAL ARE depression;
    GROUPING IS gender (1=women 2=men);
  MODEL:
    wellbeing BY wb1 wb2 wb3;
    depression ON stress anxiety;
    stress WITH anxiety;
  MODEL CONSTRAINT:
    indirect = a*b;
  "
  imported <- pathj:::sem_import_parse(syntax, "mplus")
  testthat::expect_true("wellbeing =~ wb1 + wb2 + wb3" %in% imported$exports$lavaan)
  testthat::expect_true("depression ~ stress + anxiety" %in% imported$exports$lavaan)
  testthat::expect_true("stress ~~ anxiety" %in% imported$exports$lavaan)
  testthat::expect_equal(imported$graph$metadata$categorical, "depression")
  testthat::expect_true("gender" %in% names(imported$graph$metadata$groups))
  testthat::expect_true(any(imported$graph$edges$type == "loading"))
})

testthat::test_that("OpenMx RAM syntax imports without evaluating R code", {
  syntax <- '
    mxModel("example",
      manifestVars=c("stress","anxiety","depression","wb1","wb2","wb3"),
      latentVars=c("wellbeing"))
    mxPath(from="stress", to="depression", arrows=1, labels="a")
    mxPath(from="anxiety", to="depression", arrows=1, free=FALSE, values=.5)
    mxPath(from="wellbeing", to=c("wb1","wb2","wb3"), arrows=1)
    mxPath(from="stress", to="anxiety", arrows=2)
  '
  imported <- pathj:::sem_import_parse(syntax, "openmx")
  testthat::expect_true("wellbeing =~ wb1 + wb2 + wb3" %in% imported$exports$lavaan)
  testthat::expect_true("depression ~ a*stress + 0.5*anxiety" %in% imported$exports$lavaan)
  testthat::expect_true("anxiety ~~ stress" %in% imported$exports$lavaan)
  testthat::expect_true("wellbeing" %in% imported$graph$metadata$latent_variables)
})

testthat::test_that("Mermaid round trips through graph schema", {
  syntax <- "flowchart LR\nStress[Stress] -->|a| Sleep[Sleep]\nSleep --> Wellbeing\nStress <--> Anxiety"
  imported <- pathj:::sem_import_parse(syntax, "mermaid")
  testthat::expect_true("Sleep ~ a*Stress" %in% imported$exports$lavaan)
  testthat::expect_true("Stress ~~ Anxiety" %in% imported$exports$lavaan)
  testthat::expect_true(any(grepl("-->", imported$exports$mermaid, fixed=TRUE)))
})

testthat::test_that("malformed OpenMx paths return line-level parser errors", {
  imported <- pathj:::sem_import_parse('mxPath(to="depression", arrows=1)', "openmx")
  testthat::expect_true(length(imported$errors) > 0)
  testthat::expect_match(imported$errors[[1]]$message, "missing")
})

testthat::test_that("multilevel and growth syntax are preserved for lavaan mapping", {
  syntax <- "
  MODEL:
  %WITHIN%
    y ON x;
  %BETWEEN%
    y ON z;
    i s | y1@0 y2@1 y3@2;
  "
  imported <- pathj:::sem_import_parse(syntax, "mplus")
  testthat::expect_true(any(grepl("^level: 1", imported$exports$lavaan)))
  testthat::expect_true(any(grepl("^level: 2", imported$exports$lavaan)))
  testthat::expect_true(any(grepl("i =~", imported$exports$lavaan)))
  testthat::expect_true(any(imported$engine_support$status == "partial"))
})
