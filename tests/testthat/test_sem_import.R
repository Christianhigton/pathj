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

testthat::test_that("imported syntax is reported as the estimated lavaan model", {
  data("pathjdata")
  mod <- pathj::pathj(
    data=pathjdata,
    syntaxSource="lavaan",
    syntaxText="y1 ~ y2 + x1\ny2 ~ x2",
    syntaxApply=TRUE,
    syntaxVars=c("y1", "y2", "x1", "x2"),
    showSyntax=TRUE)

  info <- mod$info$asDF
  syntax <- mod$intelligent$lavaanSyntax$asDF

  testthat::expect_true("Estimated lavaan syntax" %in% info$info)
  testthat::expect_equal(as.integer(info$value[info$info %in% "Imported lavaan statements"]), 2)
  testthat::expect_equal(info$value[info$info %in% "Estimated lavaan syntax"], "y1 ~ y2 + x1 ; y2 ~ x2")
  testthat::expect_true("y1 ~ y2 + x1" %in% syntax$code)
  testthat::expect_true("y2 ~ x2" %in% syntax$code)
})

testthat::test_that("parallel mediation reports each indirect pathway", {
  set.seed(1)
  d <- data.frame(x=rnorm(200))
  d$m1 <- .5*d$x + rnorm(200)
  d$m2 <- .4*d$x + rnorm(200)
  d$y <- .3*d$m1 + .25*d$m2 + .2*d$x + rnorm(200)

  mod <- pathj::pathj(
    data=d,
    formula=list("m1 ~ x", "m2 ~ x", "y ~ m1 + m2 + x"),
    indirect=TRUE)

  effects <- mod$models$effects$asDF
  decomp <- mod$intelligent$mediationDecomp$asDF

  testthat::expect_true("x \U21d2 m1 \U21d2 y" %in% effects$pathway)
  testthat::expect_true("x \U21d2 m2 \U21d2 y" %in% effects$pathway)
  testthat::expect_true("m1" %in% decomp$mediator)
  testthat::expect_true("m2" %in% decomp$mediator)
})

testthat::test_that("Mermaid import fills intelligent-report tables without lgroup crashes", {
  set.seed(1)
  d <- data.frame(
    ACE_total=rnorm(121),
    DERS_total=rnorm(121),
    BIS_total=rnorm(121),
    AUDIT_total=rnorm(121)
  )
  syntax <- paste(
    "flowchart LR",
    "ACE_total --> DERS_total",
    "ACE_total --> BIS_total",
    "DERS_total --> AUDIT_total",
    "BIS_total --> AUDIT_total",
    "DERS_total <--> BIS_total",
    sep="\n"
  )

  mod <- pathj::pathj(
    data=d,
    syntaxSource="mermaid",
    syntaxText=syntax,
    syntaxApply=TRUE,
    syntaxVars=names(d),
    showSyntax=TRUE,
    intelligentReport=TRUE)

  testthat::expect_true(nrow(mod$models$coefficients$asDF) >= 4)
  testthat::expect_true("Estimated lavaan syntax" %in% mod$info$asDF$info)
  testthat::expect_true("Model Fit" %in% mod$intelligent$apaText$asDF$section)
  testthat::expect_true(nrow(mod$intelligent$assumptions$asDF) > 0)
  testthat::expect_true(nrow(mod$intelligent$recommendations$asDF) > 0)
  testthat::expect_false(any(is.na(mod$intelligent$recommendations$asDF$recommendation)))
})

testthat::test_that("Mermaid import diagnostics tolerate empty modification-index filters", {
  set.seed(1)
  d <- data.frame(
    ACE_total=rnorm(121),
    DERS_total=rnorm(121),
    BIS_total=rnorm(121),
    AUDIT_total=rnorm(121)
  )
  syntax <- paste(
    "flowchart LR",
    "ACE_total --> DERS_total",
    "ACE_total --> BIS_total",
    "DERS_total --> AUDIT_total",
    "BIS_total --> AUDIT_total",
    "DERS_total <--> BIS_total",
    sep="\n"
  )

  options <- pathj:::pathjOptions$new(
    syntaxSource="mermaid",
    syntaxText=syntax,
    syntaxApply=TRUE,
    syntaxVars=names(d),
    modindices=TRUE,
    miMin=1e9)
  mod <- pathj:::pathjClass$new(options=options, data=d)
  mod$run()

  testthat::expect_equal(nrow(mod$results$diagnostics$modindices$asDF), 0)
  testthat::expect_true(nrow(mod$results$models$coefficients$asDF) >= 4)
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
