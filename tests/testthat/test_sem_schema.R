context("engine agnostic SEM schema")

testthat::test_that("schema constructor normalises nodes, edges, and metadata", {
  schema <- sem_schema_new(
    nodes=data.frame(id=c("stress", "depression"), type=c("observed", "observed")),
    edges=data.frame(from="stress", to="depression", type="regression", operator="~"),
    metadata=list(source="test")
  )

  testthat::expect_s3_class(schema, "pathj_sem_schema")
  testthat::expect_equal(schema$nodes$label, c("stress", "depression"))
  testthat::expect_equal(schema$nodes$outcome_type, c("unknown", "unknown"))
  testthat::expect_equal(schema$metadata$source, "test")
})

testthat::test_that("schema validator catches missing edge endpoints", {
  schema <- sem_schema_new(
    nodes=data.frame(id="stress", type="observed"),
    edges=data.frame(from="stress", to="depression", type="regression", operator="~")
  )

  validation <- sem_schema_validate(schema)
  testthat::expect_false(validation$valid)
  testthat::expect_match(validation$errors[[1]], "missing nodes")
})

testthat::test_that("schema validator warns for future-engine outcome types", {
  schema <- sem_schema_new(
    nodes=data.frame(id="visits", type="observed", outcome_type="count"),
    edges=data.frame(from="visits", to="visits", type="variance", operator="~~")
  )

  validation <- sem_schema_validate(schema, engine="lavaan")
  testthat::expect_true(validation$valid)
  testthat::expect_true(any(grepl("Count", validation$warnings)))
})

testthat::test_that("syntax imports return validated schema graphs", {
  imported <- sem_import_parse("depression ON stress anxiety;", "mplus")

  testthat::expect_s3_class(imported$graph, "pathj_sem_schema")
  testthat::expect_true(imported$graph$metadata$validation$valid)
  testthat::expect_equal(imported$graph$metadata$endogenous, "depression")
  testthat::expect_equal(imported$graph$metadata$exogenous, c("stress", "anxiety"))
})

testthat::test_that("capability map records planned backend phases", {
  caps <- sem_schema_capability_map()

  testthat::expect_true(all(c("lavaan", "OpenMx", "brms", "Stan", "blavaan") %in% caps$engine))
  testthat::expect_equal(caps$status[caps$engine == "lavaan"], "primary")
})
