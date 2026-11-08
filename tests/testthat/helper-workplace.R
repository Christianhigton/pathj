# Cache the two reported GUI configurations for integration checks.
workplace_result <- local({
  cache <- new.env(parent = emptyenv())
  function(model = "A") {
    if (!exists(model, cache, inherits = FALSE)) {
      d <- read.csv(testthat::test_path("..", "..", "data", "workplace_wellbeing.csv"))
      forms <- if (model == "A") list(
        "stress ~ support_person + resilience + workload", "sleep_quality ~ stress",
        "wellbeing ~ sleep_quality", "productivity ~ wellbeing") else list(
        "sleep_quality ~ stress + wellbeing", "stress ~ support_person + resilience + workload",
        "support_person ~ wellbeing", "wellbeing ~ resilience")
      vc <- if (model == "B") list(c("stress", "wellbeing"), c("resilience", "support_person")) else NULL
      cache[[model]] <- pathj::pathj(data=d, formula=forms, varcov=vc,
        multigroup="condition", missing="fiml", indirect=TRUE,
        modindices=TRUE, intelligentReport=TRUE)
    }
    cache[[model]]
  }
})
