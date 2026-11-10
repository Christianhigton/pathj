die_result <- local({
  cache <- new.env(parent=emptyenv())
  function(groups=2L, equal=FALSE) {
    key <- paste(groups,equal)
    if (!exists(key,cache,inherits=FALSE)) {
      set.seed(812)
      n <- 210L*groups
      d <- data.frame(condition=rep(c("Control","Digital curfew","Third")[seq_len(groups)],each=210),
                      fomo=rnorm(n),loneliness=rnorm(n))
      d$night_phone <- .5*d$fomo+.2*d$loneliness+rnorm(n)
      d$sleep_quality <- ifelse(d$condition=="Control",-.6,-.2)*d$night_phone+rnorm(n)
      d$exam_anxiety <- -.4*d$sleep_quality+.2*d$loneliness+rnorm(n)
      d$exam_score <- -.3*d$exam_anxiety+.5*d$sleep_quality+rnorm(n)
      cache[[key]] <- pathj::pathj(data=d, formula=list(
        "night_phone ~ fomo + loneliness", "sleep_quality ~ night_phone",
        "exam_anxiety ~ sleep_quality + loneliness", "exam_score ~ exam_anxiety + sleep_quality"),
        multigroup="condition", missing="fiml", indirect=TRUE, intelligentReport=TRUE,
        group.equal=if(equal) "regressions" else NULL)
    }
    cache[[key]]
  }
})
