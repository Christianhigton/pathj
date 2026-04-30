
readiness <- function(options) {
  result <- list(reason = NULL, ready = TRUE, report = FALSE)

  syntax_source <- options$syntaxSource
  if (is.null(syntax_source))
    syntax_source <- "gui"
  syntax_lines <- options$syntaxText
  syntax_lines <- trimws(syntax_lines)
  syntax_lines <- syntax_lines[nzchar(syntax_lines)]
  if (!identical(syntax_source, "gui")) {
    if (!isTRUE(options$syntaxApply)) {
      result$ready <- FALSE
      result$report <- TRUE
      result$reason <- glue::glue("press Import syntax after pasting {syntax_source} syntax, or switch Model input back to GUI builder")
      return(result)
    }
    if (length(syntax_lines) > 0 && length(options$syntaxVars) > 0)
      return(result)
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("paste {syntax_source} syntax and select the variables used in the imported syntax, or switch Model input back to GUI builder")
    return(result)
  }

  if(length(options$endogenous) == 0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("we need at least 1 endogenous variable")
    return(result)
  } 

  if((length(options$factors) == 0) & (length(options$covs) == 0)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("we need at least 1 exogenous variable")
    return(result)
  } 
  
  check<-sum(unlist(sapply(options$endogenousTerms, function(l) as.numeric(length(l)>0))))
  if (check< length(options$endogenous)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("Predictors not specified for {length(options$endogenous)-check} endogenous variable")
    return(result)
  }

  check<-length(unlist(options$varcov))

  return(result)
}
