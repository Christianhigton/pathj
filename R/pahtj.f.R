#' Path Analysis
#'
#' Path Analysis
#' @param data the data as a data frame
#' @param endogenous a vector of strings naming the mediators from \code{data}
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}
#' @param covs a vector of strings naming the covariates from \code{data}
#' @param multigroup factor defining groups for multigroup analysis
#' @param clusterVariable optional cluster or participant identifier used for
#'   multilevel diagnostics, ICC screening, and within-/between-person
#'   interpretation.
#' @param withinVariables variables interpreted at the within-person or level-1
#'   occasion level when \code{clusterVariable} is supplied.
#' @param betweenVariables variables interpreted at the between-person or level-2
#'   cluster level when \code{clusterVariable} is supplied.
#' @param se .
#' @param r2ci Choose the confidence interval type
#' @param r2test .
#' @param bootci Choose the confidence interval type
#' @param ci .
#' @param ciWidth a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the parameter estimates
#' @param bootN number of bootstrap samples for estimating confidence
#'   intervals
#' @param showintercepts \code{TRUE} or \code{FALSE} (default), show
#'   intercepts
#' @param intercepts \code{TRUE} or \code{FALSE} (default), show intercepts
#' @param indirect \code{TRUE} or \code{FALSE} (default), show intercepts
#' @param contrasts a list of lists specifying the factor and type of contrast
#'   to use, one of \code{'deviation'}, \code{'simple'}, \code{'difference'},
#'   \code{'helmert'}, \code{'repeated'} or \code{'polynomial'}
#' @param showRealNames \code{TRUE} or \code{FALSE} (default), provide raw
#'   names of the contrasts variables
#' @param showContrastCode \code{TRUE} or \code{FALSE} (default), provide
#'   contrast coefficients tables
#' @param scaling a named vector of the form \code{c(var1='type',
#'   var2='type2')} specifying the transformation to apply to covariates, one of
#'   \code{'centered'} to the mean, \code{'standardized'},\code{'log'} or
#'   \code{'none'}. \code{'none'} leaves the variable as it is.
#' @param endogenousTerms a list of lists specifying the models for with the
#'   mediators as dependent variables.
#' @param diagram \code{TRUE} or \code{FALSE} (default), produce a path
#'   diagram
#' @param diag_paths Choose the diagram labels
#' @param diag_sigstars \code{TRUE} or \code{FALSE} (default), append
#'   significance stars to regression-path labels in path diagrams.
#' @param diag_resid \code{TRUE} or \code{FALSE} (default), produce a path
#'   diagram
#' @param diag_labsize Choose the diagram labels
#' @param diag_rotate Choose the diagram labels
#' @param diag_type Choose the diagram labels
#' @param diag_shape Choose the diagram labels
#' @param diag_abbrev Choose the diagram labels
#' @param varcov a list of lists specifying the  covariances that need to be
#'   estimated
#' @param cov_y \code{TRUE} or \code{FALSE} (default), produce a path diagram
#' @param cov_x \code{TRUE} or \code{FALSE} (default), produce a path diagram
#' @param constraints a list of lists specifying the models random effects.
#' @param constraints_examples .
#' @param showlabels .
#' @param scoretest .
#' @param cumscoretest .
#' @param modindices \code{TRUE} or \code{FALSE} (default), compute and show
#'   lavaan modification indices.
#' @param miMin minimum modification index threshold for inclusion in the table.
#' @param estimator Choose the diagram labels
#' @param likelihood Choose the diagram labels
#' @param missing Missing-data handling method. \code{"fiml"} uses full
#'   information maximum likelihood, \code{"mi"} uses multiple imputation,
#'   \code{"listwise"} removes incomplete cases, and \code{"pairwise"} uses
#'   pairwise deletion with a warning.
#' @param miN number of imputed datasets when \code{missing = "mi"}.
#' @param miSeed random seed used by \code{mice} when \code{missing = "mi"}.
#' @param miStrategy multigroup imputation strategy when \code{missing = "mi"}.
#'   \code{"include_group"} keeps the group variable in one imputation model;
#'   \code{"within_group"} imputes separately within each group.
#'   When the optional \code{lavaan.mi} package is available, formal pooled MI
#'   fit measures are also reported.
#' @param showMissingDiagnostics \code{TRUE} or \code{FALSE} (default), show
#'   missing-data summaries, patterns, and an MCAR diagnostic when available.
#' @param intelligentReport \code{TRUE} or \code{FALSE} (default TRUE), show
#'   automatic APA reporting, assumption checks, recommendations, insights,
#'   multigroup comparisons, and model-selection guidance.
#' @param reportLevel reporting detail level: \code{"basic"}, \code{"apa"}, or
#'   \code{"advanced"}.
#' @param autoOrdinal \code{TRUE} or \code{FALSE} (default TRUE), automatically
#'   detect ordinal or binary model variables and use ordered-variable estimation
#'   where appropriate.
#' @param reportParagraph \code{TRUE} or \code{FALSE} (default TRUE), show a
#'   single copyable paragraph version of the automatic report with a caution to
#'   review the text before manuscript use.
#' @param showSyntax \code{TRUE} or \code{FALSE} (default TRUE), show generated
#'   lavaan and Mermaid syntax tables.
#' @param showMermaidDiagramSyntax \code{TRUE} or \code{FALSE} (default FALSE),
#'   show generated Mermaid syntax below the path diagram output.
#' @param showPathLegend \code{TRUE} or \code{FALSE} (default TRUE), show a
#'   non-overlapping table legend explaining path labels and significance stars
#'   below the path diagram.
#' @param syntaxSource model input source: \code{"gui"}, \code{"lavaan"},
#'   \code{"mermaid"}, \code{"mplus"}, or \code{"openmx"}. When not
#'   \code{"gui"}, \code{syntaxText} is used as the model syntax.
#' @param syntaxVars variables referenced by imported syntax. When omitted in R
#'   calls with \code{data}, all columns in \code{data} are made available.
#' @param syntaxText full lavaan or Mermaid syntax pasted as one string.
#' @param syntaxApply \code{TRUE} or \code{FALSE} (default FALSE), confirm that
#'   imported syntax should be used for estimation.
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model} \tab \tab \tab \tab \tab The underlying \code{lavaan} object \cr
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$fit$main} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$fit$constraints} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$fit$indices} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$r2} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$coefficients} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$correlations} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$intercepts} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$defined} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$contrastCodeTable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$pathgroup$diagrams} \tab \tab \tab \tab \tab an array of path diagrams \cr
#'   \code{results$pathgroup$notes} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$contraintsnotes} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$info$asDF}
#'
#' \code{as.data.frame(results$info)}
#'
#' @export
pathj <- function(
  data,
  endogenous = NULL,
  factors = NULL,
  covs = NULL,
  multigroup = NULL,
  clusterVariable = NULL,
  withinVariables = NULL,
  betweenVariables = NULL,
  se = "standard",
  r2ci = "fisher",
  r2test = FALSE,
  bootci = "perc",
  ci = TRUE,
  ciWidth = 95,
  bootN = 1000,
  showintercepts = TRUE,
  intercepts = TRUE,
  indirect = FALSE,
  contrasts = NULL,
  showRealNames = TRUE,
  showContrastCode = FALSE,
  scaling = NULL,
  endogenousTerms = list(
    list()),
  diagram = FALSE,
  diag_paths = "est",
  diag_sigstars = FALSE,
  diag_resid = FALSE,
  diag_labsize = "medium",
  diag_rotate = "2",
  diag_type = "tree2",
  diag_shape = "rectangle",
  diag_abbrev = "0",
  varcov=NULL,
  cov_y = TRUE,
  cov_x = TRUE,
  constraints = list(),
  constraints_examples = FALSE,
  showlabels = FALSE,
  scoretest = TRUE,
  cumscoretest = FALSE,
  modindices = FALSE,
  miMin = 4,
  estimator = "ML",
  likelihood = "normal",
  missing = "fiml",
  miN = 5,
  miSeed = 12345,
  miStrategy = "include_group",
  showMissingDiagnostics = FALSE,
  intelligentReport = TRUE,
  reportLevel = "apa",
  autoOrdinal = TRUE,
  reportParagraph = TRUE,
  showSyntax = TRUE,
  showMermaidDiagramSyntax = FALSE,
  showPathLegend = TRUE,
  syntaxSource = "gui",
  syntaxVars = NULL,
  syntaxText = "",
  syntaxApply = FALSE,
  formula) {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("pathj requires jmvcore to be installed (restart may be required)")
  
  if ( ! missing(formula)) {
    if (missing(endogenous))
      endogenous <- pathjClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="endogenous")
    if (missing(endogenousTerms))
      endogenousTerms <- pathjClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="endogenousTerms")
    if (missing(factors))
      factors <- pathjClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="factors")
    if (missing(covs))
      covs <- pathjClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="covs")
  }
  
  if ( ! missing(endogenous)) endogenous <- jmvcore::resolveQuo(jmvcore::enquo(endogenous))
  if ( ! missing(factors)) factors <- jmvcore::resolveQuo(jmvcore::enquo(factors))
  if ( ! missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
  if ( ! missing(multigroup)) multigroup <- jmvcore::resolveQuo(jmvcore::enquo(multigroup))
  if ( ! missing(clusterVariable)) clusterVariable <- jmvcore::resolveQuo(jmvcore::enquo(clusterVariable))
  if ( ! missing(withinVariables)) withinVariables <- jmvcore::resolveQuo(jmvcore::enquo(withinVariables))
  if ( ! missing(betweenVariables)) betweenVariables <- jmvcore::resolveQuo(jmvcore::enquo(betweenVariables))
  if (missing(data))
    data <- jmvcore::marshalData(
      parent.frame(),
      `if`( ! missing(endogenous), endogenous, NULL),
      `if`( ! missing(factors), factors, NULL),
      `if`( ! missing(covs), covs, NULL),
      `if`( ! missing(multigroup), multigroup, NULL),
      `if`( ! missing(clusterVariable), clusterVariable, NULL),
      `if`( ! missing(withinVariables), withinVariables, NULL),
      `if`( ! missing(betweenVariables), betweenVariables, NULL))

  if (!identical(syntaxSource, "gui") && is.null(syntaxVars) && !missing(data))
    syntaxVars <- names(data)
  
  for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  for (v in multigroup) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  
  varcov<-lapply(varcov,function(v) {
    if (length(v)!=2)
       NULL
    else
      list(i1=v[[1]],i2=v[[2]])
  })

  options <- pathjOptions$new(
    endogenous = endogenous,
    factors = factors,
    covs = covs,
    multigroup = multigroup,
    clusterVariable = clusterVariable,
    withinVariables = withinVariables,
    betweenVariables = betweenVariables,
    se = se,
    r2ci = r2ci,
    r2test = r2test,
    bootci = bootci,
    ci = ci,
    ciWidth = ciWidth,
    bootN = bootN,
    showintercepts = showintercepts,
    intercepts = intercepts,
    indirect = indirect,
    contrasts = contrasts,
    showRealNames = showRealNames,
    showContrastCode = showContrastCode,
    scaling = scaling,
    endogenousTerms = endogenousTerms,
    diagram = diagram,
    diag_paths = diag_paths,
    diag_sigstars = diag_sigstars,
    diag_resid = diag_resid,
    diag_labsize = diag_labsize,
    diag_rotate = diag_rotate,
    diag_type = diag_type,
    diag_shape = diag_shape,
    diag_abbrev = diag_abbrev,
    varcov = varcov,
    cov_y = cov_y,
    cov_x = cov_x,
    constraints = constraints,
    constraints_examples = constraints_examples,
    showlabels = showlabels,
    scoretest = scoretest,
    cumscoretest = cumscoretest,
    modindices = modindices,
    miMin = miMin,
    estimator = estimator,
    likelihood = likelihood,
    missing = missing,
    miN = miN,
    miSeed = miSeed,
    miStrategy = miStrategy,
    showMissingDiagnostics = showMissingDiagnostics,
    intelligentReport = intelligentReport,
    reportLevel = reportLevel,
    autoOrdinal = autoOrdinal,
    reportParagraph = reportParagraph,
    showSyntax = showSyntax,
    showMermaidDiagramSyntax = showMermaidDiagramSyntax,
    showPathLegend = showPathLegend,
    syntaxSource = syntaxSource,
    syntaxVars = syntaxVars,
    syntaxText = syntaxText,
    syntaxApply = syntaxApply)
  
  analysis <- pathjClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}
