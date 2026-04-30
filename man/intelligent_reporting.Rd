\name{intelligent_reporting}
\alias{intelligent_reporting}
\alias{run_model}
\alias{detect_variable_types}
\alias{report_fit}
\alias{report_paths}
\alias{report_mediation}
\alias{report_moderation}
\alias{report_multigroup}
\alias{compare_groups}
\alias{compare_models}
\alias{check_assumptions}
\alias{generate_recommendations}
\alias{generate_model_insights}
\alias{diagnose_modification_indices}
\alias{p_curve_analysis}
\alias{generate_report}
\title{Intelligent SEM reporting and diagnostics}
\usage{
run_model(model, data, group = NULL, ordered = NULL, estimator = NULL,
  missing = "fiml", se = "standard", bootstrap = 1000, ...)

detect_variable_types(data, ordinal_max_unique = 7)

report_fit(fit)

report_paths(fit, include_nonsignificant = FALSE)

report_mediation(fit)

report_moderation(fit)

report_multigroup(fit)

compare_groups(fit, group_labels = NULL)

compare_models(...)

check_assumptions(fit, data = NULL, cluster = NULL)

generate_recommendations(diagnostics)

generate_model_insights(fit)

diagnose_modification_indices(fit, threshold = 10)

p_curve_analysis(fit)

generate_report(fit, data = NULL, teaching_mode = c("apa", "basic", "advanced"))
}
\arguments{
\item{model}{lavaan model syntax.}
\item{data}{A data frame.}
\item{group}{Optional lavaan group variable.}
\item{ordered}{Optional ordered variable names.}
\item{estimator}{Optional lavaan estimator. Defaults to WLSMV for ordered data and MLR otherwise.}
\item{missing}{Missing data method for ML-family estimators.}
\item{se}{Standard error method passed to lavaan.}
\item{bootstrap}{Bootstrap draws when bootstrap standard errors are requested.}
\item{...}{Additional lavaan arguments, or fitted models for \code{compare_models()}.}
\item{ordinal_max_unique}{Maximum number of integer-like unique values treated as ordinal.}
\item{fit}{A fitted lavaan object.}
\item{include_nonsignificant}{Whether to include non-significant paths in narrative text.}
\item{group_labels}{Optional group labels for multigroup comparisons.}
\item{diagnostics}{A diagnostics data frame returned by \code{check_assumptions()}.}
\item{cluster}{Optional clustering variable name.}
\item{threshold}{Minimum modification index to report.}
\item{teaching_mode}{Reporting level: \code{"basic"}, \code{"apa"}, or \code{"advanced"}.}
}
\description{
Utilities for estimating lavaan path/SEM models and converting fitted output into
APA-style text, interpretable tables, diagnostics, recommendations, model
comparisons, multigroup path comparisons, mediation/moderation summaries, and
teaching-oriented model insights.
}
\value{
Most reporting functions return a list containing one or more tables and a
human-readable narrative. \code{detect_variable_types()} and
\code{check_assumptions()} return data frames. \code{run_model()} returns a
lavaan object.
}
