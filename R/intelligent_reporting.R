apa_p <- function(p) {
  if (is.na(p))
    return("p = NA")
  if (p < .001)
    return("p < .001")
  paste0("p = ", formatC(p, digits = 3, format = "f"))
}

apa_num <- function(x, digits = 3) {
  if (is.na(x))
    return("NA")
  formatC(x, digits = digits, format = "f")
}

effect_size_label <- function(beta) {
  beta <- abs(beta)
  out <- rep("negligible", length(beta))
  out[beta >= .10] <- "small"
  out[beta >= .30] <- "medium"
  out[beta >= .50] <- "large"
  out[is.na(beta)] <- NA_character_
  out
}

mermaid_escape <- function(x) {
  x <- gsub("\"", "'", x, fixed = TRUE)
  x <- gsub("\n", " ", x, fixed = TRUE)
  x
}

mermaid_id <- function(x) {
  id <- gsub("[^A-Za-z0-9_]", "_", x)
  id <- gsub("_+", "_", id)
  if (grepl("^[0-9]", id))
    id <- paste0("v_", id)
  id
}

safe_fit_measures <- function(fit, measures) {
  vals <- rep(NA_real_, length(measures))
  names(vals) <- measures
  res <- try_hard(lavaan::fitmeasures(fit, fit.measures = measures))
  if (isFALSE(res$error) && is.something(res$obj))
    vals[names(res$obj)] <- unname(res$obj)
  vals
}

model_observed_variables <- function(model) {
  pt <- try_hard(lavaan::lavaanify(model, fixed.x = FALSE))
  if (!isFALSE(pt$error) || !is.something(pt$obj))
    return(character(0))
  vars <- unique(c(pt$obj$lhs, pt$obj$rhs))
  vars <- vars[!is.na(vars) & vars != "" & vars != "1"]
  defined <- pt$obj$lhs[pt$obj$op == ":="]
  setdiff(vars, defined)
}

#' Detect observed variable types for SEM estimation.
#'
#' @param data A data frame.
#' @param ordinal_max_unique Numeric variables with this many ordered unique
#'   values or fewer are treated as ordinal, unless binary.
#' @return A data frame with variable, type, n_unique, and ordered columns.
#' @export
detect_variable_types <- function(data, ordinal_max_unique = 7) {
  rows <- lapply(names(data), function(v) {
    x <- data[[v]]
    ux <- unique(x[!is.na(x)])
    n_unique <- length(ux)
    ordered_var <- is.ordered(x)

    type <- "continuous"
    if (is.logical(x) || n_unique == 2) {
      type <- "binary"
    } else if (ordered_var) {
      type <- "ordinal"
    } else if (is.factor(x) || is.character(x)) {
      type <- "nominal"
    } else if (is.numeric(x) && n_unique > 2 && n_unique <= ordinal_max_unique) {
      sorted <- sort(ux)
      if (all(abs(sorted - round(sorted)) < .Machine$double.eps^0.5))
        type <- "ordinal"
    }

    data.frame(
      variable = v,
      type = type,
      n_unique = n_unique,
      ordered = ordered_var || type %in% c("ordinal", "binary"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Estimate a lavaan path or SEM model with automatic ordinal handling.
#'
#' @param model lavaan model syntax.
#' @param data A data frame.
#' @param group Optional lavaan group variable.
#' @param ordered Optional ordered variable names. If NULL, ordinal and binary
#'   variables are detected automatically.
#' @param estimator Optional estimator. Defaults to WLSMV when ordered variables
#'   are present and MLR otherwise.
#' @param missing Missing data method for continuous ML models.
#' @param se Standard errors passed to lavaan.
#' @param bootstrap Number of bootstrap draws when se = "bootstrap" or "boot".
#' @param ... Additional arguments passed to lavaan::sem().
#' @return A lavaan object with intelligent_reporting metadata attributes.
#' @export
run_model <- function(model, data, group = NULL, ordered = NULL, estimator = NULL,
                      missing = "fiml", se = "standard", bootstrap = 1000, ...) {
  types <- detect_variable_types(data)
  model_vars <- intersect(model_observed_variables(model), names(data))
  if (!is.something(model_vars))
    model_vars <- names(data)
  if (is.null(ordered))
    ordered <- types$variable[types$type %in% c("ordinal", "binary")]
  ordered <- intersect(ordered, model_vars)

  if (is.null(estimator))
    estimator <- if (length(ordered) > 0) "WLSMV" else "MLR"

  args <- list(
    model = model,
    data = data,
    estimator = estimator,
    se = se
  )
  if (length(ordered) > 0)
    args$ordered <- ordered
  if (is.something(group))
    args$group <- group
  if (tolower(estimator) %in% c("ml", "mlr", "mlm", "mlmv", "mlf"))
    args$missing <- missing
  if (se %in% c("boot", "bootstrap"))
    args$bootstrap <- bootstrap
  args <- c(args, list(...))

  fit <- with_safe_lavaan_cores(do.call(lavaan::sem, args))
  attr(fit, "pathj_variable_types") <- types
  attr(fit, "pathj_ordered") <- ordered
  attr(fit, "pathj_estimator") <- estimator
  attr(fit, "pathj_missing_method") <- ifelse("missing" %in% names(args), missing, "lavaan default")
  fit
}

#' APA-style model fit report.
#'
#' @param fit A lavaan object.
#' @return A list with table and text.
#' @export
report_fit <- function(fit) {
  measures <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
                "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic")
  ff <- safe_fit_measures(fit, measures)
  tab <- data.frame(
    chisq = ff[["chisq"]],
    df = ff[["df"]],
    pvalue = ff[["pvalue"]],
    cfi = ff[["cfi"]],
    tli = ff[["tli"]],
    rmsea = ff[["rmsea"]],
    rmsea.ci.lower = ff[["rmsea.ci.lower"]],
    rmsea.ci.upper = ff[["rmsea.ci.upper"]],
    srmr = ff[["srmr"]],
    aic = ff[["aic"]],
    bic = ff[["bic"]],
    stringsAsFactors = FALSE
  )

  text <- paste0(
    "Model fit was evaluated using chi-square, CFI, TLI, RMSEA, and SRMR. ",
    "The model fit was chi-square(",
    apa_num(tab$df, 0), ") = ", apa_num(tab$chisq), ", ",
    apa_p(tab$pvalue), ", CFI = ", apa_num(tab$cfi),
    ", TLI = ", apa_num(tab$tli), ", RMSEA = ", apa_num(tab$rmsea),
    " [", apa_num(tab$rmsea.ci.lower), ", ", apa_num(tab$rmsea.ci.upper),
    "], and SRMR = ", apa_num(tab$srmr), "."
  )

  list(table = tab, text = text)
}

#' APA-style direct path report.
#'
#' @param fit A lavaan object.
#' @param include_nonsignificant Include non-significant paths in text.
#' @return A list with table and text.
#' @export
report_paths <- function(fit, include_nonsignificant = FALSE) {
  pe <- lavaan::parameterestimates(fit, standardized = TRUE, ci = TRUE)
  paths <- pe[pe$op == "~", , drop = FALSE]
  if (nrow(paths) == 0)
    return(list(table = paths, text = "No direct regression paths were estimated."))

  paths$beta <- if ("std.all" %in% names(paths)) paths$std.all else paths$est
  paths$effect_size <- effect_size_label(paths$beta)
  paths$significant <- !is.na(paths$pvalue) & paths$pvalue < .05
  paths$interpretation <- ifelse(
    paths$significant,
    paste0(paths$rhs, " was a ", paths$effect_size, " predictor of ", paths$lhs, "."),
    paste0(paths$rhs, " was not a statistically significant predictor of ", paths$lhs, ".")
  )

  keep <- intersect(c("lhs", "rhs", "group", "est", "se", "z", "pvalue",
                      "ci.lower", "ci.upper", "beta", "effect_size",
                      "significant", "interpretation"), names(paths))
  tab <- paths[, keep, drop = FALSE]

  text_rows <- paths[paths$significant | include_nonsignificant, , drop = FALSE]
  if (nrow(text_rows) == 0) {
    text <- "No direct paths were statistically significant at alpha = .05."
  } else {
    text <- paste(vapply(seq_len(nrow(text_rows)), function(i) {
      r <- text_rows[i, ]
      paste0(
        r$rhs, " predicted ", r$lhs, ", beta = ", apa_num(r$beta),
        ", SE = ", apa_num(r$se), ", z = ", apa_num(r$z), ", ",
        apa_p(r$pvalue), " (", r$effect_size, " effect)."
      )
    }, FUN.VALUE = character(1)), collapse = " ")
  }

  list(table = tab, text = text)
}

#' Mediation decomposition from lavaan defined parameters.
#'
#' @param fit A lavaan object.
#' @return A list with table and text.
#' @export
report_mediation <- function(fit) {
  pe <- lavaan::parameterestimates(fit, standardized = TRUE, ci = TRUE)
  defs <- pe[pe$op == ":=", , drop = FALSE]
  if (nrow(defs) == 0)
    return(list(table = data.frame(), text = "No indirect or defined effects were estimated."))

  is_indirect <- grepl("ind|indirect|^ie", defs$lhs, ignore.case = TRUE) |
    grepl("\\*", defs$rhs)
  med <- defs[is_indirect, , drop = FALSE]
  if (nrow(med) == 0)
    med <- defs

  tab <- data.frame(
    effect = med$lhs,
    expression = med$rhs,
    indirect = med$est,
    se = med$se,
    z = med$z,
    pvalue = med$pvalue,
    ci.lower = med$ci.lower,
    ci.upper = med$ci.upper,
    beta = if ("std.all" %in% names(med)) med$std.all else med$est,
    significant = !is.na(med$pvalue) & med$pvalue < .05,
    percent_mediated = NA_real_,
    stringsAsFactors = FALSE
  )

  text <- paste(vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    paste0(
      "The indirect effect ", r$effect, " was beta = ", apa_num(r$beta),
      ", 95% CI [", apa_num(r$ci.lower), ", ", apa_num(r$ci.upper),
      "], ", apa_p(r$pvalue), "."
    )
  }, FUN.VALUE = character(1)), collapse = " ")

  list(table = tab, text = text)
}

detect_interaction_terms <- function(paths) {
  rhs_hit <- grepl(":", paths$rhs, fixed = TRUE) |
    grepl("\\*", paths$rhs) |
    grepl("__XX__XX__|_x_|\\.x\\.", paths$rhs, ignore.case = TRUE)
  if ("label" %in% names(paths))
    rhs_hit <- rhs_hit | grepl("int|interaction|moder", paths$label, ignore.case = TRUE)
  rhs_hit
}

#' Moderation report based on interaction terms in regression paths.
#'
#' @param fit A lavaan object.
#' @return A list with table and text.
#' @export
report_moderation <- function(fit) {
  pe <- lavaan::parameterestimates(fit, standardized = TRUE, ci = TRUE)
  paths <- pe[pe$op == "~", , drop = FALSE]
  paths <- paths[detect_interaction_terms(paths), , drop = FALSE]
  if (nrow(paths) == 0)
    return(list(table = data.frame(), text = "No interaction terms were detected."))

  paths$beta <- if ("std.all" %in% names(paths)) paths$std.all else paths$est
  paths$direction <- ifelse(paths$beta >= 0, "amplifying", "buffering")
  paths$significant <- !is.na(paths$pvalue) & paths$pvalue < .05
  tab <- paths[, intersect(c("lhs", "rhs", "group", "est", "se", "z", "pvalue",
                             "ci.lower", "ci.upper", "beta", "direction",
                             "significant"), names(paths)), drop = FALSE]
  text <- paste(vapply(seq_len(nrow(paths)), function(i) {
    r <- paths[i, ]
    paste0(
      "The interaction term ", r$rhs, " predicting ", r$lhs,
      " was ", r$direction, ", beta = ", apa_num(r$beta),
      ", ", apa_p(r$pvalue), "."
    )
  }, FUN.VALUE = character(1)), collapse = " ")

  list(table = tab, text = text)
}

#' Compare path coefficients between two or more lavaan groups.
#'
#' @param fit A multigroup lavaan object.
#' @param group_labels Optional group labels.
#' @return A list with table and text.
#' @export
compare_groups <- function(fit, group_labels = NULL) {
  pe <- lavaan::parameterestimates(fit, standardized = TRUE)
  paths <- pe[pe$op == "~" & !is.na(pe$group), , drop = FALSE]
  if (nrow(paths) == 0 || length(unique(paths$group)) < 2)
    return(list(table = data.frame(), text = "No multigroup regression paths were available for comparison."))

  if (is.null(group_labels)) {
    labels <- lavaan::lavInspect(fit, "group.label")
    if (is.null(labels))
      labels <- as.character(sort(unique(paths$group)))
    group_labels <- labels
  }

  keys <- unique(paste(paths$lhs, paths$rhs, sep = "\r"))
  rows <- lapply(keys, function(key) {
    parts <- strsplit(key, "\r", fixed = TRUE)[[1]]
    x <- paths[paths$lhs == parts[[1]] & paths$rhs == parts[[2]], , drop = FALSE]
    x <- x[order(x$group), , drop = FALSE]
    if (nrow(x) < 2)
      return(NULL)
    g1 <- x[1, ]
    g2 <- x[2, ]
    diff <- g1$est - g2$est
    se_diff <- sqrt(g1$se^2 + g2$se^2)
    z <- diff / se_diff
    p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
    data.frame(
      path = paste(parts[[1]], "~", parts[[2]]),
      group_1 = group_labels[[g1$group]],
      group_2 = group_labels[[g2$group]],
      group_1_beta = g1$std.all,
      group_2_beta = g2$std.all,
      difference = diff,
      z = z,
      pvalue = p,
      significant = !is.na(p) && p < .05,
      interpretation = ifelse(!is.na(p) && p < .05,
                              "The path differs significantly between groups.",
                              "No statistically significant group difference was detected."),
      stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (!is.something(tab))
    tab <- data.frame()

  if (nrow(tab) == 0 || !any(tab$significant)) {
    text <- "No statistically significant multigroup differences in regression paths were detected."
  } else {
    sig <- tab[tab$significant, , drop = FALSE]
    text <- paste(vapply(seq_len(nrow(sig)), function(i) {
      r <- sig[i, ]
      paste0("The multigroup ", r$path, " path differed between ", r$group_1,
             " and ", r$group_2, ", z = ", apa_num(r$z), ", ", apa_p(r$pvalue), ".")
    }, FUN.VALUE = character(1)), collapse = " ")
  }

  list(table = tab, text = text)
}

#' Alias for multigroup reporting.
#'
#' @param fit A multigroup lavaan object.
#' @return A list with table and text.
#' @export
report_multigroup <- function(fit) {
  compare_groups(fit)
}

recursive_status <- function(fit) {
  pe <- lavaan::parameterestimates(fit)
  edges <- pe[pe$op == "~", c("lhs", "rhs"), drop = FALSE]
  if (nrow(edges) == 0)
    return(TRUE)
  nodes <- unique(c(edges$lhs, edges$rhs))
  graph <- lapply(nodes, function(n) edges$lhs[edges$rhs == n])
  names(graph) <- nodes
  visiting <- stats::setNames(rep(FALSE, length(nodes)), nodes)
  visited <- visiting

  has_cycle <- FALSE
  dfs <- function(n) {
    if (visiting[[n]]) {
      has_cycle <<- TRUE
      return()
    }
    if (visited[[n]] || has_cycle)
      return()
    visiting[[n]] <<- TRUE
    for (to in graph[[n]])
      if (to %in% nodes) dfs(to)
    visiting[[n]] <<- FALSE
    visited[[n]] <<- TRUE
  }
  for (n in nodes)
    dfs(n)
  !has_cycle
}

numeric_skew <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || stats::sd(x) == 0)
    return(NA_real_)
  mean((x - mean(x))^3) / stats::sd(x)^3
}

numeric_kurtosis <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 4 || stats::sd(x) == 0)
    return(NA_real_)
  mean((x - mean(x))^4) / stats::sd(x)^4
}

mardia_summary <- function(data) {
  num <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
  num <- stats::na.omit(num)
  p <- ncol(num)
  n <- nrow(num)
  if (p < 2 || n <= p)
    return(c(skew = NA_real_, kurtosis = NA_real_))
  s <- stats::cov(num)
  inv <- tryCatch(solve(s), error = function(e) NULL)
  if (is.null(inv))
    return(c(skew = NA_real_, kurtosis = NA_real_))
  z <- scale(num, center = TRUE, scale = FALSE)
  d <- as.matrix(z) %*% inv %*% t(as.matrix(z))
  c(skew = mean(d^3), kurtosis = mean(diag(d)^2))
}

max_vif_from_paths <- function(fit, data) {
  pe <- lavaan::parameterestimates(fit)
  paths <- pe[pe$op == "~", c("lhs", "rhs"), drop = FALSE]
  outcomes <- unique(paths$lhs)
  vifs <- numeric(0)
  for (y in outcomes) {
    preds <- unique(paths$rhs[paths$lhs == y])
    preds <- preds[preds %in% names(data) & vapply(data[preds], is.numeric, logical(1))]
    if (length(preds) < 2)
      next()
    d <- stats::na.omit(data[, preds, drop = FALSE])
    if (nrow(d) <= length(preds) + 1)
      next()
    for (p in preds) {
      f <- stats::as.formula(paste(p, "~", paste(setdiff(preds, p), collapse = "+")))
      r2 <- tryCatch(summary(stats::lm(f, data = d))$r.squared, error = function(e) NA_real_)
      if (!is.na(r2) && r2 < 1)
        vifs <- c(vifs, 1 / (1 - r2))
    }
  }
  if (length(vifs) == 0) NA_real_ else max(vifs, na.rm = TRUE)
}

diagnostic_row <- function(check, status, explanation, recommendation) {
  data.frame(
    check = check,
    status = status,
    explanation = explanation,
    recommendation = recommendation,
    stringsAsFactors = FALSE
  )
}

#' Assumption checks and recommendations for a fitted SEM.
#'
#' @param fit A lavaan object.
#' @param data Optional original data frame for distribution, missingness, and VIF diagnostics.
#' @param cluster Optional cluster variable name for independence screening.
#' @return A data frame of checks, statuses, explanations, and recommendations.
#' @export
check_assumptions <- function(fit, data = NULL, cluster = NULL) {
  rows <- list()
  ff <- safe_fit_measures(fit, c("cfi", "tli", "rmsea", "srmr"))
  ok_fit <- (is.na(ff[["cfi"]]) || ff[["cfi"]] >= .90) &&
    (is.na(ff[["rmsea"]]) || ff[["rmsea"]] <= .08) &&
    (is.na(ff[["srmr"]]) || ff[["srmr"]] <= .08)
  rows[[length(rows) + 1]] <- diagnostic_row(
    "Model fit",
    ifelse(ok_fit, "Met", "Warning"),
    paste0("CFI = ", apa_num(ff[["cfi"]]), ", RMSEA = ", apa_num(ff[["rmsea"]]),
           ", SRMR = ", apa_num(ff[["srmr"]]), "."),
    ifelse(ok_fit, "Report fit indices and proceed with substantive interpretation.",
           "Inspect residuals, theory, and modification indices before changing the model.")
  )

  rows[[length(rows) + 1]] <- diagnostic_row(
    "Recursive structure",
    ifelse(recursive_status(fit), "Met", "Violated"),
    ifelse(recursive_status(fit), "No directed feedback loop was detected.",
           "At least one directed feedback loop was detected."),
    ifelse(recursive_status(fit), "No action required for recursive path analysis.",
           "Use a model class that supports reciprocal effects or revise the directed structure.")
  )

  n <- tryCatch(lavaan::lavInspect(fit, "ntotal"), error = function(e) NA_integer_)
  npar <- tryCatch(fit@Fit@npar, error = function(e) NA_integer_)
  ratio <- n / npar
  rows[[length(rows) + 1]] <- diagnostic_row(
    "Sample size",
    ifelse(!is.na(ratio) && ratio >= 10, "Met", "Warning"),
    paste0("N = ", n, "; free parameters = ", npar, "; N/parameter = ", apa_num(ratio, 1), "."),
    ifelse(!is.na(ratio) && ratio >= 10, "Sample size is broadly adequate by a simple N/parameter rule.",
           "Use robust estimation, bootstrap intervals, or a simpler theory-driven model.")
  )

  if (!is.null(data)) {
    num <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
    max_skew <- if (ncol(num) > 0) max(abs(vapply(num, numeric_skew, numeric(1))), na.rm = TRUE) else NA_real_
    max_kurt <- if (ncol(num) > 0) max(abs(vapply(num, function(x) numeric_kurtosis(x) - 3, numeric(1))), na.rm = TRUE) else NA_real_
    mardia <- mardia_summary(num)
    normal_warn <- (!is.na(max_skew) && max_skew > 2) || (!is.na(max_kurt) && max_kurt > 7)
    rows[[length(rows) + 1]] <- diagnostic_row(
      "Multivariate normality",
      ifelse(normal_warn, "Warning", "Met"),
      paste0("Maximum absolute skew = ", apa_num(max_skew),
             "; maximum excess kurtosis = ", apa_num(max_kurt),
             "; Mardia skew = ", apa_num(mardia[["skew"]]),
             "; Mardia kurtosis = ", apa_num(mardia[["kurtosis"]]), "."),
      ifelse(normal_warn, "Prefer robust ML, WLSMV for ordered variables, or bootstrap confidence intervals.",
             "Distributional screening did not flag severe non-normality.")
    )

    max_cor <- NA_real_
    if (ncol(num) > 1) {
      cc <- suppressWarnings(stats::cor(num, use = "pairwise.complete.obs"))
      max_cor <- max(abs(cc[upper.tri(cc)]), na.rm = TRUE)
    }
    max_vif <- max_vif_from_paths(fit, data)
    col_warn <- (!is.na(max_cor) && max_cor >= .85) || (!is.na(max_vif) && max_vif >= 5)
    rows[[length(rows) + 1]] <- diagnostic_row(
      "Multicollinearity",
      ifelse(col_warn, "Warning", "Met"),
      paste0("Maximum predictor correlation = ", apa_num(max_cor),
             "; maximum VIF = ", apa_num(max_vif), "."),
      ifelse(col_warn, "Consider centering, combining redundant predictors, or simplifying the model.",
             "No severe collinearity was detected by correlation/VIF screening.")
    )

    miss <- sum(is.na(data))
    miss_pct <- 100 * miss / (nrow(data) * ncol(data))
    rows[[length(rows) + 1]] <- diagnostic_row(
      "Missing data",
      ifelse(miss_pct <= 5, "Met", "Warning"),
      paste0(apa_num(miss_pct, 1), "% of data cells were missing."),
      ifelse(miss_pct <= 5, "Describe the missing data method used.",
             "Report the missing data method and consider FIML or multiple imputation.")
    )

    if (!is.null(cluster) && cluster %in% names(data)) {
      cluster_tab <- table(data[[cluster]], useNA = "no")
      min_cluster <- if (length(cluster_tab) > 0) min(cluster_tab) else NA_integer_
      median_cluster <- if (length(cluster_tab) > 0) stats::median(cluster_tab) else NA_real_
      cluster_warn <- length(cluster_tab) > 1 && (!is.na(min_cluster) && min_cluster < 5)
      rows[[length(rows) + 1]] <- diagnostic_row(
        "Independence",
        "Warning",
        paste0("Cluster variable ", cluster, " was supplied with ", length(cluster_tab),
               " clusters; median cluster size = ", apa_num(median_cluster, 1),
               "; minimum cluster size = ", min_cluster, "."),
        ifelse(cluster_warn,
               "Some cluster sizes are small, which may reduce the reliability of multilevel estimates. Interpret clustered results with caution.",
               "Estimate ICCs and use cluster-robust standard errors or multilevel SEM when clustering is material.")
      )
    } else {
      rows[[length(rows) + 1]] <- diagnostic_row(
        "Independence",
        "Met",
        "No cluster variable was supplied for dependence screening.",
        "If data are clustered, provide the cluster variable and consider robust or multilevel methods."
      )
    }
  }

  rows[[length(rows) + 1]] <- diagnostic_row(
    "Residual independence",
    "Warning",
    "Residual dependence requires inspection of residual covariances and theory.",
    "Review standardized residuals and modification indices; add residual covariances only with theoretical justification."
  )

  rows[[length(rows) + 1]] <- diagnostic_row(
    "Measurement reliability",
    "Warning",
    "Reliability cannot be inferred from a path model without scale/item information.",
    "Report alpha, omega, or CFA reliability when multi-item constructs are used."
  )

  do.call(rbind, rows)
}

#' Estimate simple ICC summaries for clustered variables.
#'
#' @param data Original data frame.
#' @param cluster Cluster variable name.
#' @param variables Numeric variable names to screen.
#' @return A data frame with one row per variable.
#' @export
calculate_icc <- function(data, cluster, variables) {
  if (is.null(data) || is.null(cluster) || !cluster %in% names(data))
    return(data.frame())
  variables <- intersect(variables, names(data))
  variables <- variables[vapply(data[variables], is.numeric, logical(1))]
  if (length(variables) == 0)
    return(data.frame())

  rows <- lapply(variables, function(variable) {
    d <- data.frame(y = data[[variable]], cluster = data[[cluster]])
    d <- d[stats::complete.cases(d), , drop = FALSE]
    if (nrow(d) == 0 || length(unique(d$cluster)) < 2)
      return(data.frame(variable = variable, clusters = length(unique(d$cluster)),
                        icc = NA_real_, interpretation = "ICC could not be estimated.",
                        stringsAsFactors = FALSE))

    cluster_means <- stats::aggregate(y ~ cluster, d, mean)
    between <- stats::var(cluster_means$y, na.rm = TRUE)
    within_values <- stats::ave(d$y, d$cluster, FUN = function(x) x - mean(x, na.rm = TRUE))
    within <- stats::var(within_values, na.rm = TRUE)
    icc <- between / (between + within)
    interpretation <- if (is.na(icc)) {
      "ICC could not be estimated."
    } else if (icc < .05) {
      "Little clustering was detected for this variable."
    } else if (icc < .15) {
      "A modest share of variance was between clusters."
    } else {
      "A meaningful share of variance was between clusters; multilevel modelling should be considered."
    }
    data.frame(variable = variable, clusters = length(unique(d$cluster)),
               icc = icc, interpretation = interpretation,
               stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

#' Multilevel interpretation for clustered SEM inputs.
#'
#' @param data Optional original data frame.
#' @param cluster Optional cluster variable name.
#' @param within Within-person or level-1 variable names.
#' @param between Between-person or level-2 variable names.
#' @return A list with ICC table and plain-English text.
#' @export
report_multilevel <- function(data = NULL, cluster = NULL, within = NULL, between = NULL) {
  within <- within[!is.na(within) & nzchar(within)]
  between <- between[!is.na(between) & nzchar(between)]
  variables <- unique(c(within, between))
  icc <- calculate_icc(data, cluster, variables)

  if (is.null(cluster) || !nzchar(cluster)) {
    text <- "No cluster variable was supplied, so multilevel within-person and between-person interpretations were not generated."
  } else {
    text <- paste0(
      "Cluster variable: ", cluster, ". ",
      if (length(within) > 0)
        paste0("Within-person variables were specified as ", paste(within, collapse = ", "),
               "; these effects should be interpreted as occasion-level deviations within clusters. ")
      else
        "No within-person variables were specified. ",
      if (length(between) > 0)
        paste0("Between-person variables were specified as ", paste(between, collapse = ", "),
               "; these effects should be interpreted as stable differences between clusters. ")
      else
        "No between-person variables were specified. ",
      if (nrow(icc) > 0 && any(!is.na(icc$icc)))
        paste0("ICC screening found the largest ICC was ", apa_num(max(icc$icc, na.rm = TRUE)), ".")
      else
        "ICC screening was not available for the selected variables."
    )
  }

  list(table = icc, text = text)
}

#' Generate recommendations from assumption diagnostics.
#'
#' @param diagnostics A data frame from check_assumptions().
#' @return Character vector of recommendations.
#' @export
generate_recommendations <- function(diagnostics) {
  if (!is.something(diagnostics) || !all(c("status", "recommendation") %in% names(diagnostics)))
    return(character(0))
  unique(diagnostics$recommendation[diagnostics$status %in% c("Warning", "Violated")])
}

#' Model insight engine for fitted SEM paths.
#'
#' @param fit A lavaan object.
#' @return A list with strongest predictors, mediators, risk/protective tables, and text.
#' @export
generate_model_insights <- function(fit) {
  paths <- report_paths(fit, include_nonsignificant = TRUE)$table
  if (!is.something(paths) || nrow(paths) == 0)
    return(list(text = "No direct paths were available for insight generation."))

  paths$abs_beta <- abs(paths$beta)
  paths <- paths[order(-paths$abs_beta), , drop = FALSE]
  strongest <- paths[seq_len(min(3, nrow(paths))), , drop = FALSE]
  risk <- paths[!is.na(paths$beta) & paths$beta > 0 & paths$significant, , drop = FALSE]
  protective <- paths[!is.na(paths$beta) & paths$beta < 0 & paths$significant, , drop = FALSE]
  med <- report_mediation(fit)$table

  text <- paste0(
    "The strongest direct predictor was ", strongest$rhs[[1]], " predicting ",
    strongest$lhs[[1]], " (beta = ", apa_num(strongest$beta[[1]]), "). ",
    if (nrow(risk) > 0) paste0("Positive significant paths may be interpreted as risk or amplifying factors when higher outcome scores are adverse. ") else "",
    if (nrow(protective) > 0) paste0("Negative significant paths may be interpreted as protective or buffering factors when higher outcome scores are adverse. ") else "",
    if (is.something(med) && nrow(med) > 0) "Defined indirect effects were available for mediation interpretation." else "No defined indirect effects were available."
  )

  list(strongest_predictors = strongest, key_mediators = med,
       risk_factors = risk, protective_factors = protective, text = text)
}

#' Modification-index diagnostics.
#'
#' @param fit A lavaan object.
#' @param threshold Minimum MI to report.
#' @return A list with table and text.
#' @export
diagnose_modification_indices <- function(fit, threshold = 10) {
  mi <- try_hard(lavaan::modindices(fit))
  if (!isFALSE(mi$error))
    return(list(table = data.frame(), text = paste("Modification indices could not be computed:", mi$error)))
  tab <- mi$obj
  tab <- tab[!is.na(tab$mi) & tab$mi >= threshold, , drop = FALSE]
  if (nrow(tab) > 0)
    tab <- tab[order(-tab$mi), , drop = FALSE]
  text <- if (nrow(tab) == 0) {
    paste0("No modification indices exceeded ", threshold, ".")
  } else {
    paste0(nrow(tab), " modification indices exceeded ", threshold,
           ". Treat these as diagnostics only; model changes should be theory-driven.")
  }
  list(table = tab, text = text)
}

#' P-curve style screening of model p-values.
#'
#' @param fit A lavaan object.
#' @param target Effect family to screen: "all", "direct", "mediation",
#'   "moderation", "multigroup", or "multilevel".
#' @param within Optional within-person or level-1 variable names used when
#'   \code{target = "multilevel"}.
#' @param between Optional between-person or level-2 variable names used when
#'   \code{target = "multilevel"}.
#' @return A list with p-values, summary, and text.
#' @export
p_curve_analysis <- function(fit, target = c("all", "direct", "mediation", "moderation", "multigroup", "multilevel"),
                             within = NULL, between = NULL) {
  target <- match.arg(target)
  pe <- lavaan::parameterestimates(fit)
  pe <- pe[!is.na(pe$pvalue), , drop = FALSE]
  if (target == "direct") {
    pe <- pe[pe$op == "~", , drop = FALSE]
  } else if (target == "mediation") {
    pe <- pe[pe$op == ":=", , drop = FALSE]
    if ("label" %in% names(pe) && nrow(pe) > 0) {
      label <- tolower(pe$label)
      indirect <- grepl("ind|med", label)
      if (any(indirect))
        pe <- pe[indirect, , drop = FALSE]
    }
  } else if (target == "moderation") {
    mod_label <- if ("label" %in% names(pe)) grepl("int|mod", tolower(pe$label)) else rep(FALSE, nrow(pe))
    pe <- pe[pe$op == "~" & (grepl("__XX__XX__|:|\\*", pe$rhs) | mod_label), , drop = FALSE]
  } else if (target == "multigroup") {
    pe <- pe[pe$op == "~" & "group" %in% names(pe) & !is.na(pe$group) & pe$group > 0, , drop = FALSE]
  } else if (target == "multilevel") {
    vars <- unique(c(within, between))
    pe <- pe[pe$op == "~" & (pe$lhs %in% vars | pe$rhs %in% vars), , drop = FALSE]
  } else {
    pe <- pe[pe$op %in% c("~", ":="), , drop = FALSE]
  }
  p <- pe$pvalue
  sig <- p[p < .05]
  summary <- data.frame(
    n_tests = length(p),
    n_significant = length(sig),
    prop_p_lt_025 = if (length(sig) > 0) mean(sig < .025) else NA_real_,
    stringsAsFactors = FALSE
  )
  label <- switch(target,
                  all = "all available effects",
                  direct = "direct paths",
                  mediation = "mediation or indirect effects",
                  moderation = "moderation or interaction paths",
                  multigroup = "multigroup paths",
                  multilevel = "multilevel-relevant paths")
  text <- if (length(sig) < 3) {
    paste0("Too few significant p-values were available for an informative p-curve screen of ", label, ".")
  } else if (summary$prop_p_lt_025 > .50) {
    paste0("The significant p-values for ", label, " show a right-skewed pattern consistent with evidential value, but this is only a descriptive screen.")
  } else {
    paste0("The significant p-values for ", label, " do not show a clear right-skewed pattern; interpret evidential value cautiously.")
  }
  list(p_values = p, summary = summary, text = text)
}

#' Compare lavaan models and select the best-supported model.
#'
#' @param ... lavaan fits or a single named list of fits.
#' @return A list with comparison table, nested comparison when available, and text.
#' @export
compare_models <- function(...) {
  fits <- list(...)
  if (length(fits) == 1 && is.list(fits[[1]]) && !inherits(fits[[1]], "lavaan"))
    fits <- fits[[1]]
  if (is.null(names(fits)) || any(names(fits) == ""))
    names(fits) <- paste0("Model ", seq_along(fits))

  measures <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
  rows <- lapply(seq_along(fits), function(i) {
    ff <- safe_fit_measures(fits[[i]], measures)
    data.frame(
      model = names(fits)[[i]],
      chisq = ff[["chisq"]],
      df = ff[["df"]],
      pvalue = ff[["pvalue"]],
      cfi = ff[["cfi"]],
      tli = ff[["tli"]],
      rmsea = ff[["rmsea"]],
      srmr = ff[["srmr"]],
      aic = ff[["aic"]],
      bic = ff[["bic"]],
      stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  criterion <- if (any(!is.na(tab$bic))) "bic" else if (any(!is.na(tab$aic))) "aic" else "cfi"
  if (criterion %in% c("bic", "aic")) {
    best_i <- which.min(ifelse(is.na(tab[[criterion]]), Inf, tab[[criterion]]))
  } else {
    best_i <- which.max(ifelse(is.na(tab[[criterion]]), -Inf, tab[[criterion]]))
  }
  if (length(best_i) == 0 || !is.finite(tab[[criterion]][[best_i]]))
    best_i <- 1
  tab$best_fit <- FALSE
  tab$best_fit[best_i] <- TRUE

  nested <- NULL
  if (length(fits) >= 2) {
    nested_try <- try_hard(do.call(stats::anova, fits))
    if (isFALSE(nested_try$error))
      nested <- nested_try$obj
  }

  text <- paste0(
    tab$model[[best_i]], " demonstrated the strongest comparative fit by lowest ",
    toupper(criterion), ". Model selection should consider both statistical fit and theoretical justification."
  )
  if (is.something(nested) && is.data.frame(nested) && "Pr(>Chisq)" %in% names(nested)) {
    p <- nested[["Pr(>Chisq)"]][length(nested[["Pr(>Chisq)"]])]
    if (!is.na(p)) {
      text <- paste0(text, " The nested-model chi-square difference test was ",
                     ifelse(p < .05, "significant, supporting the more complex model.",
                            "not significant, favoring the more parsimonious model."))
    }
  }

  list(table = tab, nested = nested, text = text)
}

#' Complete intelligent SEM report.
#'
#' @param fit A lavaan object.
#' @param data Optional original data for diagnostics.
#' @param teaching_mode One of "basic", "apa", or "advanced".
#' @param cluster Optional cluster variable name for multilevel diagnostics.
#' @param within Optional within-person or level-1 variable names.
#' @param between Optional between-person or level-2 variable names.
#' @param pcurve_target Effect family for the p-curve screen.
#' @return A structured report list.
#' @export
generate_report <- function(fit, data = NULL, teaching_mode = c("apa", "basic", "advanced"),
                            cluster = NULL, within = NULL, between = NULL,
                            pcurve_target = c("all", "direct", "mediation", "moderation", "multigroup", "multilevel")) {
  teaching_mode <- match.arg(teaching_mode)
  pcurve_target <- match.arg(pcurve_target)
  fit_report <- report_fit(fit)
  topology <- detect_mediation_topology(fit)
  conditional_effects <- conditional_process_table(fit, topology, data = data)
  moderated_index <- conditional_process_index(fit, topology)
  paths <- report_paths(fit, include_nonsignificant = teaching_mode == "advanced")
  mediation <- report_mediation(fit)
  moderation <- report_moderation(fit)
  multigroup <- report_multigroup(fit)
  multilevel <- report_multilevel(data = data, cluster = cluster, within = within, between = between)
  diagnostics <- check_assumptions(fit, data = data, cluster = cluster)
  insights <- generate_model_insights(fit)
  mi <- diagnose_modification_indices(fit)
  pcurve <- p_curve_analysis(fit, target = pcurve_target, within = within, between = between)

  estimator <- attr(fit, "pathj_estimator")
  if (is.null(estimator))
    estimator <- tryCatch(fit@Options$estimator, error = function(e) NA_character_)
  missing <- attr(fit, "pathj_missing_method")
  if (is.null(missing))
    missing <- tryCatch(fit@Options$missing, error = function(e) "lavaan default")
  ordered <- attr(fit, "pathj_ordered")
  estimator_text <- paste0("The model was estimated with ", estimator,
                           if (length(ordered) > 0) paste0(" and ordered variables: ", paste(ordered, collapse = ", ")) else "",
                           ". Missing data handling was: ", missing, ".")

  text <- paste(c(
    fit_report$text,
    paths$text,
    topology$text,
    mediation$text,
    moderation$text,
    multigroup$text,
    multilevel$text,
    estimator_text,
    insights$text
  ), collapse = "\n\n")

  conditional_text <- if (nrow(conditional_effects) > 0L) {
    paste0("Conditional indirect effects through the moderator were estimated at the observed probing values. ",
           paste(paste0(conditional_effects$moderator, " = ", apa_num(conditional_effects$value),
                        ": indirect effect = ", apa_num(conditional_effects$indirect)), collapse = "; "), ".")
  } else {
    reason <- if (length(topology$moderators) == 0L) {
      "it contains no moderator"
    } else if (length(topology$indirect_paths) == 0L) {
      "it contains no indirect pathway"
    } else {
      "the conditional effect could not be estimated from the fitted paths"
    }
    paste0("Conditional indirect effects were not defined for this model because ", reason, ".")
  }
  index_text <- if (is.finite(moderated_index$index[[1]])) {
    paste0("The index of moderated mediation was ", apa_num(moderated_index$index[[1]]), ". ", moderated_index$note[[1]])
  } else "The index of moderated mediation was not estimable for this model."
  text <- paste(text, conditional_text, index_text, sep = "\n\n")

  list(
    teaching_mode = teaching_mode,
    text = text,
    fit = fit_report,
    paths = paths,
    topology = topology,
    conditional_effects = conditional_effects,
    moderated_index = moderated_index,
    mediation = mediation,
    moderation = moderation,
    multigroup = multigroup,
    multilevel = multilevel,
    diagnostics = diagnostics,
    recommendations = generate_recommendations(diagnostics),
    insights = insights,
    modification_indices = mi,
    p_curve = pcurve
  )
}
