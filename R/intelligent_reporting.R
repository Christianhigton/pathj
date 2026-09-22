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

model_identification <- function(fit) {
  df <- unname(safe_fit_measures(fit, "df")[["df"]])
  converged <- tryCatch(isTRUE(lavaan::lavInspect(fit, "converged")),
                        error = function(e) FALSE)
  if (!is.finite(df) || df < 0 || !converged) {
    status <- "underidentified"
  } else if (df == 0) {
    status <- "just_identified"
  } else {
    status <- "overidentified"
  }
  list(status = status, df = df, converged = converged)
}

# A mediated proportion is descriptive only when the component effects operate
# in the same direction and the denominator is not dominated by cancellation.
mediation_proportion <- function(indirect, direct, total = direct + indirect,
                                 relative_tolerance = .05, max_abs_ratio = 1) {
  values <- c(indirect = indirect, direct = direct, total = total)
  if (any(!is.finite(values)))
    return(list(value = NA_real_, interpretable = FALSE,
                reason = "The direct, indirect, and total effects were not all estimable."))
  scale <- max(abs(c(direct, indirect)), .Machine$double.eps)
  if (abs(total) <= relative_tolerance * scale)
    return(list(value = NA_real_, interpretable = FALSE,
                reason = "The total effect is zero or too close to zero for a stable proportion."))
  if (direct * indirect < 0)
    return(list(value = NA_real_, interpretable = FALSE,
                reason = "The direct and indirect effects operate in opposite directions."))
  ratio <- indirect / total
  if (!is.finite(ratio) || ratio < 0 || abs(ratio) > max_abs_ratio)
    return(list(value = NA_real_, interpretable = FALSE,
                reason = "The indirect-to-total ratio is unstable or substantively misleading."))
  list(value = 100 * ratio, interpretable = TRUE, reason = "Interpretable")
}

#' Summarise simulation-based power from replicate p-values.
#'
#' @param simulations A data frame containing one row per valid simulated fit.
#' @param alpha Significance threshold.
#' @param effect_cols Columns identifying an effect (for example path and group).
#' @param sample_size_col Column containing candidate sample size.
#' @param pvalue_col Column containing the simulated p-value.
#' @return A data frame with estimated power and the number of valid simulations.
#' @export
simulation_power_summary <- function(simulations, alpha = .05,
                                     effect_cols = c("effect"),
                                     sample_size_col = "n",
                                     pvalue_col = "pvalue") {
  required <- unique(c(effect_cols, sample_size_col, pvalue_col))
  if (!is.data.frame(simulations) || !all(required %in% names(simulations)))
    stop("simulations must contain effect, sample-size, and p-value columns")
  d <- simulations[is.finite(simulations[[pvalue_col]]) &
                     !is.na(simulations[[pvalue_col]]), required, drop = FALSE]
  if (nrow(d) == 0L)
    return(data.frame())
  keys <- interaction(d[c(effect_cols, sample_size_col)], drop = TRUE, lex.order = TRUE)
  split_rows <- split(seq_len(nrow(d)), keys)
  rows <- lapply(split_rows, function(ii) {
    out <- d[ii[[1]], c(effect_cols, sample_size_col), drop = FALSE]
    p <- d[[pvalue_col]][ii]
    out$power <- sum(p < alpha) / length(p)
    out$valid_simulations <- length(p)
    out
  })
  ans <- do.call(rbind, rows)
  rownames(ans) <- NULL
  ans
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

  identification <- model_identification(fit)
  tab$identification <- identification$status
  tab$fit_informative <- identical(identification$status, "overidentified")

  indices <- paste0(
    "chi-square(", apa_num(tab$df, 0), ") = ", apa_num(tab$chisq), ", ",
    apa_p(tab$pvalue), ", CFI = ", apa_num(tab$cfi),
    ", TLI = ", apa_num(tab$tli), ", RMSEA = ", apa_num(tab$rmsea),
    " [", apa_num(tab$rmsea.ci.lower), ", ", apa_num(tab$rmsea.ci.upper),
    "], and SRMR = ", apa_num(tab$srmr), ".")
  text <- if (identical(identification$status, "just_identified")) {
    paste0(
      "Model identification: Just-identified model. The model has 0 degrees of freedom and therefore reproduces the observed covariance matrix exactly. ",
      "Global fit indices such as chi-square, CFI, TLI, RMSEA, and SRMR are not informative for evaluating model fit. ",
      "Interpretation should instead focus on parameter estimates, confidence intervals, indirect effects, and explained variance. ",
      "For transparency, the numerical indices were ", indices)
  } else if (identical(identification$status, "underidentified")) {
    paste0(
      "Model identification warning: the model appears underidentified or did not converge. ",
      "Global fit indices and parameter interpretations may be unreliable. Review model identification before interpreting the results. ",
      "Available numerical indices were ", indices)
  } else {
    paste0("Model fit was evaluated using chi-square, CFI, TLI, RMSEA, and SRMR. The model fit statistics were ", indices)
  }

  list(table = tab, text = text, identification = identification)
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
  paths$direction <- ifelse(paths$beta < 0, "negative", "positive")
  paths$interpretation <- ifelse(
    paths$significant,
    paste0("Significant ", paths$direction, " association: higher ", paths$rhs,
           " values were associated with ", ifelse(paths$beta < 0, "lower", "higher"),
           " ", paths$lhs, " values."),
    paste0(paths$rhs, " was not a statistically significant predictor of ", paths$lhs, ".")
  )

  keep <- intersect(c("lhs", "rhs", "group", "est", "se", "z", "pvalue",
                      "ci.lower", "ci.upper", "beta", "effect_size",
                      "significant", "direction", "interpretation"), names(paths))
  tab <- paths[, keep, drop = FALSE]

  text_rows <- paths[paths$significant | include_nonsignificant, , drop = FALSE]
  if (nrow(text_rows) == 0) {
    text <- "No direct paths were statistically significant at alpha = .05."
  } else {
    text <- paste(vapply(seq_len(nrow(text_rows)), function(i) {
      r <- text_rows[i, ]
      paste0(
        "Higher ", r$rhs, " values were associated with ",
        ifelse(r$beta < 0, "lower", "higher"), " ", r$lhs,
        " values, beta = ", apa_num(r$beta),
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

  is_indirect <- (grepl("ind|indirect|^ie|^ab$", defs$lhs, ignore.case = TRUE) |
    grepl("\\*", defs$rhs)) &
    !grepl("total|^direct$|^direct_|^cp$", defs$lhs, ignore.case = TRUE)
  med <- defs[is_indirect, , drop = FALSE]
  if (nrow(med) == 0)
    med <- defs

  ci_available <- is.finite(med$ci.lower) & is.finite(med$ci.upper)
  ci_supported <- ci_available & (med$ci.lower > 0 | med$ci.upper < 0)
  supported <- ifelse(ci_available, ci_supported,
                      !is.na(med$pvalue) & med$pvalue < .05)
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
    significant = supported,
    ci_supported = ci_supported,
    direct = NA_real_,
    total = NA_real_,
    percent_mediated = NA_real_,
    percent_mediated_interpretation = "Not available",
    stringsAsFactors = FALSE
  )

  topology <- tryCatch(detect_mediation_topology(fit), error = function(e) NULL)
  pe_paths <- pe[pe$op == "~", , drop = FALSE]
  simple_details <- ""
  if (is.list(topology) && identical(topology$model_class, "simple_mediation") &&
      length(topology$predictors) == 1L && length(topology$mediators) == 1L &&
      length(topology$outcomes) == 1L) {
    x <- topology$predictors[[1]]; m <- topology$mediators[[1]]; y <- topology$outcomes[[1]]
    get_path <- function(lhs, rhs) pe_paths[pe_paths$lhs == lhs & pe_paths$rhs == rhs, , drop = FALSE]
    a <- get_path(m, x); b <- get_path(y, m); direct <- get_path(y, x)
    direct_est <- if (nrow(direct)) direct$est[[1]] else NA_real_
    total_defs <- defs[grepl("total", defs$lhs, ignore.case = TRUE), , drop = FALSE]
    for (i in seq_len(nrow(tab))) {
      total_est <- if (nrow(total_defs)) total_defs$est[[1]] else direct_est + tab$indirect[[i]]
      proportion <- mediation_proportion(tab$indirect[[i]], direct_est, total_est)
      tab$direct[[i]] <- direct_est
      tab$total[[i]] <- total_est
      tab$percent_mediated[[i]] <- proportion$value
      tab$percent_mediated_interpretation[[i]] <- if (proportion$interpretable)
        "Interpretable" else paste0("Not interpretable: ", proportion$reason)
    }
    fmt_path <- function(label, row) {
      if (nrow(row) == 0L) return(paste0(label, " was not estimable"))
      paste0(label, " = ", apa_num(row$est[[1]]), ", 95% CI [",
             apa_num(row$ci.lower[[1]]), ", ", apa_num(row$ci.upper[[1]]), "], ",
             apa_p(row$pvalue[[1]]))
    }
    r2 <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(e) numeric(0))
    if (is.list(r2)) r2 <- r2[[1]]
    r2_text <- paste(c(
      if (m %in% names(r2)) paste0("R-squared for ", m, " = ", apa_num(r2[[m]])) else NULL,
      if (y %in% names(r2)) paste0("R-squared for ", y, " = ", apa_num(r2[[y]])) else NULL
    ), collapse = "; ")
    simple_details <- paste0(
      "A mediation model was tested. ", fmt_path("The a path", a), "; ",
      fmt_path("the b path", b), "; ", fmt_path("the direct effect", direct), "; ",
      "the total effect = ", apa_num(if (nrow(total_defs)) total_defs$est[[1]] else direct_est + tab$indirect[[1]]), ".",
      if (nzchar(r2_text)) paste0(" ", r2_text, ".") else "")
  }

  effect_text <- paste(vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    paste0(
      "The indirect effect ", r$effect, " was beta = ", apa_num(r$beta),
      ", 95% CI [", apa_num(r$ci.lower), ", ", apa_num(r$ci.upper),
      "], ", apa_p(r$pvalue), ". ",
      if (!is.na(r$direct) && r$direct * r$indirect < 0)
        "The direct and indirect effects had opposite signs, an inconsistent mediation or suppression-type pattern. "
      else "",
      if (isTRUE(r$significant))
        "The indirect effect was statistically supported."
      else
        "The indirect effect was not statistically supported; a mediation effect should not be inferred."
    )
  }, FUN.VALUE = character(1)), collapse = " ")
  text <- paste(c(simple_details, effect_text)[nzchar(c(simple_details, effect_text))], collapse = " ")

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
  paths$direction <- ifelse(paths$beta >= 0, "positive", "negative")
  paths$significant <- !is.na(paths$pvalue) & paths$pvalue < .05
  tab <- paths[, intersect(c("lhs", "rhs", "group", "est", "se", "z", "pvalue",
                             "ci.lower", "ci.upper", "beta", "direction",
                             "significant"), names(paths)), drop = FALSE]
  text <- paste(vapply(seq_len(nrow(paths)), function(i) {
    r <- paths[i, ]
    paste0(
      "The interaction term ", r$rhs, " predicting ", r$lhs,
      " had a ", r$direction, " coefficient, beta = ", apa_num(r$beta),
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

  nobs <- tryCatch(lavaan::lavInspect(fit, "nobs"), error = function(e) NA_real_)
  if (is.list(nobs)) nobs <- unlist(nobs, use.names = FALSE)
  nobs <- as.numeric(nobs)
  if (length(nobs) == 1L && length(group_labels) > 1L) nobs <- rep(NA_real_, length(group_labels))
  if (length(nobs) < length(group_labels)) length(nobs) <- length(group_labels)

  partable <- tryCatch(lavaan::parameterTable(fit), error = function(e) NULL)
  formal_test <- function(lhs, rhs, g1, g2, fallback_z) {
    if (!is.null(partable) && "plabel" %in% names(partable)) {
      hit <- partable$op == "~" & partable$lhs == lhs & partable$rhs == rhs &
        partable$group %in% c(g1, g2)
      labs <- partable$plabel[hit][match(c(g1, g2), partable$group[hit])]
      if (length(labs) == 2L && all(!is.na(labs)) && all(nzchar(labs)) && labs[[1]] != labs[[2]]) {
        wt <- tryCatch(lavaan::lavTestWald(fit, constraints = paste(labs[[1]], "==", labs[[2]])),
                       error = function(e) NULL)
        if (is.list(wt) && is.finite(wt$stat) && is.finite(wt$p.value))
          return(c(z = sign(fallback_z) * sqrt(wt$stat), pvalue = wt$p.value))
      } else if (length(labs) == 2L && identical(labs[[1]], labs[[2]])) {
        return(c(z = 0, pvalue = 1))
      }
    }
    c(z = fallback_z, pvalue = 2 * stats::pnorm(abs(fallback_z), lower.tail = FALSE))
  }

  keys <- unique(paste(paths$lhs, paths$rhs, sep = "\r"))
  rows <- lapply(keys, function(key) {
    parts <- strsplit(key, "\r", fixed = TRUE)[[1]]
    x <- paths[paths$lhs == parts[[1]] & paths$rhs == parts[[2]], , drop = FALSE]
    x <- x[order(x$group), , drop = FALSE]
    if (nrow(x) < 2)
      return(NULL)
    pairs <- utils::combn(seq_len(nrow(x)), 2L, simplify = FALSE)
    do.call(rbind, lapply(pairs, function(pair) {
      g1 <- x[pair[[1]], ]; g2 <- x[pair[[2]], ]
      diff <- g1$est - g2$est
      se_diff <- sqrt(g1$se^2 + g2$se^2)
      fallback_z <- diff / se_diff
      test <- formal_test(parts[[1]], parts[[2]], g1$group, g2$group, fallback_z)
      p <- unname(test[["pvalue"]]); z <- unname(test[["z"]])
      g1_sig <- !is.na(g1$pvalue) && g1$pvalue < .05
      g2_sig <- !is.na(g2$pvalue) && g2$pvalue < .05
      diff_sig <- is.finite(p) && p < .05
      interpretation <- if (diff_sig) {
        "The formal comparison indicated a statistically significant between-group difference."
      } else if (xor(g1_sig, g2_sig)) {
        "The pathway reached statistical significance in one group but not another; however, the direct comparison of path coefficients did not indicate a statistically significant between-group difference."
      } else {
        "The formal comparison did not indicate a statistically significant between-group difference."
      }
      data.frame(
        path = paste(parts[[1]], "~", parts[[2]]),
        group_1 = group_labels[[g1$group]], group_1_n = nobs[[g1$group]],
        group_2 = group_labels[[g2$group]], group_2_n = nobs[[g2$group]],
        group_1_beta = g1$std.all, group_1_pvalue = g1$pvalue,
        group_2_beta = g2$std.all, group_2_pvalue = g2$pvalue,
        difference = diff, z = z, pvalue = p,
        significant = diff_sig, interpretation = interpretation,
        stringsAsFactors = FALSE
      )
    }))
  })
  tab <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (!is.something(tab))
    tab <- data.frame()

  size_text <- paste0("Group sample sizes: ",
                      paste(paste0(group_labels, " (n = ", nobs[seq_along(group_labels)], ")"), collapse = ", "), ".")
  small_warning <- if (any(is.finite(nobs) & nobs < 20))
    " One or more groups contain fewer than 20 observations. Group-specific path estimates and standard errors may be unstable. Treat these results as exploratory."
  else ""
  if (nrow(tab) == 0 || !any(tab$significant)) {
    detail <- if (nrow(tab) > 0 && any(grepl("one group", tab$interpretation, fixed = TRUE)))
      paste(unique(tab$interpretation[grepl("one group", tab$interpretation, fixed = TRUE)]), collapse = " ")
    else "No statistically significant multigroup differences in regression paths were detected."
  } else {
    sig <- tab[tab$significant, , drop = FALSE]
    detail <- paste(vapply(seq_len(nrow(sig)), function(i) {
      r <- sig[i, ]
      paste0("The formal comparison of the ", r$path, " path between ", r$group_1,
             " and ", r$group_2, " was significant, z = ", apa_num(r$z), ", ", apa_p(r$pvalue), ".")
    }, FUN.VALUE = character(1)), collapse = " ")
  }
  text <- paste0(size_text, small_warning, " ", detail)

  list(table = tab, text = text,
       group_sizes = data.frame(group = group_labels, n = nobs[seq_along(group_labels)],
                                small_group = nobs[seq_along(group_labels)] < 20,
                                stringsAsFactors = FALSE))
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
  identification <- model_identification(fit)
  ff <- safe_fit_measures(fit, c("cfi", "tli", "rmsea", "srmr"))
  ok_fit <- (is.na(ff[["cfi"]]) || ff[["cfi"]] >= .90) &&
    (is.na(ff[["rmsea"]]) || ff[["rmsea"]] <= .08) &&
    (is.na(ff[["srmr"]]) || ff[["srmr"]] <= .08)
  if (identical(identification$status, "just_identified")) {
    rows[[length(rows) + 1]] <- diagnostic_row(
      "Global model fit", "Info", "Not evaluable: the model is just-identified (df = 0).",
      "Interpret path coefficients, indirect effects, confidence intervals, and R-squared rather than global fit indices.")
  } else if (identical(identification$status, "underidentified")) {
    rows[[length(rows) + 1]] <- diagnostic_row(
      "Global model fit", "Violated",
      "The model appears underidentified or did not converge; global fit is not interpretable.",
      "Resolve model identification and convergence before interpreting fit or parameters.")
  } else {
    rows[[length(rows) + 1]] <- diagnostic_row(
      "Global model fit", ifelse(ok_fit, "Met", "Warning"),
      paste0("CFI = ", apa_num(ff[["cfi"]]), ", RMSEA = ", apa_num(ff[["rmsea"]]),
             ", SRMR = ", apa_num(ff[["srmr"]]), "."),
      ifelse(ok_fit, "Report fit indices and proceed with substantive interpretation.",
             "Inspect residuals, theory, and modification indices before changing the model."))
  }

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
    estimator <- toupper(as.character(attr(fit, "pathj_estimator") %||%
                                        tryCatch(fit@Options$estimator, error = function(e) "")))
    normal_recommendation <- if (normal_warn && estimator %in% c("ML", "MLR", "MLM", "MLMV", "MLF")) {
      "Consider robust ML/MLR where available, bootstrap confidence intervals for indirect effects, and sensitivity analysis."
    } else if (normal_warn) {
      "Use an estimator appropriate to the variable distributions and consider bootstrap confidence intervals and sensitivity analysis."
    } else {
      "No severe violation was detected by automated screening; still inspect distributions and estimator robustness."
    }
    rows[[length(rows) + 1]] <- diagnostic_row(
      "Multivariate normality",
      ifelse(normal_warn, "Warning", "Info"),
      paste0("Maximum absolute skew = ", apa_num(max_skew),
             "; maximum excess kurtosis = ", apa_num(max_kurt),
             "; Mardia skew = ", apa_num(mardia[["skew"]]),
             "; Mardia kurtosis = ", apa_num(mardia[["kurtosis"]]), "."),
      normal_recommendation
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
#' @return A list with strongest predictors, mediators, signed-association tables, and text.
#' @export
generate_model_insights <- function(fit) {
  paths <- report_paths(fit, include_nonsignificant = TRUE)$table
  if (!is.something(paths) || nrow(paths) == 0)
    return(list(text = "No direct paths were available for insight generation."))

  paths$abs_beta <- abs(paths$beta)
  paths <- paths[order(-paths$abs_beta), , drop = FALSE]
  strongest <- paths[seq_len(min(3, nrow(paths))), , drop = FALSE]
  positive <- paths[!is.na(paths$beta) & paths$beta > 0 & paths$significant, , drop = FALSE]
  negative <- paths[!is.na(paths$beta) & paths$beta < 0 & paths$significant, , drop = FALSE]
  med <- report_mediation(fit)$table

  text <- paste0(
    "The strongest direct predictor was ", strongest$rhs[[1]], " predicting ",
    strongest$lhs[[1]], " (beta = ", apa_num(strongest$beta[[1]]), "). ",
    if (nrow(positive) > 0) "Positive coefficients indicate that higher predictor values were associated with higher outcome values; their desirability cannot be inferred without knowing the outcome coding. " else "",
    if (nrow(negative) > 0) "Negative coefficients indicate that higher predictor values were associated with lower outcome values; the practical meaning depends on how the outcome is coded. " else "",
    if (is.something(med) && nrow(med) > 0 && any(med$significant))
      "At least one indirect effect was statistically supported."
    else if (is.something(med) && nrow(med) > 0)
      "A mediation model was tested, but no indirect effect was statistically supported."
    else "No defined indirect effects were available."
  )

  list(strongest_predictors = strongest, key_mediators = med,
       positive_associations = positive, negative_associations = negative, text = text)
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
    identification <- model_identification(fits[[i]])$status
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
      identification = identification,
      fit_note = if (identification == "just_identified")
        "Global fit indices are not informative because df = 0."
      else if (identification == "underidentified")
        "Model identification/convergence must be resolved before fit is interpreted."
      else "Global fit indices are available for evaluation.",
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

  text <- if (length(fits) == 1L) {
    "Fit and information criteria are shown for the current model; comparative model selection requires at least two fitted models."
  } else {
    paste0(tab$model[[best_i]], " had the strongest comparative support by lowest ",
           toupper(criterion), ". Model selection should consider both statistical evidence and theoretical justification.")
  }
  if (any(tab$identification == "just_identified"))
    text <- paste0(text, " Just-identified models have df = 0; their global fit indices are not substantively interpretable.")
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
