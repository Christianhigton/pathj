missing_method_label <- function(method) {
  labels <- c(
    fiml = "Full Information Maximum Likelihood (FIML)",
    mi = "Multiple Imputation (MI)",
    listwise = "Listwise Deletion",
    pairwise = "Pairwise Deletion"
  )
  unname(labels[[method]])
}

mi_strategy_label <- function(strategy) {
  labels <- c(
    include_group = "Include group as predictor",
    within_group = "Impute within groups"
  )
  if (!strategy %in% names(labels))
    strategy <- "include_group"
  unname(labels[[strategy]])
}

model_variable_names <- function(lav_structure, group = NULL) {
  vars <- unique(c(lav_structure$lhs, lav_structure$rhs))
  vars <- vars[!is.na(vars) & vars != "" & vars != "1"]
  if (is.something(group))
    vars <- unique(c(vars, group))
  vars
}

missing_data_summary <- function(data, vars, display_vars = NULL) {
  vars <- intersect(vars, names(data))
  if (!is.something(display_vars))
    display_vars <- vars
  display_vars <- display_vars[seq_along(vars)]
  n <- nrow(data)
  missing <- vapply(vars, function(v) sum(is.na(data[[v]])), numeric(1))
  data.frame(
    variable = display_vars,
    missing = as.integer(missing),
    percent = if (n > 0) 100 * missing / n else NA_real_,
    stringsAsFactors = FALSE
  )
}

missing_data_guidance <- function() {
  paste(
    "If you expected missing data, check that blank cells and user-defined missing-value codes,",
    "such as 99, -99, 'Missing', or similar values, have been correctly defined as missing in jamovi before interpreting these diagnostics."
  , sep = "\n")
}

missing_pattern_summary <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (!is.something(vars) || nrow(data) == 0)
    return(data.frame(pattern = character(0), n = integer(0), percent = numeric(0)))

  mat <- is.na(data[, vars, drop = FALSE])
  patterns <- apply(mat, 1, function(x) paste(ifelse(x, "M", "."), collapse = ""))
  tab <- sort(table(patterns), decreasing = TRUE)
  data.frame(
    pattern = names(tab),
    n = as.integer(tab),
    percent = 100 * as.integer(tab) / nrow(data),
    stringsAsFactors = FALSE
  )
}

missing_pattern_plot_data <- function(data, vars, display_vars = NULL, max_patterns = 20) {
  if (!is.something(display_vars))
    display_vars <- vars

  keep <- vars %in% names(data)
  vars <- vars[keep]
  display_vars <- as.character(display_vars[keep])

  if (!is.something(vars) || nrow(data) == 0) {
    return(data.frame(
      pattern = character(0),
      variable = character(0),
      missing = logical(0),
      n = integer(0),
      percent = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  mat <- is.na(data[, vars, drop = FALSE])
  pattern_codes <- apply(mat, 1, function(x) paste(ifelse(x, "M", "."), collapse = ""))
  tab <- sort(table(pattern_codes), decreasing = TRUE)
  if (length(tab) > max_patterns)
    tab <- tab[seq_len(max_patterns)]

  rows <- list()
  for (i in seq_along(tab)) {
    code <- names(tab)[[i]]
    status <- strsplit(code, "", fixed = TRUE)[[1]] == "M"
    count <- as.integer(tab[[i]])
    pct <- 100 * count / nrow(data)
    label <- sprintf("Pattern %d  n=%d (%.1f%%)", i, count, pct)
    rows[[length(rows) + 1]] <- data.frame(
      pattern = label,
      variable = display_vars,
      missing = status,
      n = count,
      percent = pct,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

mcar_diagnostic <- function(data, vars) {
  vars <- intersect(vars, names(data))
  recognized_missing <- if (is.something(vars)) {
    sum(is.na(data[, vars, drop = FALSE]))
  } else {
    0L
  }

  if (recognized_missing == 0L) {
    return(data.frame(
      test = "Little's MCAR test",
      statistic = NA_real_,
      df = NA_integer_,
      pvalue = NA_real_,
      note = paste(
        "No recognised missing values were detected. Little's MCAR test was not performed because there were no recognised missing-data patterns to evaluate.",
        paste0("Guidance:\n", missing_data_guidance()),
        sep = "\n\n"
      ),
      stringsAsFactors = FALSE
    ))
  }

  numeric_vars <- vars[vapply(data[, vars, drop = FALSE], is.numeric, logical(1))]
  if (length(numeric_vars) < 2) {
    return(data.frame(
      test = "Little's MCAR test",
      statistic = NA_real_,
      df = NA_integer_,
      pvalue = NA_real_,
      note = paste("Little's MCAR test could not be estimated from the available missing-data patterns.",
                   "At least two numeric variables are required."),
      stringsAsFactors = FALSE
    ))
  }

  if (!requireNamespace("naniar", quietly = TRUE)) {
    return(data.frame(
      test = "Little's MCAR test",
      statistic = NA_real_,
      df = NA_integer_,
      pvalue = NA_real_,
      note = paste("Little's MCAR test could not be estimated from the available missing-data patterns.",
                   "Install the naniar package to compute this diagnostic."),
      stringsAsFactors = FALSE
    ))
  }

  res <- try_hard(naniar::mcar_test(data[, numeric_vars, drop = FALSE]))
  if (!isFALSE(res$error)) {
    return(data.frame(
      test = "Little's MCAR test",
      statistic = NA_real_,
      df = NA_integer_,
      pvalue = NA_real_,
      note = paste("Little's MCAR test could not be estimated from the available missing-data patterns.",
                   as.character(res$error)),
      stringsAsFactors = FALSE
    ))
  }

  out <- as.data.frame(res$obj)
  statistic <- if ("statistic" %in% names(out)) suppressWarnings(as.numeric(out$statistic[[1]])) else NA_real_
  df <- if ("df" %in% names(out)) suppressWarnings(as.numeric(out$df[[1]])) else NA_real_
  pvalue <- if ("p.value" %in% names(out)) suppressWarnings(as.numeric(out$p.value[[1]])) else NA_real_
  valid <- length(statistic) == 1L && length(df) == 1L && length(pvalue) == 1L &&
    is.finite(statistic) && is.finite(df) && is.finite(pvalue) && df > 0

  if (!valid) {
    return(data.frame(
      test = "Little's MCAR test",
      statistic = NA_real_,
      df = NA_integer_,
      pvalue = NA_real_,
      note = "Little's MCAR test could not be estimated from the available missing-data patterns.",
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    test = "Little's MCAR test",
    statistic = statistic,
    df = as.integer(df),
    pvalue = pvalue,
    note = "Large p-values are consistent with MCAR; this test is only a screening aid.",
    stringsAsFactors = FALSE
  )
}

handle_missing_data <- function(data, vars, options, group = NULL) {
  method <- options$missing
  if (!is.something(method))
    method <- "fiml"

  vars <- intersect(vars, names(data))
  analysis_vars <- vars
  missing_cases <- if (is.something(analysis_vars)) !stats::complete.cases(data[, analysis_vars, drop = FALSE]) else rep(FALSE, nrow(data))
  total_missing <- if (is.something(analysis_vars)) sum(is.na(data[, analysis_vars, drop = FALSE])) else 0
  removed <- sum(missing_cases)

  info <- list(
    method = method,
    method_label = missing_method_label(method),
    n_original = nrow(data),
    n_used = nrow(data),
    n_removed = 0,
    total_missing = total_missing,
    percent_removed = if (nrow(data) > 0) 100 * removed / nrow(data) else 0,
    imputations = NA_integer_,
    successful_imputations = NA_integer_,
    mi_strategy = NA_character_,
    mi_strategy_label = "",
    all_available = FALSE
  )

  warnings <- character(0)
  lav_missing <- NULL
  imputed_data <- NULL

  if (method == "fiml") {
    if (options$estimator == "ML") {
      lav_missing <- "fiml"
      info$all_available <- TRUE
    } else {
      warnings <- c(warnings, "FIML requires ML estimation in lavaan; listwise deletion was used for this estimator.")
      data <- data[!missing_cases, , drop = FALSE]
      info$n_used <- nrow(data)
      info$n_removed <- removed
      method <- "listwise"
      info$method <- method
      info$method_label <- missing_method_label(method)
    }
  } else if (method == "listwise") {
    data <- data[!missing_cases, , drop = FALSE]
    info$n_used <- nrow(data)
    info$n_removed <- removed
    if (info$percent_removed > 20)
      warnings <- c(warnings, "Listwise deletion removed more than 20% of cases. Consider FIML or multiple imputation.")
    else if (info$percent_removed > 10)
      warnings <- c(warnings, "Listwise deletion removed more than 10% of cases. Check whether this is acceptable.")
  } else if (method == "pairwise") {
    lav_missing <- "pairwise"
    info$all_available <- TRUE
    warnings <- c(warnings, "Pairwise deletion can bias estimates and may produce non-positive definite covariance matrices.")
  } else if (method == "mi") {
    if (!requireNamespace("mice", quietly = TRUE))
      stop("Multiple imputation requires the mice package. Install mice or choose FIML/Listwise Deletion.")

    m <- as.integer(options$miN)
    if (is.na(m) || m < 2)
      m <- 5

    seed <- as.integer(options$miSeed)
    if (is.na(seed))
      seed <- 12345

    strategy <- options$miStrategy
    if (!is.something(strategy))
      strategy <- "include_group"
    if (!is.something(group))
      strategy <- "include_group"
    if (!strategy %in% c("include_group", "within_group"))
      strategy <- "include_group"

    impute_data <- data[, unique(c(analysis_vars, group)), drop = FALSE]
    if (strategy == "within_group" && is.something(group)) {
      group_values <- unique(impute_data[[group]])
      group_values <- group_values[!is.na(group_values)]
      group_imputations <- vector("list", length(group_values))
      impute_vars <- setdiff(names(impute_data), group)

      for (g in seq_along(group_values)) {
        rows <- which(!is.na(impute_data[[group]]) & impute_data[[group]] == group_values[[g]])
        part <- impute_data[rows, impute_vars, drop = FALSE]
        if (nrow(part) < 2 || !anyNA(part)) {
          group_imputations[[g]] <- replicate(m, part, simplify = FALSE)
        } else {
          methods <- mice::make.method(part)
          imp <- mice::mice(part, m = m, method = methods, seed = seed + g - 1, printFlag = FALSE)
          group_imputations[[g]] <- lapply(seq_len(m), function(i) mice::complete(imp, action = i))
        }
        attr(group_imputations[[g]], "rows") <- rows
      }

      imputed_data <- lapply(seq_len(m), function(i) {
        out <- data
        for (g in seq_along(group_values)) {
          rows <- attr(group_imputations[[g]], "rows")
          out[rows, impute_vars] <- group_imputations[[g]][[i]]
        }
        out
      })
    } else {
      methods <- mice::make.method(impute_data)
      if (is.something(group) && group %in% names(methods))
        methods[[group]] <- ""
      imp <- mice::mice(impute_data, m = m, method = methods, seed = seed, printFlag = FALSE)
      imputed_data <- lapply(seq_len(m), function(i) {
        completed <- mice::complete(imp, action = i)
        out <- data
        out[, names(completed)] <- completed
        out
      })
    }

    info$n_used <- nrow(data)
    info$imputations <- m
    info$mi_strategy <- strategy
    info$mi_strategy_label <- mi_strategy_label(strategy)
  }

  if (is.something(analysis_vars))
    data <- data[, analysis_vars, drop = FALSE]
  if (is.something(imputed_data)) {
    imputed_data <- lapply(imputed_data, function(d) {
      d[, analysis_vars, drop = FALSE]
    })
  }

  list(
    data = data,
    imputed_data = imputed_data,
    lav_missing = lav_missing,
    info = info,
    warnings = warnings
  )
}

with_safe_lavaan_cores <- function(expr) {
  cores <- parallel::detectCores()
  if (!is.na(cores))
    return(force(expr))

  ns <- asNamespace("parallel")
  old <- get("detectCores", envir = ns)
  unlockBinding("detectCores", ns)
  assign("detectCores", function(...) 1L, envir = ns)
  lockBinding("detectCores", ns)
  on.exit({
    unlockBinding("detectCores", ns)
    assign("detectCores", old, envir = ns)
    lockBinding("detectCores", ns)
  }, add = TRUE)

  force(expr)
}

run_sem_model <- function(lavoptions) {
  try_hard({ with_safe_lavaan_cores(do.call(lavaan::lavaan, lavoptions)) })
}

pool_results_if_needed <- function(param_list, ci = TRUE, level = .95) {
  m <- length(param_list)
  if (m == 0)
    return(NULL)
  if (m == 1)
    return(param_list[[1]])

  key_cols <- intersect(c("lhs", "op", "rhs", "group", "label"), names(param_list[[1]]))
  key <- Reduce(intersect, lapply(param_list, function(x) do.call(paste, c(x[, key_cols, drop = FALSE], sep = "\r"))))
  aligned <- lapply(param_list, function(x) {
    row_key <- do.call(paste, c(x[, key_cols, drop = FALSE], sep = "\r"))
    x[match(key, row_key), , drop = FALSE]
  })

  out <- aligned[[1]]
  est <- sapply(aligned, function(x) x$est)
  se <- sapply(aligned, function(x) x$se)
  qbar <- rowMeans(est, na.rm = TRUE)
  ubar <- rowMeans(se^2, na.rm = TRUE)
  b <- apply(est, 1, stats::var, na.rm = TRUE)
  b[is.na(b)] <- 0
  total <- ubar + (1 + 1 / m) * b
  pooled_se <- sqrt(total)
  lambda <- ((1 + 1 / m) * b) / total
  df <- (m - 1) / lambda^2
  df[!is.finite(df)] <- Inf
  crit <- stats::qt((1 + level) / 2, df = df)

  out$est <- qbar
  out$se <- pooled_se
  out$z <- qbar / pooled_se
  out$pvalue <- 2 * stats::pt(abs(out$z), df = df, lower.tail = FALSE)
  if ("std.all" %in% names(out))
    out$std.all <- rowMeans(sapply(aligned, function(x) x$std.all), na.rm = TRUE)
  if (ci && all(c("ci.lower", "ci.upper") %in% names(out))) {
    out$ci.lower <- qbar - crit * pooled_se
    out$ci.upper <- qbar + crit * pooled_se
  }
  out
}

effect_stars <- function(p) {
  vapply(p, function(x) {
    if (is.na(x)) return("")
    if (x < .001) return("***")
    if (x < .01) return("**")
    if (x < .05) return("*")
    ""
  }, FUN.VALUE = character(1))
}

fit_measures_table <- function(models, imputation = seq_along(models), measures = c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic")) {
  rows <- lapply(seq_along(models), function(i) {
    ff <- lavaan::fitmeasures(models[[i]], fit.measures = measures)
    data.frame(
      imputation = imputation[[i]],
      chisq = unname(ff[["chisq"]]),
      df = unname(ff[["df"]]),
      pvalue = unname(ff[["pvalue"]]),
      cfi = unname(ff[["cfi"]]),
      tli = unname(ff[["tli"]]),
      rmsea = unname(ff[["rmsea"]]),
      srmr = unname(ff[["srmr"]]),
      aic = unname(ff[["aic"]]),
      bic = unname(ff[["bic"]]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

fit_measures_summary <- function(tab) {
  if (!is.something(tab))
    return(NULL)
  numeric_cols <- setdiff(names(tab), "imputation")
  stat_row <- function(label, fun) {
    vals <- vapply(tab[, numeric_cols, drop=FALSE], function(x) fun(x, na.rm=TRUE), numeric(1))
    data.frame(statistic=label, as.list(vals), check.names=FALSE, stringsAsFactors=FALSE)
  }
  do.call(rbind, list(
    stat_row("Mean", mean),
    stat_row("SD", stats::sd),
    stat_row("Min", min),
    stat_row("Max", max)
  ))
}

pooled_mi_fit_table <- function(imputed_data, lavoptions, pool_method = "D4") {
  empty <- list(table = NULL, warning = NULL)
  if (!requireNamespace("lavaan.mi", quietly = TRUE)) {
    empty$warning <- "Formal pooled MI fit requires the lavaan.mi package. Descriptive fit summaries are shown instead."
    return(empty)
  }

  opts <- lavoptions
  opts[["data"]] <- imputed_data
  opts[["ncpus"]] <- 1
  opts[["parallel"]] <- "no"

  fit <- try_hard({
    with_safe_lavaan_cores(do.call(lavaan.mi::lavaan.mi, opts))
  })
  if (!isFALSE(fit$error)) {
    empty$warning <- paste("Formal pooled MI fit could not be computed:", fit$error)
    return(empty)
  }

  measures <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
  ff <- try_hard({
    lavaan::fitMeasures(
      fit$obj,
      fit.measures = measures,
      asymptotic = TRUE,
      pool.method = pool_method
    )
  })
  if (!isFALSE(ff$error)) {
    ff <- try_hard({
      lavaan::fitMeasures(fit$obj, fit.measures = measures)
    })
  }
  if (!isFALSE(ff$error)) {
    empty$warning <- paste("Formal pooled MI fit measures could not be computed:", ff$error)
    return(empty)
  }

  vals <- ff$obj
  table <- data.frame(
    method = pool_method,
    chisq = unname(vals[["chisq"]]),
    df = unname(vals[["df"]]),
    pvalue = unname(vals[["pvalue"]]),
    cfi = unname(vals[["cfi"]]),
    tli = unname(vals[["tli"]]),
    rmsea = unname(vals[["rmsea"]]),
    srmr = unname(vals[["srmr"]]),
    aic = unname(vals[["aic"]]),
    bic = unname(vals[["bic"]]),
    note = "Computed by lavaan.mi from pooled MI test statistics",
    stringsAsFactors = FALSE
  )

  list(table = table, warning = NULL)
}
