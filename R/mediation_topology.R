# Shared mediation and conditional-process topology helpers.

mediation_interaction_components <- function(rhs) {
  rhs <- as.character(rhs %||% "")
  if (!nzchar(rhs))
    return(character(0))
  parts <- unlist(strsplit(rhs, "__XX__XX__|:|\\*", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

mediation_path_key <- function(path) paste(path, collapse = " -> ")

mediation_graph_paths <- function(edges, max_length = 8L) {
  if (!is.data.frame(edges) || nrow(edges) == 0)
    return(list())

  paths <- list()
  walk <- function(path) {
    current <- path[[length(path)]]
    next_edges <- edges[edges$from == current, , drop = FALSE]
    if (nrow(next_edges) == 0 || length(path) >= max_length)
      return()
    for (i in seq_len(nrow(next_edges))) {
      nxt <- next_edges$to[[i]]
      if (nxt %in% path)
        next
      next_path <- c(path, nxt)
      # Only retain complete paths ending at a sink. Prefixes of a serial
      # chain are not separate indirect effects.
      if (length(next_path) >= 3L && !any(edges$from == nxt))
        paths[[length(paths) + 1L]] <<- next_path
      walk(next_path)
    }
  }

  source_nodes <- setdiff(unique(edges$from), unique(edges$to))
  for (node in source_nodes)
    walk(node)
  unique(paths)
}

detect_mediation_topology <- function(fit) {
  pe <- tryCatch(lavaan::parameterEstimates(fit, standardized = TRUE),
                 error = function(e) data.frame())
  if (!is.data.frame(pe) || nrow(pe) == 0) {
    return(list(
      model_class = "unclassified",
      display_name = "Unclassified or custom path model",
      predictors = character(0), outcomes = character(0), mediators = character(0),
      moderators = character(0), indirect_paths = list(), moderated_paths = list(),
      interaction_terms = character(0), process_equivalent = NA_character_,
      process_match = "no direct equivalent", text = "No estimable path topology was available."
    ))
  }

  paths <- pe[pe$op == "~" & !is.na(pe$lhs) & !is.na(pe$rhs), , drop = FALSE]
  interaction_rows <- paths[vapply(paths$rhs, function(x) length(mediation_interaction_components(x)) > 1L, logical(1)), , drop = FALSE]
  direct_rows <- paths[nrow(paths) == 0L | !seq_len(nrow(paths)) %in% which(paths$rhs %in% interaction_rows$rhs), , drop = FALSE]
  edges <- unique(data.frame(from = as.character(direct_rows$rhs),
                              to = as.character(direct_rows$lhs),
                              stringsAsFactors = FALSE))
  edges <- edges[nzchar(edges$from) & nzchar(edges$to) & edges$from != "1", , drop = FALSE]
  indirect_paths <- mediation_graph_paths(edges)

  interaction_terms <- unique(as.character(interaction_rows$rhs))
  moderators <- unique(unlist(lapply(interaction_terms, function(x) {
    comps <- mediation_interaction_components(x)
    if (length(comps) > 1L) comps[[length(comps)]] else character(0)
  }), use.names = FALSE))
  moderated_paths <- lapply(seq_len(nrow(interaction_rows)), function(i) {
    row <- interaction_rows[i, , drop = FALSE]
    comps <- mediation_interaction_components(row$rhs[[1]])
    list(from = comps[[1]], to = row$lhs[[1]], moderator = if (length(comps) > 1L) comps[[2]] else NA_character_,
         term = row$rhs[[1]], label = if ("label" %in% names(row)) row$label[[1]] else "")
  })

  if (length(moderated_paths) > 0L)
    moderated_paths <- Filter(function(x) !is.na(x$moderator), moderated_paths)

  predictors <- unique(vapply(indirect_paths, `[[`, character(1), 1L))
  outcomes <- unique(vapply(indirect_paths, function(x) x[[length(x)]], character(1)))
  mediators <- unique(unlist(lapply(indirect_paths, function(x) if (length(x) > 2L) x[-c(1, length(x))] else character(0)), use.names = FALSE))

  has_indirect <- length(indirect_paths) > 0L
  has_serial <- any(vapply(indirect_paths, length, integer(1)) > 3L)
  has_parallel <- length(indirect_paths) > 1L && any(vapply(indirect_paths, length, integer(1)) == 3L)
  has_first_stage <- any(vapply(moderated_paths, function(x) x$to %in% mediators && x$from %in% predictors, logical(1)))
  has_second_stage <- any(vapply(moderated_paths, function(x) x$to %in% outcomes && x$from %in% mediators, logical(1)))
  has_direct_moderation <- any(vapply(moderated_paths, function(x) x$to %in% outcomes && x$from %in% predictors, logical(1)))
  has_mediated_moderation <- any(vapply(moderated_paths, function(x) x$to %in% mediators, logical(1))) && has_indirect

  if (has_first_stage && has_second_stage) {
    model_class <- "conditional_process"
    display_name <- "Conditional process model (moderated a and b paths)"
  } else if (has_first_stage) {
    model_class <- "first_stage_moderated_mediation"
    display_name <- "First-stage moderated mediation"
  } else if (has_second_stage) {
    model_class <- "second_stage_moderated_mediation"
    display_name <- "Second-stage moderated mediation"
  } else if (has_mediated_moderation) {
    model_class <- "mediated_moderation"
    display_name <- "Mediated moderation"
  } else if (has_direct_moderation && has_indirect) {
    model_class <- "moderated_direct_effect_with_mediation"
    display_name <- "Mediation with a moderated direct effect"
  } else if (has_serial && has_parallel) {
    model_class <- "combined_parallel_serial_mediation"
    display_name <- "Combined parallel and serial mediation"
  } else if (has_serial) {
    model_class <- "serial_mediation"
    display_name <- "Serial mediation"
  } else if (has_parallel) {
    model_class <- "parallel_mediation"
    display_name <- "Parallel multiple mediation"
  } else if (has_indirect) {
    model_class <- "simple_mediation"
    display_name <- "Simple mediation"
  } else if (length(moderated_paths) > 0L) {
    model_class <- "moderation"
    display_name <- "Moderation"
  } else {
    model_class <- "custom_path_model"
    display_name <- "Unclassified or custom path model"
  }

  process_equivalent <- NA_character_
  process_match <- "no direct equivalent"
  if (model_class == "moderation" && length(moderated_paths) == 1L) {
    process_equivalent <- "Model 1"
    process_match <- "exact"
  } else if (model_class == "simple_mediation" || model_class == "parallel_mediation") {
    process_equivalent <- "Model 4"
    process_match <- "closest equivalent"
  } else if (model_class == "serial_mediation" && length(indirect_paths) == 1L) {
    process_equivalent <- "Model 6"
    process_match <- "closest equivalent"
  } else if (model_class == "first_stage_moderated_mediation") {
    process_equivalent <- "Model 7"
    process_match <- if (!has_direct_moderation) "closest equivalent" else "closest equivalent with additional paths"
  } else if (model_class == "second_stage_moderated_mediation") {
    process_equivalent <- "Model 14"
    process_match <- if (!has_direct_moderation) "closest equivalent" else "closest equivalent with additional paths"
  } else if (model_class == "conditional_process") {
    process_equivalent <- "Model 58"
    process_match <- "closest equivalent with additional paths"
  }

  path_text <- if (has_indirect) paste(vapply(indirect_paths, mediation_path_key, character(1)), collapse = "; ") else "none detected"
  text <- paste0("The specified model was identified as ", display_name, ". ",
                 if (has_indirect) paste0("Indirect pathways were detected: ", path_text, ". ") else "No indirect pathway was detected. ",
                 if (length(moderated_paths) > 0L) paste0("Moderated paths were detected for ", paste(interaction_terms, collapse = ", "), ".") else "")

  list(model_class = model_class, display_name = display_name,
       predictors = predictors, outcomes = outcomes, mediators = mediators,
       moderators = moderators, indirect_paths = indirect_paths,
       moderated_paths = moderated_paths, interaction_terms = interaction_terms,
       process_equivalent = process_equivalent, process_match = process_match,
       direct_effect_moderated = has_direct_moderation, text = text,
       parameter_estimates = pe)
}

conditional_process_table <- function(fit, topology, data = NULL) {
  empty <- data.frame(moderator = character(0), value = numeric(0), indirect = numeric(0),
                      se = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0),
                      stringsAsFactors = FALSE)
  if (!topology$model_class %in% c("first_stage_moderated_mediation", "second_stage_moderated_mediation") ||
      length(topology$moderators) == 0L || length(topology$indirect_paths) == 0L)
    return(empty)
  moderator <- topology$moderators[[1]]
  values <- if (is.data.frame(data) && moderator %in% names(data) && is.numeric(data[[moderator]])) {
    x <- data[[moderator]]
    c(mean(x, na.rm = TRUE) - stats::sd(x, na.rm = TRUE), mean(x, na.rm = TRUE), mean(x, na.rm = TRUE) + stats::sd(x, na.rm = TRUE))
  } else if (is.data.frame(data) && moderator %in% names(data)) {
    as.numeric(unique(data[[moderator]]))
  } else numeric(0)
  if (length(values) == 0L || any(!is.finite(values)))
    return(empty)

  pe <- topology$parameter_estimates
  path <- topology$indirect_paths[[1]]
  x <- path[[1]]; m <- path[[2]]; y <- path[[length(path)]]
  a <- pe[pe$op == "~" & pe$lhs == m & pe$rhs == x, , drop = FALSE]
  b <- pe[pe$op == "~" & pe$lhs == y & pe$rhs == m, , drop = FALSE]
  inter <- pe[pe$op == "~" & pe$lhs %in% c(m, y), , drop = FALSE]
  inter <- inter[vapply(inter$rhs, function(rhs) moderator %in% mediation_interaction_components(rhs), logical(1)), , drop = FALSE]
  if (nrow(a) == 0L || nrow(b) == 0L || nrow(inter) == 0L)
    return(empty)
  interaction <- if (topology$model_class == "first_stage_moderated_mediation") inter[inter$lhs == m, , drop = FALSE] else inter[inter$lhs == y, , drop = FALSE]
  if (nrow(interaction) == 0L)
    return(empty)
  a_est <- a$est[[1]]; b_est <- b$est[[1]]; int_est <- interaction$est[[1]]
  indirect <- if (topology$model_class == "first_stage_moderated_mediation") (a_est + int_est * values) * b_est else a_est * (b_est + int_est * values)
  data.frame(moderator = moderator, value = values, indirect = indirect,
             se = NA_real_, ci.lower = NA_real_, ci.upper = NA_real_,
             stringsAsFactors = FALSE)
}

conditional_process_index <- function(fit, topology) {
  out <- data.frame(index = NA_real_, se = NA_real_, ci.lower = NA_real_,
                    ci.upper = NA_real_, note = "Not defined for this model topology.",
                    stringsAsFactors = FALSE)
  if (!topology$model_class %in% c("first_stage_moderated_mediation", "second_stage_moderated_mediation") ||
      length(topology$indirect_paths) == 0L || length(topology$moderators) == 0L)
    return(out)

  pe <- topology$parameter_estimates
  path <- topology$indirect_paths[[1]]
  x <- path[[1]]; m <- path[[2]]; y <- path[[length(path)]]; w <- topology$moderators[[1]]
  a <- pe[pe$op == "~" & pe$lhs == m & pe$rhs == x, , drop = FALSE]
  b <- pe[pe$op == "~" & pe$lhs == y & pe$rhs == m, , drop = FALSE]
  inter <- pe[pe$op == "~" & pe$lhs %in% c(m, y), , drop = FALSE]
  inter <- inter[vapply(inter$rhs, function(rhs) w %in% mediation_interaction_components(rhs), logical(1)), , drop = FALSE]
  target <- if (topology$model_class == "first_stage_moderated_mediation") inter[inter$lhs == m, , drop = FALSE] else inter[inter$lhs == y, , drop = FALSE]
  if (nrow(a) == 0L || nrow(b) == 0L || nrow(target) == 0L)
    return(out)

  index <- if (topology$model_class == "first_stage_moderated_mediation") target$est[[1]] * b$est[[1]] else a$est[[1]] * target$est[[1]]
  out$index <- index
  out$note <- paste0("Index of moderated mediation for moderator ", w, ". Confidence intervals require bootstrap results when available.")
  out
}
