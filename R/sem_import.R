SEM_IMPORT_EDGE_TYPES <- c("regression", "loading", "covariance", "variance", "intercept")

sem_import_tokenize <- function(text, source="lavaan") {
  if (is.null(text))
    text <- ""
  lines <- unlist(strsplit(as.character(text), "\n", fixed=TRUE), use.names=FALSE)
  if (length(lines) == 0)
    lines <- ""
  tokens <- list()
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    starts <- gregexpr("[A-Za-z_][A-Za-z0-9_.]*|:=|=~|~~|==|<=|>=|->|<->|[~;:,*()=<>|\\[\\]]|\"[^\"]*\"|'[^']*'|[^[:space:]]+", line, perl=TRUE)[[1]]
    if (starts[[1]] == -1)
      next
    vals <- regmatches(line, list(starts))[[1]]
    for (j in seq_along(vals)) {
      value <- vals[[j]]
      type <- if (grepl("^[A-Za-z_][A-Za-z0-9_.]*$", value)) "identifier" else if (grepl("^['\"]", value)) "string" else "symbol"
      tokens[[length(tokens)+1]] <- list(type=type, value=value, line=i, column=starts[[j]], source=source)
    }
  }
  tokens
}

sem_import_parse <- function(text, source="lavaan", variables=NULL) {
  source <- tolower(source %||% "lavaan")
  parsed <- switch(source,
    lavaan=sem_import_parse_lavaan(text),
    mermaid=sem_import_parse_mermaid(text),
    mplus=sem_import_parse_mplus(text),
    openmx=sem_import_parse_openmx(text),
    stop("Unsupported syntax source: ", source)
  )
  parsed$tokens <- sem_import_tokenize(text, source)
  parsed$graph <- sem_import_ast_to_graph(parsed, variables=variables)
  parsed$exports <- sem_import_exports(parsed$graph)
  parsed$engine_support <- sem_import_engine_support(parsed$graph)
  parsed
}

sem_import_parse_lavaan <- function(text) {
  statements <- list()
  warnings <- character(0)
  errors <- list()
  parts <- sem_import_split_lines(text, comment_pattern="#.*$")
  for (part in parts) {
    stmt <- sem_import_parse_lavaan_statement(part$text, line=part$line)
    if (is.null(stmt)) {
      warnings <- c(warnings, sem_import_message(part$line, "Unsupported lavaan statement skipped."))
      next
    }
    statements[[length(statements)+1]] <- stmt
  }
  sem_import_ast("lavaan", statements, warnings, errors)
}

sem_import_parse_mermaid <- function(text) {
  statements <- list()
  warnings <- character(0)
  errors <- list()
  raw_lines <- sem_import_split_lines(gsub(";", "\n", text, fixed=TRUE), comment_pattern="^\\s*%%.*$")
  node_pattern <- "[A-Za-z0-9_.]+(?:\\[[^\\]]+\\]|\\([^\\)]*\\)|\\{[^\\}]*\\})?"
  cov_pattern <- paste0("(", node_pattern, ")\\s*<-->\\s*(?:\\|([^|]*)\\|\\s*)?(", node_pattern, ")")
  reg_pattern <- paste0("(", node_pattern, ")\\s*-->\\s*(?:\\|([^|]*)\\|\\s*)?(", node_pattern, ")")
  for (part in raw_lines) {
    line <- sub("^\\s*(graph|flowchart)\\s+[A-Za-z]+\\s*", "", part$text, ignore.case=TRUE)
    line <- trimws(line)
    if (!nzchar(line))
      next
    cov_hits <- sem_import_regex_hits(cov_pattern, line)
    reg_hits <- sem_import_regex_hits(reg_pattern, line)
    if (length(cov_hits) == 0 && length(reg_hits) == 0) {
      warnings <- c(warnings, sem_import_message(part$line, "Mermaid line did not contain an importable path."))
      next
    }
    for (hit in cov_hits) {
      statements[[length(statements)+1]] <- list(
        type="path", operator="~~", lhs=sem_import_clean_mermaid_node(hit[[2]]),
        rhs=sem_import_clean_mermaid_node(hit[[4]]), rhs_terms=list(list(name=sem_import_clean_mermaid_node(hit[[4]]))),
        label=trimws(hit[[3]] %||% ""), line=part$line, block="MODEL", source="mermaid")
    }
    for (hit in reg_hits) {
      statements[[length(statements)+1]] <- list(
        type="path", operator="~", lhs=sem_import_clean_mermaid_node(hit[[4]]),
        rhs=sem_import_clean_mermaid_node(hit[[2]]), rhs_terms=list(list(name=sem_import_clean_mermaid_node(hit[[2]]))),
        label=trimws(hit[[3]] %||% ""), line=part$line, block="MODEL", source="mermaid")
    }
  }
  sem_import_ast("mermaid", statements, warnings, errors)
}

sem_import_parse_mplus <- function(text) {
  warnings <- character(0)
  errors <- list()
  statements <- list()
  parts <- sem_import_mplus_statements(text)
  for (part in parts) {
    stmt <- trimws(part$text)
    if (!nzchar(stmt))
      next
    up <- toupper(stmt)
    if (grepl("^GROUPING\\s*(IS|=)", up)) {
      statements[[length(statements)+1]] <- sem_import_parse_mplus_grouping(stmt, part$line)
      next
    }
    if (grepl("^CATEGORICAL\\s+(ARE|=)", up)) {
      statements[[length(statements)+1]] <- list(type="categorical", variables=sem_import_rhs_names(sub("^CATEGORICAL\\s+(ARE|=)\\s*", "", stmt, ignore.case=TRUE)), line=part$line, block=part$block, source="mplus")
      next
    }
    if (grepl("\\s+IND\\s+", up)) {
      fields <- sem_import_words(stmt)
      statements[[length(statements)+1]] <- list(type="indirect", lhs=fields[[1]], rhs=fields[[length(fields)]], line=part$line, block=part$block, level=part$level, source="mplus")
      next
    }
    if (grepl("|", stmt, fixed=TRUE)) {
      growth <- sem_import_parse_mplus_growth(stmt, part)
      statements <- c(statements, growth$statements)
      warnings <- c(warnings, growth$warnings)
      next
    }
    if (grepl("\\bON\\b", up)) {
      statements[[length(statements)+1]] <- sem_import_parse_mplus_path(stmt, "ON", "~", part)
      next
    }
    if (grepl("\\bBY\\b", up)) {
      statements[[length(statements)+1]] <- sem_import_parse_mplus_path(stmt, "BY", "=~", part)
      next
    }
    if (grepl("\\bWITH\\b", up)) {
      statements[[length(statements)+1]] <- sem_import_parse_mplus_path(stmt, "WITH", "~~", part)
      next
    }
    if (grepl("^NEW\\s*\\(", up))
      next
    if (identical(part$block, "MODEL CONSTRAINT") && grepl("=", stmt, fixed=TRUE)) {
      statements[[length(statements)+1]] <- sem_import_parse_constraint_assignment(stmt, part$line, "mplus")
      next
    }
    warnings <- c(warnings, sem_import_message(part$line, paste0("Unsupported Mplus statement skipped: ", stmt)))
  }
  sem_import_ast("mplus", statements, warnings, errors)
}

sem_import_parse_openmx <- function(text) {
  warnings <- character(0)
  errors <- list()
  statements <- list()
  calls <- sem_import_find_calls(text, c("mxPath", "mxModel"))
  if (length(calls) == 0)
    warnings <- c(warnings, "No mxPath or mxModel calls were found.")
  for (call in calls) {
    args <- sem_import_parse_call_args(call$args)
    if (identical(call$name, "mxModel")) {
      manifests <- sem_import_arg_vector(args$manifestVars)
      latents <- sem_import_arg_vector(args$latentVars)
      if (length(manifests) > 0)
        statements[[length(statements)+1]] <- list(type="manifestVars", variables=manifests, line=call$line, source="openmx")
      if (length(latents) > 0)
        statements[[length(statements)+1]] <- list(type="latentVars", variables=latents, line=call$line, source="openmx")
      next
    }
    from <- sem_import_arg_vector(args$from)
    to <- sem_import_arg_vector(args$to)
    arrows <- as.integer(sem_import_scalar(args$arrows, "1"))
    labels <- sem_import_arg_vector(args$labels %||% args$label)
    free <- sem_import_logical_vector(args$free)
    values <- sem_import_numeric_vector(args$values %||% args$value)
    if (length(from) == 0) {
      errors[[length(errors)+1]] <- list(line=call$line, message="mxPath call is missing a safe from= argument.")
      next
    }
    if (length(to) == 0 && arrows == 2)
      to <- from
    if (length(to) == 0) {
      warnings <- c(warnings, sem_import_message(call$line, "mxPath call without to= was skipped because arrows is not 2."))
      next
    }
    for (i in seq_along(from)) {
      for (j in seq_along(to)) {
        label <- sem_import_recycle(labels, max(i, j))
        value <- sem_import_recycle(values, max(i, j))
        free_value <- sem_import_recycle(free, max(i, j))
        statements[[length(statements)+1]] <- list(
          type="path", operator=if (arrows == 2) "~~" else "~",
          lhs=to[[j]], rhs=from[[i]], rhs_terms=list(list(name=from[[i]], label=label, value=value, free=free_value)),
          label=label, value=value, free=free_value, line=call$line, block="mxPath", source="openmx")
      }
    }
  }
  sem_import_ast("openmx", statements, warnings, errors)
}

sem_import_ast_to_graph <- function(ast, variables=NULL) {
  edges <- list()
  nodes <- list()
  metadata <- list(
    source=ast$source,
    categorical=character(0),
    groups=list(),
    constraints=character(0),
    unsupported=ast$warnings,
    multilevel=list(),
    indirect_effects=list(),
    manifests=character(0),
    latents=character(0),
    mediation_paths=list())

  for (stmt in ast$statements) {
    if (identical(stmt$type, "manifestVars"))
      metadata$manifests <- unique(c(metadata$manifests, stmt$variables))
    if (identical(stmt$type, "latentVars"))
      metadata$latents <- unique(c(metadata$latents, stmt$variables))
  }

  add_node <- function(id, type="observed", level=NA_character_, group=NA_character_) {
    if (!nzchar(id))
      return()
    key <- paste(id, type, level, group, sep="\r")
    nodes[[key]] <<- list(id=id, label=id, type=type, level=level, group=group)
  }
  add_edge <- function(from, to, type, operator, label="", free=NA, value=NA, line=NA_integer_, level=NA_character_, group=NA_character_) {
    add_node(from, if (type == "loading") "latent" else "observed", level, group)
    add_node(to, "observed", level, group)
    edges[[length(edges)+1]] <<- list(from=from, to=to, type=type, operator=operator, label=label %||% "",
      free=free, value=value, fixed=!is.na(value) && identical(free, FALSE), line=line, level=level, group=group)
    if (type %in% c("regression", "loading")) {
      add_node(paste0("residual_", to), "residual", level, group)
      add_node(paste0("intercept_", to), "intercept", level, group)
    }
  }

  for (stmt in ast$statements) {
    if (identical(stmt$type, "categorical")) {
      metadata$categorical <- unique(c(metadata$categorical, stmt$variables))
      next
    }
    if (identical(stmt$type, "grouping")) {
      metadata$groups[[stmt$variable]] <- stmt$levels
      add_node(paste0("group_", stmt$variable), "group", NA_character_, stmt$variable)
      next
    }
    if (identical(stmt$type, "constraint") || identical(stmt$type, "defined")) {
      metadata$constraints <- c(metadata$constraints, stmt$syntax)
      next
    }
    if (identical(stmt$type, "indirect")) {
      metadata$indirect_effects[[length(metadata$indirect_effects)+1]] <- stmt
      next
    }
    if (identical(stmt$type, "manifestVars")) {
      metadata$manifests <- unique(c(metadata$manifests, stmt$variables))
      for (v in stmt$variables) add_node(v, "observed")
      next
    }
    if (identical(stmt$type, "latentVars")) {
      metadata$latents <- unique(c(metadata$latents, stmt$variables))
      for (v in stmt$variables) add_node(v, "latent")
      next
    }
    if (!identical(stmt$type, "path"))
      next
    rhs_terms <- stmt$rhs_terms
    for (term in rhs_terms) {
      edge_operator <- stmt$operator
      edge_type <- switch(stmt$operator,
        "~"="regression",
        "=~"="loading",
        "~~"=if (identical(stmt$lhs, term$name)) "variance" else "covariance",
        "intercept")
      if (identical(stmt$source, "openmx") && identical(stmt$operator, "~") && term$name %in% metadata$latents) {
        edge_type <- "loading"
        edge_operator <- "=~"
      }
      if (identical(edge_operator, "=~")) {
        if (identical(stmt$source, "openmx") && term$name %in% metadata$latents) {
          loading_from <- term$name
          loading_to <- stmt$lhs
        } else {
          loading_from <- stmt$lhs
          loading_to <- term$name
        }
        add_node(loading_from, "latent", stmt$level %||% NA_character_, stmt$group %||% NA_character_)
        add_node(loading_to, "observed", stmt$level %||% NA_character_, stmt$group %||% NA_character_)
        add_edge(loading_from, loading_to, edge_type, edge_operator, term$label %||% stmt$label %||% "",
          term$free %||% stmt$free %||% NA, term$value %||% stmt$value %||% NA, stmt$line,
          stmt$level %||% NA_character_, stmt$group %||% NA_character_)
      } else {
        add_edge(term$name, stmt$lhs, edge_type, edge_operator, term$label %||% stmt$label %||% "",
          term$free %||% stmt$free %||% NA, term$value %||% stmt$value %||% NA, stmt$line,
          stmt$level %||% NA_character_, stmt$group %||% NA_character_)
      }
    }
  }

  node_df <- if (length(nodes) == 0) {
    data.frame(id=character(0), label=character(0), type=character(0), level=character(0), group=character(0), stringsAsFactors=FALSE)
  } else do.call(rbind, lapply(nodes, as.data.frame, stringsAsFactors=FALSE))
  rownames(node_df) <- NULL
  edge_df <- if (length(edges) == 0) {
    data.frame(from=character(0), to=character(0), type=character(0), operator=character(0), label=character(0), free=logical(0), value=numeric(0), fixed=logical(0), line=integer(0), level=character(0), group=character(0), stringsAsFactors=FALSE)
  } else do.call(rbind, lapply(edges, as.data.frame, stringsAsFactors=FALSE))
  rownames(edge_df) <- NULL
  metadata$mediation_paths <- sem_import_find_mediation_paths(edge_df)
  metadata$variables <- unique(c(node_df$id[!node_df$type %in% c("residual", "intercept", "group")], variables))
  metadata$endogenous <- unique(edge_df$to[edge_df$type == "regression"])
  metadata$exogenous <- setdiff(unique(edge_df$from[edge_df$type == "regression"]), metadata$endogenous)
  metadata$latent_variables <- unique(c(node_df$id[node_df$type == "latent"], metadata$latents))
  list(nodes=node_df, edges=edge_df, metadata=metadata)
}

sem_import_exports <- function(graph) {
  list(
    lavaan=sem_import_graph_to_lavaan(graph),
    mermaid=sem_import_graph_to_mermaid(graph),
    mplus=sem_import_graph_to_mplus(graph),
    openmx=sem_import_graph_to_openmx(graph))
}

sem_import_graph_to_lavaan <- function(graph) {
  edges <- graph$edges
  if (nrow(edges) == 0)
    return(character(0))
  lines <- character(0)
  level_values <- unique(edges$level[!is.na(edges$level) & nzchar(edges$level)])
  make_lines <- function(ed) {
    out <- character(0)
    for (op in c("=~", "~")) {
      op_edges <- ed[ed$operator == op, , drop=FALSE]
      if (nrow(op_edges) == 0)
        next
      split_edges <- split(op_edges, op_edges$to)
      if (identical(op, "=~"))
        split_edges <- split(op_edges, op_edges$from)
      out <- c(out, unlist(lapply(split_edges, function(x) {
        lhs <- if (identical(op, "=~")) x$from[[1]] else x$to[[1]]
        rhs <- if (identical(op, "=~")) x$to else x$from
        paste(lhs, op, paste(mapply(sem_import_lavaan_term, rhs, x$label, x$value, x$free, USE.NAMES=FALSE), collapse=" + "))
      }), use.names=FALSE))
    }
    covs <- ed[ed$operator == "~~", , drop=FALSE]
    if (nrow(covs) > 0) {
      out <- c(out, vapply(seq_len(nrow(covs)), function(i) {
        r <- covs[i,]
        paste(r$to, "~~", sem_import_lavaan_term(r$from, r$label, r$value, r$free))
      }, FUN.VALUE=character(1)))
    }
    out
  }
  if (length(level_values) > 0) {
    for (level in level_values) {
      lines <- c(lines, paste0("level: ", if (tolower(level) %in% c("within", "level 1", "1")) "1" else "2"))
      lines <- c(lines, paste0("  ", make_lines(edges[edges$level == level, , drop=FALSE])))
    }
    no_level <- edges[is.na(edges$level) | !nzchar(edges$level), , drop=FALSE]
    if (nrow(no_level) > 0)
      lines <- c(lines, make_lines(no_level))
  } else {
    lines <- make_lines(edges)
  }
  constraints <- graph$metadata$constraints
  if (length(constraints) > 0)
    lines <- c(lines, constraints)
  unique(lines[nzchar(trimws(lines))])
}

sem_import_graph_to_mermaid <- function(graph) {
  edges <- graph$edges
  lines <- c("flowchart LR")
  if (nrow(edges) == 0)
    return(lines)
  keep <- edges$type %in% c("regression", "loading", "covariance")
  edges <- edges[keep, , drop=FALSE]
  lines <- c(lines, vapply(seq_len(nrow(edges)), function(i) {
    r <- edges[i,]
    connector <- if (identical(r$type, "covariance")) " <--> " else " --> "
    label <- if (nzchar(r$label)) paste0("|\"", sem_import_mermaid_escape(r$label), "\"|") else ""
    paste0("  ", sem_import_mermaid_id(r$from), "[\"", sem_import_mermaid_escape(r$from), "\"]", connector, label, sem_import_mermaid_id(r$to), "[\"", sem_import_mermaid_escape(r$to), "\"]")
  }, FUN.VALUE=character(1)))
  lines
}

sem_import_graph_to_mplus <- function(graph) {
  edges <- graph$edges
  lines <- c("MODEL:")
  if (nrow(edges) == 0)
    return(lines)
  loadings <- edges[edges$type == "loading", , drop=FALSE]
  if (nrow(loadings) > 0) {
    for (latent in unique(loadings$from)) {
      rhs <- loadings$to[loadings$from == latent]
      lines <- c(lines, paste0("  ", latent, " BY ", paste(rhs, collapse=" "), ";"))
    }
  }
  regs <- edges[edges$type == "regression", , drop=FALSE]
  if (nrow(regs) > 0) {
    for (lhs in unique(regs$to)) {
      rhs <- regs$from[regs$to == lhs]
      lines <- c(lines, paste0("  ", lhs, " ON ", paste(rhs, collapse=" "), ";"))
    }
  }
  covs <- edges[edges$type == "covariance", , drop=FALSE]
  if (nrow(covs) > 0)
    lines <- c(lines, paste0("  ", covs$to, " WITH ", covs$from, ";"))
  if (length(graph$metadata$categorical) > 0)
    lines <- c("VARIABLE:", paste0("  CATEGORICAL ARE ", paste(graph$metadata$categorical, collapse=" "), ";"), lines)
  lines
}

sem_import_graph_to_openmx <- function(graph) {
  edges <- graph$edges
  lines <- character(0)
  manifests <- graph$nodes$id[graph$nodes$type == "observed"]
  latents <- graph$nodes$id[graph$nodes$type == "latent"]
  manifests <- manifests[!grepl("^(residual|intercept|group)_", manifests)]
  if (length(manifests) > 0)
    lines <- c(lines, paste0("manifestVars=c(", paste(sprintf("\"%s\"", manifests), collapse=", "), ")"))
  if (length(latents) > 0)
    lines <- c(lines, paste0("latentVars=c(", paste(sprintf("\"%s\"", latents), collapse=", "), ")"))
  if (nrow(edges) > 0) {
    lines <- c(lines, vapply(seq_len(nrow(edges)), function(i) {
      r <- edges[i,]
      arrows <- if (identical(r$type, "covariance") || identical(r$type, "variance")) 2 else 1
      label <- if (nzchar(r$label)) paste0(", labels=\"", r$label, "\"") else ""
      paste0("mxPath(from=\"", r$from, "\", to=\"", r$to, "\", arrows=", arrows, label, ")")
    }, FUN.VALUE=character(1)))
  }
  lines
}

sem_import_engine_support <- function(graph) {
  features <- unique(c(graph$edges$type, if (length(graph$metadata$categorical) > 0) "categorical", if (length(graph$metadata$groups) > 0) "groups", graph$edges$level))
  multilevel <- any(!is.na(graph$edges$level) & nzchar(graph$edges$level))
  data.frame(
    engine=c("lavaan", "OpenMx", "brms", "Stan"),
    status=c(if (multilevel) "partial" else "supported", "exportable", if (multilevel) "candidate" else "not mapped", if (multilevel) "candidate" else "not mapped"),
    warning=c(
      if (multilevel) "Multilevel syntax is preserved, but estimation requires a cluster variable and lavaan multilevel support." else "",
      "OpenMx export is RAM-style text; pathJ estimation still uses lavaan.",
      if (multilevel) "brms may estimate comparable multilevel models, but automatic formula generation is not implemented." else "No brms estimator mapping is implemented.",
      if (multilevel) "Stan may estimate comparable multilevel models, but automatic Stan code generation is not implemented." else "No Stan code generation is implemented."
    ),
    features=paste(features[nzchar(features) & !is.na(features)], collapse=", "),
    stringsAsFactors=FALSE)
}

sem_import_ast <- function(source, statements, warnings, errors) {
  structure(list(source=source, statements=statements, warnings=unique(warnings), errors=errors), class="pathj_sem_import_ast")
}

sem_import_split_lines <- function(text, comment_pattern="#.*$") {
  text <- as.character(text %||% "")
  lines <- unlist(strsplit(text, "\n", fixed=TRUE), use.names=FALSE)
  out <- list()
  for (i in seq_along(lines)) {
    line <- gsub(comment_pattern, "", lines[[i]], perl=TRUE)
    for (part in unlist(strsplit(line, ";", fixed=TRUE), use.names=FALSE)) {
      part <- trimws(part)
      if (nzchar(part))
        out[[length(out)+1]] <- list(text=part, line=i)
    }
  }
  out
}

sem_import_parse_lavaan_statement <- function(text, line=NA_integer_) {
  ops <- c(":=", "==", "<=", ">=", "=~", "~~", "~")
  for (op in ops) {
    pos <- regexpr(op, text, fixed=TRUE)[[1]]
    if (pos > 0) {
      lhs <- trimws(substr(text, 1, pos - 1))
      rhs <- trimws(substr(text, pos + nchar(op), nchar(text)))
      if (op %in% c(":=", "==", "<=", ">="))
        return(list(type=if (op == ":=") "defined" else "constraint", syntax=paste(lhs, op, rhs), lhs=lhs, rhs=rhs, operator=op, line=line, source="lavaan"))
      return(list(type="path", operator=op, lhs=sem_import_clean_name(lhs), rhs=rhs,
        rhs_terms=sem_import_parse_rhs_terms(rhs), line=line, block="MODEL", source="lavaan"))
    }
  }
  NULL
}

sem_import_mplus_statements <- function(text) {
  lines <- unlist(strsplit(as.character(text %||% ""), "\n", fixed=TRUE), use.names=FALSE)
  out <- list()
  block <- "MODEL"
  level <- NA_character_
  buffer <- ""
  buffer_line <- 1
  emit <- function(value, line) {
    value <- trimws(value)
    if (nzchar(value))
      out[[length(out)+1]] <<- list(text=value, line=line, block=block, level=level)
  }
  for (i in seq_along(lines)) {
    line <- sub("!.*$", "", lines[[i]])
    line <- trimws(line)
    if (!nzchar(line))
      next
    if (grepl("^%[^%]+%$", line)) {
      lvl <- gsub("%", "", line, fixed=TRUE)
      level <- tolower(trimws(lvl))
      next
    }
    block_match <- regmatches(line, regexec("^([A-Za-z ]+):\\s*(.*)$", line, perl=TRUE))[[1]]
    if (length(block_match) > 0) {
      block <- toupper(trimws(block_match[[2]]))
      rest <- trimws(block_match[[3]])
      if (!nzchar(rest))
        next
      line <- rest
    }
    if (!nzchar(buffer))
      buffer_line <- i
    buffer <- paste(buffer, line)
    while (grepl(";", buffer, fixed=TRUE)) {
      before <- sub(";.*$", "", buffer)
      emit(before, buffer_line)
      buffer <- sub("^[^;]*;", "", buffer)
      buffer <- trimws(buffer)
      buffer_line <- i
    }
  }
  if (nzchar(trimws(buffer)))
    emit(buffer, buffer_line)
  out
}

sem_import_parse_mplus_path <- function(stmt, keyword, operator, part) {
  pieces <- strsplit(stmt, paste0("(?i)\\b", keyword, "\\b"), perl=TRUE)[[1]]
  lhs <- sem_import_clean_name(pieces[[1]])
  rhs <- paste(pieces[-1], collapse=keyword)
  list(type="path", operator=operator, lhs=lhs, rhs=rhs, rhs_terms=sem_import_parse_rhs_terms(rhs, plus=FALSE),
    line=part$line, block=part$block, level=part$level, source="mplus")
}

sem_import_parse_mplus_grouping <- function(stmt, line) {
  stmt <- sub("^GROUPING\\s*(IS|=)\\s*", "", stmt, ignore.case=TRUE)
  parts <- strsplit(stmt, "\\s*\\(", perl=TRUE)[[1]]
  var <- sem_import_clean_name(parts[[1]])
  levels <- character(0)
  if (length(parts) > 1)
    levels <- sem_import_rhs_names(gsub("[()]", " ", paste(parts[-1], collapse=" ")))
  list(type="grouping", variable=var, levels=levels, line=line, source="mplus")
}

sem_import_parse_mplus_growth <- function(stmt, part) {
  warnings <- character(0)
  statements <- list()
  pieces <- strsplit(stmt, "\\|", perl=TRUE)[[1]]
  factors <- sem_import_words(pieces[[1]])
  occasions <- sem_import_words(pieces[[2]])
  if (length(factors) == 0 || length(occasions) == 0)
    return(list(statements=statements, warnings=sem_import_message(part$line, "Malformed latent growth statement skipped.")))
  for (factor in factors) {
    terms <- lapply(occasions, function(x) {
      m <- regmatches(x, regexec("^([^@*]+)(?:[@*]([-0-9.]+))?$", x, perl=TRUE))[[1]]
      list(name=sem_import_clean_name(m[[2]]), value=if (length(m) >= 3 && nzchar(m[[3]])) as.numeric(m[[3]]) else NA)
    })
    statements[[length(statements)+1]] <- list(type="path", operator="=~", lhs=factor, rhs="", rhs_terms=terms,
      line=part$line, block=part$block, level=part$level, source="mplus")
  }
  list(statements=statements, warnings=warnings)
}

sem_import_parse_constraint_assignment <- function(stmt, line, source) {
  stmt <- sub(";", "", stmt, fixed=TRUE)
  parts <- strsplit(stmt, "=", fixed=TRUE)[[1]]
  lhs <- sem_import_clean_name(parts[[1]])
  rhs <- trimws(paste(parts[-1], collapse="="))
  list(type="defined", syntax=paste(lhs, ":=", rhs), lhs=lhs, rhs=rhs, operator=":=", line=line, source=source)
}

sem_import_parse_rhs_terms <- function(rhs, plus=TRUE) {
  sep <- if (plus) "\\+" else "\\s+"
  parts <- unlist(strsplit(rhs, sep, perl=TRUE), use.names=FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  lapply(parts, sem_import_parse_term)
}

sem_import_parse_term <- function(term) {
  term <- trimws(term)
  label <- ""
  value <- NA_real_
  free <- NA
  if (grepl("\\*", term)) {
    pieces <- strsplit(term, "\\*", perl=TRUE)[[1]]
    prefix <- trimws(pieces[[1]])
    term <- trimws(paste(pieces[-1], collapse="*"))
    if (grepl("^[-+]?[0-9.]+$", prefix))
      value <- as.numeric(prefix)
    else
      label <- prefix
  }
  if (grepl("@", term, fixed=TRUE)) {
    pieces <- strsplit(term, "@", fixed=TRUE)[[1]]
    term <- trimws(pieces[[1]])
    value <- suppressWarnings(as.numeric(pieces[[2]]))
    free <- FALSE
  }
  list(name=sem_import_clean_name(term), label=label, value=value, free=free)
}

sem_import_find_calls <- function(text, names) {
  chars <- strsplit(as.character(text %||% ""), "", fixed=TRUE)[[1]]
  calls <- list()
  i <- 1
  in_quote <- NULL
  line <- 1
  while (i <= length(chars)) {
    ch <- chars[[i]]
    if (ch == "\n")
      line <- line + 1
    if (is.null(in_quote) && ch %in% c("\"", "'")) {
      in_quote <- ch
      i <- i + 1
      next
    } else if (!is.null(in_quote)) {
      if (ch == in_quote)
        in_quote <- NULL
      i <- i + 1
      next
    }
    for (nm in names) {
      n <- nchar(nm)
      if (i + n <= length(chars) && paste(chars[i:(i+n-1)], collapse="") == nm) {
        j <- i + n
        while (j <= length(chars) && grepl("\\s", chars[[j]])) j <- j + 1
        if (j <= length(chars) && chars[[j]] == "(") {
          depth <- 1
          k <- j + 1
          q <- NULL
          while (k <= length(chars) && depth > 0) {
            cc <- chars[[k]]
            if (is.null(q) && cc %in% c("\"", "'")) q <- cc
            else if (!is.null(q) && cc == q) q <- NULL
            else if (is.null(q) && cc == "(") depth <- depth + 1
            else if (is.null(q) && cc == ")") depth <- depth - 1
            k <- k + 1
          }
          args <- paste(chars[(j+1):(k-2)], collapse="")
          calls[[length(calls)+1]] <- list(name=nm, args=args, line=line)
          i <- k
          break
        }
      }
    }
    i <- i + 1
  }
  calls
}

sem_import_parse_call_args <- function(args) {
  parts <- sem_import_split_top_level(args, ",")
  out <- list()
  unnamed <- character(0)
  for (part in parts) {
    kv <- sem_import_split_top_level(part, "=")
    if (length(kv) >= 2) {
      key <- trimws(kv[[1]])
      value <- trimws(paste(kv[-1], collapse="="))
      out[[key]] <- value
    } else {
      unnamed <- c(unnamed, trimws(part))
    }
  }
  out$..unnamed <- unnamed
  out
}

sem_import_split_top_level <- function(text, sep=",") {
  chars <- strsplit(as.character(text %||% ""), "", fixed=TRUE)[[1]]
  out <- character(0)
  current <- ""
  depth <- 0
  quote <- NULL
  for (ch in chars) {
    if (is.null(quote) && ch %in% c("\"", "'")) quote <- ch
    else if (!is.null(quote) && ch == quote) quote <- NULL
    else if (is.null(quote) && ch == "(") depth <- depth + 1
    else if (is.null(quote) && ch == ")") depth <- depth - 1
    if (is.null(quote) && depth == 0 && identical(ch, sep)) {
      out <- c(out, current)
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }
  c(out, current)
}

sem_import_arg_vector <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value))
    return(character(0))
  value <- trimws(value)
  value <- sub("^c\\s*\\(", "", value, ignore.case=TRUE)
  value <- sub("\\)$", "", value)
  parts <- sem_import_split_top_level(value, ",")
  parts <- trimws(parts)
  parts <- gsub("^['\"]|['\"]$", "", parts)
  parts[nzchar(parts)]
}

sem_import_scalar <- function(value, default="") {
  vec <- sem_import_arg_vector(value)
  if (length(vec) == 0)
    return(default)
  vec[[1]]
}

sem_import_logical_vector <- function(value) {
  vec <- tolower(sem_import_arg_vector(value))
  if (length(vec) == 0)
    return(NA)
  vec %in% c("true", "t", "1")
}

sem_import_numeric_vector <- function(value) {
  vec <- sem_import_arg_vector(value)
  if (length(vec) == 0)
    return(NA_real_)
  suppressWarnings(as.numeric(vec))
}

sem_import_lavaan_term <- function(name, label="", value=NA, free=NA) {
  prefix <- ""
  if (!is.na(value))
    prefix <- paste0(value, "*")
  else if (!is.na(label) && nzchar(label %||% ""))
    prefix <- paste0(label, "*")
  paste0(prefix, name)
}

sem_import_find_mediation_paths <- function(edges) {
  regs <- edges[edges$type == "regression", , drop=FALSE]
  paths <- list()
  if (nrow(regs) == 0)
    return(paths)
  walk <- function(start, current, visited) {
    next_edges <- regs[regs$from == current, , drop=FALSE]
    if (nrow(next_edges) == 0)
      return()
    for (i in seq_len(nrow(next_edges))) {
      nxt <- next_edges$to[[i]]
      if (nxt %in% visited)
        next
      path <- c(visited, nxt)
      if (length(path) > 2)
        paths[[length(paths)+1]] <<- path
      walk(start, nxt, path)
    }
  }
  for (v in unique(regs$from))
    walk(v, v, v)
  paths
}

sem_import_rhs_names <- function(x) {
  v <- sem_import_words(gsub("[,;]", " ", x))
  v[!toupper(v) %in% c("ARE", "IS")]
}

sem_import_words <- function(x) {
  x <- gsub("[,=()]", " ", x)
  v <- unlist(strsplit(trimws(x), "\\s+", perl=TRUE), use.names=FALSE)
  v[nzchar(v)]
}

sem_import_clean_name <- function(value) {
  value <- trimws(value %||% "")
  value <- gsub("^`|`$|^['\"]|['\"]$", "", value)
  trimws(value)
}

sem_import_clean_mermaid_node <- function(value) {
  value <- trimws(value)
  value <- sub("\\[.*$", "", value)
  value <- sub("\\(.*$", "", value)
  value <- sub("\\{.*$", "", value)
  sem_import_clean_name(value)
}

sem_import_regex_hits <- function(pattern, text) {
  starts <- gregexpr(pattern, text, perl=TRUE)[[1]]
  if (starts[[1]] == -1)
    return(list())
  hits <- regmatches(text, list(starts))[[1]]
  lapply(hits, function(hit) regmatches(hit, regexec(pattern, hit, perl=TRUE))[[1]])
}

sem_import_mermaid_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  gsub("\"", "\\\\\"", x)
}

sem_import_mermaid_id <- function(x) {
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  if (grepl("^[0-9]", x))
    x <- paste0("v_", x)
  x
}

sem_import_recycle <- function(x, i) {
  if (length(x) == 0)
    return(NA)
  x[[((i - 1) %% length(x)) + 1]]
}

sem_import_message <- function(line, message) {
  paste0("Line ", line, ": ", message)
}

`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else
    x
}
