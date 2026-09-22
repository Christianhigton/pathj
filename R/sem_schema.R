SEM_SCHEMA_NODE_TYPES <- c(
  "observed", "latent", "residual", "intercept", "constant", "group",
  "cluster", "time", "random_effect", "composite", "formative"
)

SEM_SCHEMA_EDGE_TYPES <- c(
  "regression", "loading", "covariance", "residual_covariance",
  "variance", "intercept", "indirect", "moderation", "moderated",
  "cross_lagged", "autoregressive", "random_intercept", "random_slope",
  "formative"
)

SEM_SCHEMA_OUTCOME_TYPES <- c(
  "continuous", "ordinal", "binary", "count", "zero_inflated", "unknown"
)

sem_schema_new <- function(nodes=NULL, edges=NULL, metadata=list()) {
  nodes <- sem_schema_nodes(nodes)
  edges <- sem_schema_edges(edges)
  metadata <- sem_schema_metadata(metadata)
  structure(
    list(nodes=nodes, edges=edges, metadata=metadata),
    class=c("pathj_sem_schema", "list")
  )
}

sem_schema_nodes <- function(nodes=NULL) {
  defaults <- data.frame(
    id=character(0),
    label=character(0),
    type=character(0),
    level=character(0),
    group=character(0),
    role=character(0),
    outcome_type=character(0),
    stringsAsFactors=FALSE
  )
  if (is.null(nodes))
    return(defaults)
  nodes <- as.data.frame(nodes, stringsAsFactors=FALSE)
  n <- nrow(nodes)
  for (name in names(defaults)) {
    if (!name %in% names(nodes))
      nodes[[name]] <- rep(NA, n)
  }
  nodes <- nodes[, names(defaults), drop=FALSE]
  nodes$id <- trimws(as.character(nodes$id))
  label <- as.character(nodes$label)
  label[is.na(label)] <- ""
  type <- as.character(nodes$type)
  type[is.na(type)] <- ""
  outcome_type <- as.character(nodes$outcome_type)
  outcome_type[is.na(outcome_type)] <- ""
  nodes$label <- ifelse(nzchar(label), label, nodes$id)
  nodes$type <- ifelse(nzchar(type), type, "observed")
  nodes$level <- as.character(nodes$level)
  nodes$group <- as.character(nodes$group)
  nodes$role <- as.character(nodes$role)
  nodes$outcome_type <- ifelse(nzchar(outcome_type), outcome_type, "unknown")
  nodes
}

sem_schema_edges <- function(edges=NULL) {
  defaults <- data.frame(
    from=character(0),
    to=character(0),
    type=character(0),
    operator=character(0),
    label=character(0),
    free=logical(0),
    value=numeric(0),
    fixed=logical(0),
    start=numeric(0),
    line=integer(0),
    level=character(0),
    group=character(0),
    constraint=character(0),
    stringsAsFactors=FALSE
  )
  if (is.null(edges))
    return(defaults)
  edges <- as.data.frame(edges, stringsAsFactors=FALSE)
  n <- nrow(edges)
  for (name in names(defaults)) {
    if (!name %in% names(edges))
      edges[[name]] <- rep(NA, n)
  }
  edges <- edges[, names(defaults), drop=FALSE]
  edges$from <- trimws(as.character(edges$from))
  edges$to <- trimws(as.character(edges$to))
  type <- as.character(edges$type)
  type[is.na(type)] <- ""
  edges$type <- ifelse(nzchar(type), type, "regression")
  edges$operator <- as.character(edges$operator)
  edges$label <- as.character(edges$label)
  edges$free <- as.logical(edges$free)
  edges$value <- suppressWarnings(as.numeric(edges$value))
  edges$fixed <- as.logical(edges$fixed)
  edges$start <- suppressWarnings(as.numeric(edges$start))
  edges$line <- suppressWarnings(as.integer(edges$line))
  edges$level <- as.character(edges$level)
  edges$group <- as.character(edges$group)
  edges$constraint <- as.character(edges$constraint)
  edges
}

sem_schema_metadata <- function(metadata=list()) {
  if (is.null(metadata))
    metadata <- list()
  defaults <- list(
    source="gui",
    estimator="lavaan",
    missing="default",
    bootstrap=list(enabled=FALSE, n=0),
    standardisation="std.all",
    robust_correction="none",
    categorical=character(0),
    groups=list(),
    constraints=character(0),
    indirect_effects=list(),
    mediation_paths=list(),
    unsupported=character(0),
    engine_support=NULL,
    generated_syntax=list()
  )
  for (name in names(defaults)) {
    if (is.null(metadata[[name]]))
      metadata[[name]] <- defaults[[name]]
  }
  metadata
}

sem_schema_validate <- function(schema, data_vars=NULL, engine="lavaan") {
  if (!inherits(schema, "pathj_sem_schema"))
    schema <- sem_schema_new(schema$nodes, schema$edges, schema$metadata)

  errors <- character(0)
  warnings <- character(0)
  nodes <- schema$nodes
  edges <- schema$edges

  if (any(!nzchar(nodes$id)))
    errors <- c(errors, "All schema nodes must have a non-empty id.")
  if (any(duplicated(nodes$id)))
    errors <- c(errors, paste0("Duplicate node ids: ", paste(unique(nodes$id[duplicated(nodes$id)]), collapse=", ")))

  bad_node_types <- setdiff(unique(nodes$type), SEM_SCHEMA_NODE_TYPES)
  bad_node_types <- bad_node_types[nzchar(bad_node_types) & !is.na(bad_node_types)]
  if (length(bad_node_types) > 0)
    errors <- c(errors, paste0("Unsupported node types: ", paste(bad_node_types, collapse=", ")))

  bad_outcomes <- setdiff(unique(nodes$outcome_type), SEM_SCHEMA_OUTCOME_TYPES)
  bad_outcomes <- bad_outcomes[nzchar(bad_outcomes) & !is.na(bad_outcomes)]
  if (length(bad_outcomes) > 0)
    warnings <- c(warnings, paste0("Unknown outcome types: ", paste(bad_outcomes, collapse=", ")))

  bad_edge_types <- setdiff(unique(edges$type), SEM_SCHEMA_EDGE_TYPES)
  bad_edge_types <- bad_edge_types[nzchar(bad_edge_types) & !is.na(bad_edge_types)]
  if (length(bad_edge_types) > 0)
    errors <- c(errors, paste0("Unsupported edge types: ", paste(bad_edge_types, collapse=", ")))

  node_ids <- nodes$id
  edge_refs <- unique(c(edges$from, edges$to))
  edge_refs <- edge_refs[nzchar(edge_refs) & !is.na(edge_refs)]
  missing_refs <- setdiff(edge_refs, node_ids)
  if (length(missing_refs) > 0)
    errors <- c(errors, paste0("Edges reference missing nodes: ", paste(missing_refs, collapse=", ")))

  if (!is.null(data_vars)) {
    observed <- nodes$id[nodes$type %in% c("observed", "cluster", "time", "group")]
    missing_data_vars <- setdiff(observed, data_vars)
    if (length(missing_data_vars) > 0)
      warnings <- c(warnings, paste0("Observed variables not found in the data: ", paste(missing_data_vars, collapse=", ")))
  }

  if (identical(engine, "lavaan")) {
    unsupported_for_lavaan <- edges$type %in% c("random_slope", "formative")
    if (any(unsupported_for_lavaan))
      warnings <- c(warnings, "Some schema features are preserved but not directly estimable by lavaan.")
    outcome_types <- unique(nodes$outcome_type[nodes$type == "observed"])
    if (any(outcome_types %in% c("count", "zero_inflated")))
      warnings <- c(warnings, "Count and zero-inflated outcomes require a future brms or Stan engine mapping.")
  }

  list(
    valid=length(errors) == 0,
    errors=unique(errors),
    warnings=unique(c(warnings, schema$metadata$unsupported))
  )
}

sem_schema_from_import <- function(imported) {
  if (inherits(imported, "pathj_sem_schema"))
    return(imported)
  if (!is.null(imported$graph))
    return(sem_schema_new(imported$graph$nodes, imported$graph$edges, imported$graph$metadata))
  sem_schema_new(imported$nodes, imported$edges, imported$metadata)
}

sem_schema_capability_map <- function(schema=NULL) {
  data.frame(
    engine=c("lavaan", "OpenMx", "brms", "Stan", "blavaan"),
    phase=c("1", "2", "3", "3", "4"),
    status=c("primary", "planned", "planned", "planned", "planned"),
    strengths=c(
      "SEM, CFA, multigroup, indirect effects, ordinal SEM where supported",
      "RAM paths, flexible constraints, advanced SEM prototypes",
      "Bayesian multilevel and non-Gaussian outcome models",
      "Custom Bayesian and hierarchical SEM-like models",
      "Bayesian SEM with lavaan-like syntax"
    ),
    limitations=c(
      "Limited random slopes, non-Gaussian counts, and zero-inflation",
      "Not yet wired as an estimation backend",
      "Requires formula translation rather than direct SEM syntax",
      "Requires generated model code and stronger safeguards",
      "Not yet wired as an estimation backend"
    ),
    stringsAsFactors=FALSE
  )
}
