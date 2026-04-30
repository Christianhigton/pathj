#!/usr/bin/env Rscript

root <- normalizePath(getwd(), mustWork = TRUE)
module_name <- "pathJTEST"
module_title <- "pathJTEST"
description <- read.dcf(file.path(root, "DESCRIPTION"))
module_version <- description[1, "Version"]
output <- file.path(root, paste0("pathJTEST-standalone-", module_version, ".jmo"))

compiler <- "/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/jmvtools/node_modules/jamovi-compiler/index.js"
node <- "/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/node/node-darwin/bin/node"

if (!file.exists(node))
  stop("Node executable for jmvtools was not found: ", node)
if (!file.exists(compiler))
  stop("jamovi compiler was not found: ", compiler)

stage <- tempfile("pathJTEST-build-")
dir.create(stage, recursive = TRUE)
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)

copy_entries <- c(
  "DESCRIPTION", "NAMESPACE", "R", "data", "jamovi", "man", "tests",
  "build", ".Rbuildignore"
)
for (entry in copy_entries) {
  from <- file.path(root, entry)
  if (file.exists(from))
    file.copy(from, stage, recursive = TRUE, copy.date = TRUE)
}
unlink(
  Sys.glob(file.path(stage, "build", "*", "pathj")),
  recursive = TRUE,
  force = TRUE
)

replace_file <- function(path, replacements) {
  text <- readLines(path, warn = FALSE)
  for (replacement in replacements)
    text <- gsub(replacement$pattern, replacement$value, text, fixed = replacement$fixed)
  writeLines(text, path, useBytes = TRUE)
}

replace_first_fixed <- function(path, pattern, value) {
  text <- readLines(path, warn = FALSE)
  hit <- grep(pattern, text, fixed = TRUE)
  if (length(hit) > 0)
    text[hit[[1]]] <- sub(pattern, value, text[hit[[1]]], fixed = TRUE)
  writeLines(text, path, useBytes = TRUE)
}

remove_line_fixed <- function(path, pattern) {
  text <- readLines(path, warn = FALSE)
  text <- text[!grepl(pattern, text, fixed = TRUE)]
  writeLines(text, path, useBytes = TRUE)
}

replace_file(file.path(stage, "DESCRIPTION"), list(
  list(pattern = "Package: pathj", value = paste0("Package: ", module_name), fixed = TRUE),
  list(pattern = "Title: Path Analysis", value = paste0("Title: ", module_title), fixed = TRUE),
  list(pattern = "Exports: pathj", value = "Exports: pathj", fixed = TRUE)
))

replace_first_fixed(
  file.path(stage, "jamovi", "0000.yaml"),
  "name: pathj",
  paste0("name: ", module_name)
)
replace_file(file.path(stage, "jamovi", "0000.yaml"), list(
  list(pattern = "title: Path Analysis", value = paste0("title: ", module_title), fixed = TRUE),
  list(pattern = "ns: pathj", value = paste0("ns: ", module_name), fixed = TRUE),
  list(pattern = "menuGroup: SEM", value = paste0("menuGroup: ", module_title), fixed = TRUE),
  list(pattern = "menuTitle: Path Analysis", value = "menuTitle: Path Analysis TEST", fixed = TRUE),
  list(pattern = "description: Path Analysis", value = "description: Path Analysis TEST module", fixed = TRUE)
))
remove_line_fixed(file.path(stage, "jamovi", "0000.yaml"), "menuSubgroup: pathj")

replace_file(file.path(stage, "jamovi", "pathj.a.yaml"), list(
  list(pattern = "menuGroup: SEM", value = paste0("menuGroup: ", module_title), fixed = TRUE),
  list(pattern = "title: Path Analysis", value = "title: Path Analysis TEST", fixed = TRUE),
  list(pattern = "description:\n    main: Path Analysis", value = "description:\n    main: Path Analysis TEST module", fixed = TRUE)
))

replace_file(file.path(stage, "jamovi", "pathj.u.yaml"), list(
  list(pattern = "title: Path Analysis", value = "title: Path Analysis TEST", fixed = TRUE)
))

replace_file(file.path(stage, "jamovi", "00refs.yaml"), list(
  list(pattern = "PATHj: jamovi Path Analysis", value = "pathJTEST: standalone test module for PATHj", fixed = TRUE),
  list(pattern = paste0("Version ", module_version, " local sideload"), value = paste0("Version ", module_version, " standalone test"), fixed = TRUE)
))

args <- c(
  shQuote(compiler),
  "--build", shQuote(stage),
  "--assume-app-version", "2.6.0",
  "--jmo", shQuote(output)
)
status <- system2(node, args = args)
if (!identical(status, 0L))
  stop("pathJTEST .jmo build failed with status ", status)

message("Built standalone test module: ", output)
