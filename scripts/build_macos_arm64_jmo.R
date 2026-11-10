#!/usr/bin/env Rscript

# Build against the installed Apple Silicon jamovi runtime in an isolated tree.
if (Sys.info()[["sysname"]] != "Darwin" || !R.version$arch %in% c("aarch64", "arm64"))
    stop("Run this script on an Apple Silicon Mac using arm64 R.")
root <- normalizePath(getwd(), mustWork = TRUE)
description <- read.dcf(file.path(root, "DESCRIPTION"))
module <- unname(description[1, "Package"])
version <- unname(description[1, "Version"])
jamovi_home <- Sys.getenv("JAMOVI_HOME", "/Applications/jamovi.app")
compiler <- system.file("node_modules", "jamovi-compiler", "index.js", package = "jmvtools")
node <- Sys.which("node")
if (!nzchar(node) || !file.exists(compiler))
    stop("Install Node.js and jmvtools before building.")
stage <- tempfile("pathj-arm64-build-")
dir.create(stage)
on.exit(unlink(stage, recursive = TRUE), add = TRUE)
for (entry in c("DESCRIPTION", "NAMESPACE", "R", "data", "jamovi", "man", "tests", "inst", ".Rbuildignore", "build")) {
    from <- file.path(root, entry)
    if (file.exists(from)) file.copy(from, stage, recursive = TRUE, copy.date = TRUE)
}
cache <- file.path(stage, "build", "R4.5.0-arm64-macos")
lavaan_description <- file.path(cache, "lavaan", "DESCRIPTION")
if (!file.exists(lavaan_description) ||
    package_version(read.dcf(lavaan_description)[1, "Version"]) < "0.7.2")
    stop("The arm64 build cache must contain lavaan >= 0.7-2; prepare it with jamovi's R first.")
unlink(file.path(cache, module), recursive = TRUE)

# The client compares version and build-time when deciding whether to refresh
# a sideloaded module. A build hash by itself does not invalidate its UI cache.
metadata_path <- file.path(stage, "jamovi", "0000.yaml")
metadata <- readLines(metadata_path, warn = FALSE)
metadata <- metadata[!grepl("^build-time:", metadata)]
timestamp <- format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%OS3Z")
end <- match("...", trimws(metadata))
after <- if (is.na(end)) length(metadata) else end-1L
metadata <- append(metadata, paste0("build-time: '", timestamp, "'"), after=after)
writeLines(metadata, metadata_path)
commit <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
output <- file.path(root, sprintf("%s_%s_macos-arm64.jmo", module, version))
common <- c("--home", shQuote(jamovi_home), "--assume-app-version", "2.7.0", "--skip-remotes")
if (length(commit) == 1L && grepl("^[[:xdigit:]]{40}$", commit))
    common <- c(common, "--build-hash", commit)
for (operation in c("--prepare", "--build")) {
    args <- c(shQuote(compiler), operation, shQuote(stage), common)
    if (operation == "--build") args <- c(args, "--jmo", shQuote(output))
    status <- system2(node, args)
    if (status != 0L) stop(operation, " failed with status ", status)
}
verify <- tempfile("pathj-arm64-verify-")
dir.create(verify)
on.exit(unlink(verify, recursive = TRUE), add = TRUE)
utils::unzip(output, exdir = verify)
meta <- yaml::read_yaml(file.path(verify, module, "jamovi.yaml"))
stopifnot(identical(meta$version, version), nzchar(meta[["build-time"]]),
          identical(meta$rVersion, "4.5.0-arm64"), isTRUE(meta$usesNative))
libraries <- list.files(verify, pattern = "\\.(so|dylib)$", recursive = TRUE, full.names = TRUE)
if (!length(libraries)) stop("No native libraries found in the built module.")
info <- system2("file", shQuote(libraries), stdout = TRUE)
if (any(!grepl("arm64", info, fixed = TRUE)))
    stop("The module contains a native library without arm64 support.")
message("Built ", output, "; verified ", length(libraries), " arm64 libraries and fresh update metadata.")
