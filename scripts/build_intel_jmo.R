#!/usr/bin/env Rscript

root <- normalizePath(getwd(), mustWork = TRUE)
description <- read.dcf(file.path(root, "DESCRIPTION"))
module_name <- description[1, "Package"]
module_version <- description[1, "Version"]
output <- file.path(root, sprintf("%s_%s_intel.jmo", module_name, module_version))

arch <- R.version$arch
if (!arch %in% c("x86_64", "x86-64")) {
    stop(
        "The Intel build must run under x86_64 R; detected ",
        arch,
        ". Use the macos-15-intel GitHub Actions runner."
    )
}

if (!requireNamespace("jmvtools", quietly = TRUE))
    stop("The jmvtools package is required")

# Build dependencies for the same R and CPU architecture as the active runner.
jmvtools::prepare(root)

node <- getFromNamespace("node", "jmvtools")()
compiler <- getFromNamespace("jmcPath", "jmvtools")()

args <- c(
    shQuote(compiler),
    "--build", shQuote(root),
    "--assume-app-version", "2.7.0",
    "--jmo", shQuote(output)
)
status <- system2(node, args = args)
if (!identical(status, 0L))
    stop("Intel .jmo build failed with status ", status)

# Validate the archive and every native library included in it.
stage <- tempfile("pathj-intel-verify-")
dir.create(stage, recursive = TRUE)
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
utils::unzip(output, exdir = stage)

native_libraries <- list.files(
    stage,
    pattern = "\\.(so|dylib)$",
    recursive = TRUE,
    full.names = TRUE
)
if (!length(native_libraries))
    stop("The built .jmo contains no native libraries to verify")

file_info <- system2("file", native_libraries, stdout = TRUE, stderr = TRUE)
invalid <- file_info[!grepl("x86_64", file_info, fixed = TRUE)]
if (length(invalid)) {
    stop(
        "Non-Intel native libraries were found in the .jmo:\n",
        paste(invalid, collapse = "\n")
    )
}

message("Built and verified Intel module: ", output)
message("Verified ", length(native_libraries), " x86_64 native libraries")
