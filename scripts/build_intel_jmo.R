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

node <- Sys.which("node")
if (!nzchar(node))
    stop("A native Node.js executable was not found on PATH")
compiler <- system.file(
    "node_modules", "jamovi-compiler", "index.js",
    package = "jmvtools"
)
if (!nzchar(compiler) || !file.exists(compiler))
    stop("The jamovi compiler bundled with jmvtools was not found")

# The compiler derives its macOS R path from a jamovi application bundle.
# CI only needs a minimal bundle-shaped shim pointing at the runner's Intel R.
jamovi_home <- tempfile("jamovi-intel-home-")
macos_dir <- file.path(jamovi_home, "Contents", "MacOS")
r_versions_dir <- file.path(
    jamovi_home, "Contents", "Frameworks", "R.framework", "Versions", "Current"
)
dir.create(macos_dir, recursive = TRUE)
fake_r_bin <- file.path(r_versions_dir, "Resources", "bin")
dir.create(fake_r_bin, recursive = TRUE)
if (!file.symlink("/usr/bin/true", file.path(macos_dir, "jamovi")))
    stop("Could not create the temporary jamovi executable shim")
r_launcher <- file.path(fake_r_bin, "R")
writeLines(
    c(
        "#!/bin/sh",
        "unset R_HOME R_SHARE_DIR",
        sprintf("exec %s \"$@\"", shQuote(Sys.which("R")))
    ),
    r_launcher
)
Sys.chmod(r_launcher, mode = "0755")

# Bypass the node R package, whose macOS binary may be arm64 even on an Intel
# host. The compiler itself is JavaScript and runs under the runner's native Node.
prepare_args <- c(
    shQuote(compiler),
    "--prepare", shQuote(root),
    "--home", shQuote(jamovi_home),
    "--assume-app-version", "2.7.0"
)
prepare_status <- system2(node, args = prepare_args)
if (!identical(prepare_status, 0L))
    stop("Intel dependency preparation failed with status ", prepare_status)

args <- c(
    shQuote(compiler),
    "--build", shQuote(root),
    "--home", shQuote(jamovi_home),
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
