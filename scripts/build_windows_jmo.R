#!/usr/bin/env Rscript

# Run with the R bundled with the target Windows jamovi installation.
if (.Platform$OS.type != "windows" || .Machine$sizeof.pointer != 8L)
    stop("Build this module on 64-bit Windows using jamovi's bundled R.")

root <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(root, "DESCRIPTION")))
    stop("Run this script from the pathj source directory.")
jamovi_home <- Sys.getenv("JAMOVI_HOME")
if (!nzchar(jamovi_home))
    stop("Set JAMOVI_HOME to the jamovi installation directory.")
jamovi_home <- normalizePath(jamovi_home, mustWork = TRUE)
bundled_r <- normalizePath(file.path(jamovi_home, "Frameworks", "R"), mustWork = TRUE)
if (tolower(normalizePath(R.home())) != tolower(bundled_r))
    stop("Run this script with Rscript.exe from JAMOVI_HOME/Frameworks/R/bin.")
if (!requireNamespace("jmvtools", quietly = TRUE))
    stop("Install jmvtools into the bundled R library first; see docs/windows-build.md.")
node <- Sys.which("node")
if (!nzchar(node))
    stop("Install Node.js and add it to PATH.")
compiler <- system.file("node_modules", "jamovi-compiler", "index.js", package = "jmvtools")
if (!file.exists(compiler))
    stop("The jamovi compiler bundled with jmvtools was not found.")

description <- read.dcf(file.path(root, "DESCRIPTION"))
output <- file.path(root, sprintf("%s_%s_windows.jmo", description[1, "Package"], description[1, "Version"]))
common <- c("--home", shQuote(jamovi_home), "--rlibs", shQuote(normalizePath(.libPaths()[1])))
app_version <- Sys.getenv("JAMOVI_VERSION")
if (nzchar(app_version))
    common <- c(common, "--assume-app-version", app_version)
if (nzchar(Sys.which("git"))) {
    commit <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
    if (length(commit) == 1L && grepl("^[[:xdigit:]]{40}$", commit))
        common <- c(common, "--build-hash", commit)
}
for (operation in c("--prepare", "--build")) {
    args <- c(shQuote(compiler), operation, shQuote(root), common)
    if (operation == "--build")
        args <- c(args, "--jmo", shQuote(output))
    status <- system2(node, args)
    if (status != 0L)
        stop(operation, " failed with status ", status)
}

verify <- function() {
    stage <- tempfile("pathj-windows-verify-")
    dir.create(stage)
    on.exit(unlink(stage, recursive = TRUE), add = TRUE)
    utils::unzip(output, exdir = stage)
    foreign <- list.files(stage, pattern = "\\.(so|dylib)$", recursive = TRUE)
    if (length(foreign))
        stop("The archive contains non-Windows libraries: ", paste(foreign, collapse = ", "))
    dlls <- list.files(stage, pattern = "\\.dll$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (!length(dlls))
        stop("No Windows DLLs found in the module.")
    for (dll in dlls) {
        con <- file(dll, "rb")
        tryCatch({
            if (!identical(readBin(con, "raw", 2), charToRaw("MZ")))
                stop("Invalid Windows DLL: ", dll)
            seek(con, 60)
            offset <- readBin(con, "integer", 1, size = 4, endian = "little")
            seek(con, offset)
            if (!identical(readBin(con, "raw", 4), as.raw(c(80, 69, 0, 0))) ||
                readBin(con, "integer", 1, size = 2, signed = FALSE, endian = "little") != 34404L)
                stop("Expected an x64 Windows DLL: ", dll)
        }, finally = close(con))
    }
    message("Built Windows module: ", output, " (verified ", length(dlls), " x64 DLLs)")
}
verify()
