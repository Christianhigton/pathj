#!/usr/bin/env Rscript

# Exercise the installed package inside a JMO, without installing it into jamovi.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[[1]]))
    stop("Usage: Rscript scripts/smoke_module.R path/to/module.jmo")
stage <- tempfile("pathj-module-smoke-")
dir.create(stage)
utils::unzip(args[[1]], exdir = stage)
module_library <- file.path(stage, "pathj", "R")
runtime_libraries <- strsplit(Sys.getenv("PATHJ_RUNTIME_LIBS"), .Platform$path.sep, fixed = TRUE)[[1]]
.libPaths(c(module_library, runtime_libraries, .libPaths()))
library(pathj)
stopifnot(normalizePath(find.package("pathj")) ==
          normalizePath(file.path(module_library, "pathj")))
stopifnot(utils::packageVersion("lavaan") >= "0.7.2")
stopifnot(is.function(getFromNamespace("plot_path_diagram", "pathj")))

data <- read.csv("data/workplace_wellbeing.csv")
model <- list("sleep_quality ~ stress + wellbeing",
              "stress ~ support_person + resilience + workload",
              "support_person ~ wellbeing", "wellbeing ~ resilience")
result <- pathj::pathj(data = data, formula = model,
    varcov = list(c("stress", "wellbeing"), c("resilience", "support_person")),
    multigroup = "condition", missing = "fiml", indirect = TRUE,
    modindices = TRUE, intelligentReport = TRUE, diagram = TRUE,
    diag_abbrev = "0", diag_sigstars = TRUE)
defined <- result$models$defined$asDF
stopifnot(sum(grepl("^DIE[0-9]+$", defined$lhs)) == 6L)
stopifnot(all(result$intelligent$variableTypes$asDF$ordered == "FALSE"))
stopifnot(all(result$intelligent$groupComparison$asDF$significant %in% c("TRUE", "FALSE")))
r2 <- result$models$r2$asDF
stopifnot(all(r2$ci.lower <= r2$r2 & r2$r2 <= r2$ci.upper))
for (group in c("Control", "Training")) {
    image <- file.path(stage, paste0("workplace-", group, ".pdf"))
    result$pathgroup$diagrams$get(key = group)$saveAs(image)
    stopifnot(file.info(image)$size > 1000)
}
message("Packaged module passed workplace analysis and both group diagram exports: ", R.version.string)
unlink(stage, recursive = TRUE)
