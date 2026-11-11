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
# Exercise the regenerated result schema as well as the source calculations.
source("tests/testthat/helper-die.R")
for (equal in c(FALSE, TRUE)) {
    m <- die_result(equal=equal)
    definitions <- m$models$defined$asDF
    index <- m$intelligent$moderatedIndex$asDF
    stopifnot(sum(grepl("^IE", definitions$lhs)) == 16L)
    if (equal) {
        stopifnot(!any(grepl("^D(IE|TE)", definitions$lhs)),
                  nrow(m$intelligent$groupComparison$asDF) == 0L,
                  all(is.na(index$index)),
                  grepl("zero by design", index$note[1], fixed=TRUE))
        info <- m$info$asDF
        stopifnot(as.numeric(info$value[which(info$info == "Free parameters")]) ==
                  as.numeric(lavaan::fitMeasures(m$model, "npar")))
    } else {
        stopifnot(sum(grepl("^DIE", definitions$lhs)) == 8L,
                  sum(grepl("^DTE", definitions$lhs)) == 4L,
                  nrow(index) == 8L,
                  isTRUE(all.equal(index$index, definitions$est[grepl("^DIE", definitions$lhs)])),
                  all(is.na(definitions$std.all[grepl("^D(IE|TE)", definitions$lhs)])))
    }
}
three <- die_result(groups=3L)$intelligent$moderatedIndex$asDF
stopifnot(sum(is.finite(three$index)) == 24L,
          sum(is.finite(three$wald)) == 8L,
          all(three$df[is.finite(three$wald)] == 2))
message("Packaged module passed workplace diagrams and two-/three-group DIE checks: ", R.version.string)
unlink(stage, recursive = TRUE)
