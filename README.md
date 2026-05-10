# pathj: A Suite for Path Analysis

jamovi Path Analysis 
version > 1.*.*

Path analysis for *jamovi* based on lavaan, with jamovi output style and functions. Provides access to lavaan
             estimation with restructuring of data and options for executing most common tasks in path analysis.
Path diagrams can also be requested.

Syntax import for lavaan, Mermaid, Mplus-style SEM syntax, and OpenMx RAM-style
paths is documented in [docs/syntax-import.md](docs/syntax-import.md).

# Docs and help

Help and examples can be found at [PATHj page](https://pathj.github.io/)

# Install in jamovi

Please install [jamovi](https://www.jamovi.org/download.html) and run it. Select the jamovi modules library and install PATHj from there


<center>
<img width="600" src="https://pathj.github.io/install.png" class="img-responsive" alt="">
</center>


## From source


You will first need to download [jamovi](https://www.jamovi.org/download.html). 


You can clone this repository and compile the module within R with 

```
library(jmvtools)

jmvtools::install()

```

# Install in R

```

devtools::install_github("pathj/pathj")

```

## Missing data example

PATHj 1.1.7.9006 supports explicit missing-data handling in the SEM workflow. FIML is the default for ML models.

```r
results <- pathj(
  data = mydata,
  endogenous = c("y1", "y2"),
  covs = c("x1", "x2", "x3"),
  endogenousTerms = list(
    list("x1", "x2", "x3"),
    list("x2", "x3")
  ),
  missing = "fiml",
  showMissingDiagnostics = TRUE
)

results_mi <- pathj(
  data = mydata,
  endogenous = c("y1", "y2"),
  covs = c("x1", "x2", "x3"),
  endogenousTerms = list(
    list("x1", "x2", "x3"),
    list("x2", "x3")
  ),
  missing = "mi",
  miN = 5,
  miSeed = 12345
)
```
