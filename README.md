# MLR
R package for multiple linear regression

## Overview
The **MLR** package is designed to simplify multiple linear regression analysis in R. Included in the package is the function `regression_fit()`, which fits a linear model based on the provided formula and dataset. This produces a list containing coefficient estimates, residuals, an ANOVA test, and several other key regression components. More information on `regression_fit()` can be found in the package help pages, with complete examples included under *Vignettes*.

## Installation
To install the package from github, simply use the `install_github()` function from the **devtools** package.

```
install.packages("devtools")
devtools::install_github("dclarkboucher/MLR")
```
## Authorship
This package was created in 2020 by Dylan Clark-Boucher, a student of Biostatistics at the University of Michigan, as part of *BIOS 625: Big Data Computing*, a graduate-level course taught by Professor Hui Jiang, Ph.D.
