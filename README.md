
<!-- README.md is generated from README.Rmd. Please edit that file -->

# susolisting shiny application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This application is part of the large set of tools, to facilitate survey
implementation with [Survey
Solutions](https://docs.mysurvey.solutions/). Enumeration Areas, sampled
for example with the JDC-Survey Solutions Spatial Sampling application,
can be used to enumerate all buildings visible within the boundaries on
a google map. The enumerate buildings can then be used for the second
stage sampling frame and to draw the survey units within the cluster
from it.

## Installation

- Install R: <https://cran.r-project.org/mirrors.html> (version 4.1.1 or
  greater)

- Install R Studio: <https://rstudio.com/products/rstudio/download/>
  (version 1.2.5001-3 or newer)

- Make sure the *devtools* package is installed, if not install it with:

``` r
install.packages("devtools")
```

- After that install the actual package:

``` r
devtools::install_github("michael-cw/susolisting")
```

## Start the application

``` r
library(susolisting)
susolisting::runListingApp()
```
