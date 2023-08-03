
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Survey Solutions Listing Application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<div style="text-align: justify">

The Survey Solutions Listing Application is a key part of a broad
toolkit designed to streamline survey implementation using [Survey
Solutions](https://docs.mysurvey.solutions/). By employing Enumeration
Areas (for instance, those sampled using the [JDC-Survey Solutions
Spatial Sampling
application](https://github.com/michael-cw/susogrdframe)), you can
enumerate all visible buildings within specified boundaries on Google
Maps. These enumerated buildings can then serve as the second stage
sampling frame, enabling the extraction of survey units within the
cluster from it.

## Prerequisites

This application requires a Google Maps API key. Get yours
[here](https://mapsplatform.google.com/)

## Installation

1.  Install R: <https://cran.r-project.org/mirrors.html> (version 4.1.1
    or greater)

2.  Install R Studio: <https://rstudio.com/products/rstudio/download/>
    (version 1.2.5001-3 or newer)

3.  Make sure the *devtools* package is installed, if not install it
    with:

``` r
install.packages("devtools")
```

4.  After that install the actual package:

``` r
devtools::install_github("michael-cw/susolisting")
```

## Running the application

``` r
library(susolisting)
susolisting::runListingApp()
```

#### Attention - Potential issue with ‘shinyalert’

There’s a known issue with the ‘shinyalert’ package from Dean Attali,
which may cause the application to fail during start-up on certain
Windows installations running the latest version of R. For more details,
please check this
[issue](https://github.com/daattali/shinyalert/issues/75).

This issue has been resolved in the development version of ‘shinyalert’,
but the fix is not yet available in the official CRAN release. If you
encounter this problem, please install the development version for
‘shinyalert’ using the following command:

``` r
devtools::install_github("daattali/shinyalert")
```

Please continue to check for updates, as we are constantly working to
improve the application.

</div>
