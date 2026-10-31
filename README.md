
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href='https://docs.mysurvey.solutions/'><img src="man/figures/susospatial.png" alt="Survey Solutions Spatial Sampling and Listing logo" align="right" height="139" style="float:right; height:139px;"/></a>

# Survey Solutions Listing Application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<div align="justify">

The Survey Solutions Listing Application forms an integral part of a
comprehensive toolkit designed to enhance the process of survey
implementation through [Survey
Solutions](https://docs.mysurvey.solutions/). This application provides
valuable tools for enumerating and extracting survey units.

#### Key Features

1.  **Enumeration of Buildings & Spatial Coverage Check**: Utilizing
    Enumeration Areas (e.g., those sampled with the [JDC-Survey
    Solutions Spatial Sampling
    application](https://github.com/michael-cw/susogrdframe)), you can
    identify and enumerate all visible buildings within specified
    boundaries on Google Maps or Vantor (Maxar) satellite imagery via
    [`vantorr`](https://github.com/michael-cw/vantorr). When saving an
    area, a spatial grid coverage check (`n_grid = 10L` by default)
    computes the percentage of the polygon covered by building markers
    and flags low-coverage areas (`< 20%`).

2.  **Admin Interface**: This intuitive interface lets you:

    - Extract all enumerated areas from the individual user directories
      of your collaborators.
    - Construct the frame and review cluster-level population and
      spatial coverage diagnostics, which become the basis for drawing
      the sample.

3.  **Sample Drawing & Reporting**: For this stage, the application
    leverages the [`spsurvey`](https://usepa.github.io/spsurvey/)
    package, which facilitates the drawing of spatially balanced or
    random second-stage samples, and
    [`officer`](https://CRAN.R-project.org/package=officer) to generate
    a World Bank Group-styled Word (`.docx`) sampling report.

4.  **Assignment to Supervisors**: Once the sample is generated, you can
    assign it directly to the Survey Solutions supervisors. This
    assignment is performed through a streamlined interface grounded on
    [`SurveySolutionsAPIv2`](https://CRAN.R-project.org/package=SurveySolutionsAPIv2)
    (REST and GraphQL APIs).

#### Usage Overview

The Survey Solutions Listing Application not only accelerates survey
implementation but also provides robust capabilities for managing
complex survey projects. By enabling an efficient workflow from
enumeration to sample extraction, this application stands as a vital
resource for researchers and survey professionals.

For detailed information on how to get started, please consult our
[documentation](https://datanalytics.worldbank.org/SpatialSamplingManual/).

## Prerequisites

This application requires either a Google Maps API key (get yours
[here](https://mapsplatform.google.com/)) when using
`provider = "google"` (the default), or Vantor / Maxar API credentials
when using `provider = "vantor"`.

## Installation

1.  Install R: <https://CRAN.R-project.org/mirrors.html> (version 4.1.0
    or greater)

2.  Install RStudio: <https://posit.co/downloads/>

3.  Make sure the `devtools` package is installed, if not install it
    with:

``` r
install.packages("devtools")
```

4.  Install `vantorr` and `susolisting` from GitHub (CRAN dependencies
    including `SurveySolutionsAPIv2` are installed automatically):

``` r
devtools::install_github("michael-cw/vantorr")
devtools::install_github("michael-cw/susolisting")
```

## Running the application interactively

There are two options to run the application. The first one is in
*local* mode, without `admuser` and `admpass` credentials provided. In
this mode, the user needs to download their work with the *enabled*
download button. The work is stored locally until the download button is
clicked. Once the archive is created, the directory will be flushed,
**so make sure you have finished all the enumeration areas/segments and
the downloaded file is safely stored.**

``` r
library(susolisting)
# Default: Google Maps satellite imagery (10x10 spatial coverage grid)
susolisting::runListingApp(provider = "google", n_grid = 10L)

# Or use Vantor / Maxar Vivid satellite imagery via leaflet + vantorr:
susolisting::runListingApp(provider = "vantor", n_grid = 10L)
```

The second option allows you to run the application in *team* mode. This
mode allows you to work with several *desk enumerators* either on the
same device or everyone on their own devices, and copying the files to
the administrator device. The administrator can then compile a single
sampling frame based on all the enumerated areas, sample from it, and
finally directly assign the sampled units to the survey teams on the
Survey Solutions server. There is also a mapping module included, which
allows you to map the frame variables to the Survey Solutions
questionnaire Identification variables used on the [Cover
Page](https://docs.mysurvey.solutions/questionnaire-designer/components/special-section-cover/).
The GPS location of the sample units is automatically assigned to the
GPS variable from the identifying variables. To use this mode, run:

``` r
library(susolisting)
susolisting::runListingApp(admuser = "you", admpass = "your_password", n_grid = 10L)
```

## Running the application on a Shiny Server

The package also contains a Shiny Server function (open source or pro),
which you can run, e.g., on an Ubuntu server or Virtual Machine. The
same two modes are available. To run it on a Shiny Server, create a
server directory (e.g., `susolisting`) and place an `app.R` script into
this directory. If you run in *local* mode, the script contains the
following two lines:

``` r
library(susolisting)
susolisting::runListingAppServer()
```

or in *team* mode:

``` r
library(susolisting)
susolisting::runListingAppServer(admuser = "you", admpass = "your_password")
```

## Feature requests and bug reports

You can either use the standard GitHub approach by filing a bug
report/feature request
[here](https://github.com/michael-cw/susolisting/issues) or use the
Survey Solutions user forum
[here](https://forum.mysurvey.solutions/c/api/13).

Please continue to check for updates, as we are constantly working to
improve the application.

</div>
