
<!-- README.md is generated from README.Rmd. Please edit that file -->

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

1.  **Enumeration of Buildings**: Utilizing Enumeration Areas (e.g.,
    those sampled with the [JDC-Survey Solutions Spatial Sampling
    application](https://github.com/michael-cw/susogrdframe)), you can
    identify and enumerate all visible buildings within specified
    boundaries on Google Maps. The enumerated buildings form the second
    stage sampling frame, allowing for precise extraction of survey
    units within the cluster.

2.  **Admin Interface**: This intuitive interface lets you:

    - Extract all enumerated areas from the individual user directories
      of your collaborators.
    - Construct the frame, which becomes the basis for drawing the
      sample.

3.  **Sample Drawing**: For this stage, the application leverages the
    [‘spsurvey’](https://usepa.github.io/spsurvey/) package, which
    facilitates the drawing of the sample.

4.  **Assignment to Supervisors**: Once the sample is generated, you can
    assign it directly to the Survey Solutions supervisors. This
    assignment is performed through a simple interface grounded on the
    Survey Solutions REST API.

#### Usage Overview

The Survey Solutions Listing Application not only accelerates survey
implementation but also provides robust capabilities for managing
complex survey projects. By enabling an efficient workflow from
enumeration to sample extraction, this application stands as a vital
resource for researchers and survey professionals.

For detailed information on how to get started, please consult our
[documentation](https://datanalytics.worldbank.org/SpatialSamplingManual/).

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

## Running the application interactively

There are two options to run the application. The first one is in
*local* mode, without *admuser* and *admpass* credentials provided. In
this mode, the user needs to download her work, with the *enabled*
download Button. The work is still stored, until the download button is
clicked. Once the file is created, the directory will be flushed, \*\*so
make sure, you have finished all the enumeration areas/segments and the
downloaded file is safely stored.

``` r
library(susolisting)
susolisting::runListingApp()
```

The second option allows you to run the application in *team* mode. This
mode allows you to work with several *desk enumerators* either on the
same device or everyone on their own devices, and copying the file then
to the administrator device. The administrator can then compile a single
sampling frame based on all the enumerated areas, sample from it, and
finally directly assign the sampled units to the survey teams on the
survey solution server. There is also a mapping module included, which
allows you to map the frame variables to the Survey Solutions
questionnaire Identification variables used on the [Cover
Page](https://docs.mysurvey.solutions/questionnaire-designer/components/special-section-cover/).
The GPS location of the sample units is automatically assigned to the
GPS variable from the identifying variables. To use this mode, run:

``` r
library(susolisting)
susolisting::runListingApp(admuser = "you", admpass = "your_password")
```

## Running the application on a Shiny Server

The package also contains a shiny server function (open source or pro),
which you can run, i.e. in an Ubuntu Virtual Machine on your PC. The
same two modes are available. To run it on a shiny server, you need to
create a server location (= directory), i.e. *susolisting* and then put
an *app.R* script into this directory. If you run in *local* mode, the
script contains the following two lines:

``` r
library(susolisting)
susolisting::runListingAppServer()
```

or in *team* mode:

``` r
library(susolisting)
susolisting::runListingAppServer(admuser = "you", admpass = "your_password")
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

## Feature requests and bug reports

You can either use the standard GitHub approach by filing a bug
report/feature request
[here](https://github.com/michael-cw/SurveySolutionsAPI/issues) or you
use the Survey Solutions user forum
[here](https://forum.mysurvey.solutions/c/api/13).

Please continue to check for updates, as we are constantly working to
improve the application.

</div>
