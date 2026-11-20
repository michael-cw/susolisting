## Test environments

* local macOS (aarch64-apple-darwin20, macOS 26.0.1), R 4.3.1 (and R-release / R-devel via GitHub Actions / R-hub v2)
* win-builder (R-devel and R-release)

## R CMD check results

0 errors | 0 warnings | 1 note

* `checking CRAN incoming feasibility ... NOTE`
  `Maintainer: 'Michael Wild <michael.wild@me.com>'`
  `New submission`

  This is the initial CRAN submission of `susolisting` (v0.2.0).

* Interactive Shiny application launchers (`runListingApp()`, `runListingAppServer()`) guard `shiny::runApp()` inside `if (interactive())` in their `@examples` sections as recommended by the CRAN Cookbook.
* In non-interactive sessions (such as `R CMD check`, examples, and automated unit tests), local file storage defaults to `tempdir()`, ensuring the user's home filespace is never modified.
