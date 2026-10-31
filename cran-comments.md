## Test environments

* local macOS (aarch64-apple-darwin20), R 4.3.1
* win-builder (devel and release)
* R-hub (linux, macos, windows)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release of `susolisting` (v0.2.0).
* Interactive Shiny application functions (`runListingApp()`, `runListingAppServer()`) wrap `shiny::runApp()` inside `if (interactive())` in their `@examples` sections as recommended by the CRAN Cookbook.
* In non-interactive sessions (such as `R CMD check` and automated unit tests), local file storage defaults to `tempdir()`, ensuring the user's home filespace is never modified.
