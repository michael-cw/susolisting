# susolisting 0.2.0

* Added support for Vantor / Maxar Vivid satellite imagery (`provider = c("google", "vantor", "maxar")`) via `leaflet` and `vantorr` (>= 0.3.0), including `runListingApp()` and `runListingAppServer()`.
* Added a spatial grid polygon coverage check (`calculate_polygon_coverage()`, `summarize_cluster_coverage()`, and `n_grid = 10L` parameter) to compute the percentage of each Enumeration Area polygon covered by marked building locations and flag low-coverage clusters (`< 20%`) in the sidebar `AreaSummary` table, Admin `Frame Summary`, and Word sampling report.
* Added a dedicated Word (`.docx`) Sampling Report module (`R/shiny_sampling_report_module.R`) powered by `officer` and styled according to the World Bank Group Branding and Visual Identity Guidelines.
* Replaced deprecated `googleway::add_drawing()` (`google.maps.drawing.DrawingManager`, removed in Google Maps JS API v3.65+) with a custom Pan / Marker drawing control and click handler (`add_marker_drawing()`, `clear_marker_drawing()`, `suso_drawing_js()`).
* Migrated Survey Solutions integration from `SurveySolutionsAPI` to `SurveySolutionsAPIv2`.
* Removed `shinyWidgets`, `dplyr`, `readr`, `stringr`, and `magrittr` dependencies in favor of base R (`>= 4.1.0`) and `data.table`.
* Updated `DT` calls to `DT::DTOutput()` and `DT::renderDT()`, and `waiter` to `waiter::useWaiter()`.
* Fixed `project_to_utm()` EPSG formatting for single-digit UTM zones and retained take-all strata in 2nd-stage spatial sampling.
* Added three workflow vignettes (`enumerator-workflow`, `admin-sampling-workflow`, `data-flow-and-suso-assignment`) and comprehensive `testthat` unit and module server tests.
* Added admin interface to extract listing data and compile the 2nd-stage frame.
* Added `spsurvey` integration to generate spatially balanced (`grts`) or random (`irs`) 2nd-stage samples.
* Added Survey Solutions interface for automated supervisor assignment creation.

# susolisting 0.1.1

* Added `shinyjs` for navigation buttons.
* Added dependency on `googleway` 2.7.5 or later.
* Added option change and reversion.

# susolisting 0.1.0

* Initial release.
