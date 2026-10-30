# susolisting 0.2.0

* Replaced deprecated `googleway::add_drawing()` (`google.maps.drawing.DrawingManager`, removed in Google Maps JS API v3.65+) with a custom Google Maps marker drawing control and click handler (`add_marker_drawing()`, `clear_marker_drawing()`, `suso_drawing_js()`).
* Removed deprecated `shinyWidgets::useShinydashboard()` and `shinyalert::useShinyalert()` calls and dropped unused `shinyWidgets` dependency.
* Migrated Survey Solutions integration from `SurveySolutionsAPI` to `SurveySolutionsAPIv2`.
* Updated `DT` calls to `DT::DTOutput()` and `DT::renderDT()`, `waiter` to `waiter::useWaiter()`, and `dplyr::select()` to use `dplyr::all_of()`.
* Fixed `project_to_utm()` EPSG formatting for single-digit UTM zones and retained take-all strata in 2nd-stage spatial sampling.
* Added comprehensive `testthat` unit and module server tests and refactored documentation.
* Added admin interface to extract listing data and compile the frame.
* Added `spsurvey` package to generate spatially balanced samples.
* Added Survey Solutions interface for assignment creation.

# susolisting 0.1.1

* added shinyjs for navigation buttons
* added dependency on googleway 2.7.5 or later
* added option change and reversion 

# susolisting 0.1.0

* Initial release
