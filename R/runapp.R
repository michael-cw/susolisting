#' Start the Survey Solutions Listing Application
#'
#' @description A wrapper function to start the interactive Shiny application.
#' Please make sure you have read the documentation on how to use the app at
#' <https://datanalytics.worldbank.org/SpatialSamplingManual/>.
#'
#' @details
#' This application is part of the suite of tools facilitating survey
#' implementation with [Survey Solutions](https://docs.mysurvey.solutions/).
#' Enumeration Areas (e.g., sampled with the JDC-Survey Solutions Spatial
#' Sampling application) can be used to enumerate all buildings visible within
#' the boundaries on a Google or Vantor (Maxar) satellite map. The enumerated
#' buildings can then be used for the second-stage sampling frame and to draw
#' survey units within each cluster.
#'
#' @inheritParams shiny::runApp
#' @param admuser Character string specifying the admin username (optional).
#' @param admpass Character string specifying the admin password (optional).
#' @param provider Character string specifying the satellite imagery provider:
#'   `"google"` (default, uses `googleway` and a Google Maps API key) or
#'   `"vantor"` / `"maxar"` (uses `leaflet` and `vantorr` with Vantor/Maxar API
#'   credentials).
#'
#' @return No return value, called for side effects (launches the interactive
#'   'Shiny' application).
#'
#' @examples
#' if (interactive()) {
#'   runListingApp()
#'   runListingApp(provider = "vantor")
#' }
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinydashboard
#' @import DT
#' @import googleway
#' @importFrom leaflet addLayersControl addMarkers addPolygons clearGroup fitBounds hideGroup invokeMethod layersControlOptions leaflet leafletOptions leafletOutput leafletProxy pathOptions renderLeaflet
#' @importFrom vantorr add_vantor_basemap add_vantor_seamlines vantor_auth vantor_has_auth
#' @importFrom data.table as.IDate data.table as.data.table as.ITime copy setnames setorderv tstrsplit rbindlist
#' @importFrom shinyjs show runjs
#' @importFrom fst read_fst write_fst
#' @importFrom readr write_csv
#' @importFrom shinyalert shinyalert
#' @importFrom sf st_area st_as_sf st_bbox st_buffer st_convex_hull st_crs st_geometry st_intersection st_intersects st_is_empty st_is_longlat st_make_grid st_make_valid st_set_geometry st_transform st_union
#' @importFrom waiter spin_fading_circles useWaiter waiter_show waiter_hide
#' @importFrom graphics barplot grid legend par text
#' @importFrom stats median runif sd setNames
#' @importFrom utils packageVersion
#' @export
runListingApp <- function(launch.browser = TRUE,
                          admuser = NULL,
                          admpass = NULL,
                          provider = c("google", "vantor", "maxar")) {
  provider <- resolve_map_provider(provider)
  shiny::addResourcePath("www", system.file("www", package = "susolisting"))

  old_opts <- options(
    shiny.maxRequestSize = 5000 * 1024^2,
    spinner.color.background = "#0d47a1",
    admuser = admuser,
    admpass = admpass,
    susolisting.provider = provider
  )
  on.exit(options(old_opts), add = TRUE)

  changeoptions <- function() {
    options(
      shiny.maxRequestSize = 5000 * 1024^2,
      spinner.color.background = "#0d47a1",
      admuser = admuser,
      admpass = admpass,
      susolisting.provider = provider
    )
    shiny::shinyOptions(shiny.maxRequestSize = 5000 * 1024^2)

    shiny::onStop(function() {
      options(old_opts)
    })
  }

  appObj <- shiny::shinyApp(ui = main_ui, server = main_server, onStart = changeoptions)
  shiny::runApp(appObj, launch.browser = launch.browser, quiet = TRUE)
}
