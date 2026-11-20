#' Start the Survey Solutions Listing Application
#'
#' @description A wrapper function to start the interactive Shiny application.
#' Please make sure you have read the documentation on how to use the app at
#' <https://datanalytics.worldbank.org/SpatialSamplingManual/>.
#'
#' @details
#' This application is part of the suite of tools facilitating survey
#' implementation with [Survey Solutions](https://docs.mysurvey.solutions/).
#' Enumeration Areas (e.g., sampled with the Survey Solutions Spatial
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
#' @param n_grid Integer specifying the number of grid cells along each axis
#'   used for the spatial polygon coverage check (default `10L`, producing a
#'   `10 x 10` spatial grid).
#' @param logo_path Optional character string specifying the file path to a
#'   custom PNG logo image to display on the front cover of the Word sampling
#'   report. If `NULL` (default), uses `getOption("susolisting.logo_path")` or
#'   falls back to the built-in package logo (`inst/www/susospatial.png`).
#'
#' @return No return value, called for side effects (launches the interactive
#'   'Shiny' application).
#'
#' @examples
#' if (interactive()) {
#'   runListingApp()
#'   runListingApp(provider = "vantor", n_grid = 10L)
#' }
#'
#' @export
runListingApp <- function(launch.browser = TRUE,
                          admuser = NULL,
                          admpass = NULL,
                          provider = c("google", "vantor", "maxar"),
                          n_grid = 10L,
                          logo_path = NULL) {
  provider <- resolve_map_provider(provider)
  n_grid <- resolve_n_grid(n_grid)
  shiny::addResourcePath("www", system.file("www", package = "susolisting"))

  old_opts <- options(
    shiny.maxRequestSize = 5000 * 1024^2,
    spinner.color.background = "#0d47a1",
    admuser = admuser,
    admpass = admpass,
    susolisting.provider = provider,
    susolisting.n_grid = n_grid,
    susolisting.logo_path = logo_path
  )
  on.exit(options(old_opts), add = TRUE)

  changeoptions <- function() {
    options(
      shiny.maxRequestSize = 5000 * 1024^2,
      spinner.color.background = "#0d47a1",
      admuser = admuser,
      admpass = admpass,
      susolisting.provider = provider,
      susolisting.n_grid = n_grid,
      susolisting.logo_path = logo_path
    )
    shiny::shinyOptions(shiny.maxRequestSize = 5000 * 1024^2)

    shiny::onStop(function() {
      options(old_opts)
    })
  }

  appObj <- shiny::shinyApp(ui = main_ui, server = main_server, onStart = changeoptions)
  shiny::runApp(appObj, launch.browser = launch.browser, quiet = TRUE)
}
