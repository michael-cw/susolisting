#' Start the Survey Solutions Listing Application
#'
#' @description A wrappter function to start the application. Please make sure you have read the
#' documentation on how to use the app under <https://datanalytics.worldbank.org/SpatialSamplingManual/>
#'
#' @details
#' This application is part of the large set of tools, to facilitate survey implementation with
#' [Survey Solutions](https://docs.mysurvey.solutions/). Enumeration Areas, sampled for example
#' with the JDC-Survey Solutions Spatial Sampling application, can be used to enumerate all buildings
#' visible within the boundaries on a Google map. The enumerate buildings can then be used for the
#' second stage sampling frame and to draw the survey units within the cluster from it.
#'
#' @inherit shiny::runApp
#' @param admuser user name for admin
#' @param admpass user password for admin
#'
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinydashboard
#' @import DT
#' @import googleway
#' @importFrom data.table as.IDate data.table as.data.table as.ITime copy setnames setorderv tstrsplit
#' @importFrom shinyjs show
#' @importFrom fst read_fst write_fst
#' @importFrom readr write_csv
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs runjs
#' @importFrom sf st_as_sf st_is_longlat st_set_geometry st_transform
#' @importFrom waiter spin_fading_circles
#' @export

runListingApp <- function(launch.browser = T, admuser = NULL, admpass = NULL) {
  shiny::addResourcePath("www", system.file("www", package = "susolisting"))

  # get original options
  original_options <- list(
    shiny.maxRequestSize = getOption("shiny.maxRequestSize"),
    # You might want to store your original spinner.color.background if it's set somewhere in your code
    spinner.color.background = getOption("spinner.color.background"),
    admuser = NULL,
    admpass = NULL
  )
  # change options and revert on stop
  changeoptions <- function() {
    options(
      # Temporary change of environment options
      spinner.color.background = "#0d47a1",
      admuser = admuser,
      admpass = admpass
    )
    shiny::shinyOptions(shiny.maxRequestSize=5000*1024^2)

    # revert to original state at the end
    shiny::onStop(function() {
      if (!is.null(original_options)) {
        options(original_options)
      }
    })
  }
  # create app & run
  appObj<-shiny::shinyApp(ui = main_ui, server = main_server, onStart = changeoptions)
  shiny::runApp(appObj, launch.browser = launch.browser, quiet = T)
}
