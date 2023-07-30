#' Start the Survey Solutions Listing Application
#'
#' @description A wrappter function to start the application. Please make sure you have read the
#' documentation on how to use the app under <https://datanalytics.worldbank.org/SpatialSamplingManual/>
#'
#' @details
#' This application is part of the large set of tools, to facilitate survey implementation with
#' [Survey Solutions](https://docs.mysurvey.solutions/). Enumeration Areas, sampled for example
#' with the JDC-Survey Solutions Spatial Sampling application, can be used to enumerate all buildings
#' visible within the boundaries on a google map. The enumerate buildings can then be used for the
#' second stage sampling frame and to draw the survey units within the cluster from it.
#'
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinydashboard
#' @import DT
#' @import googleway
#' @importFrom data.table rbindlist data.table as.data.table
#' @importFrom shinyjs show
#' @importFrom fst read_fst write_fst
#' @importFrom readr write_csv
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs runjs
#' @importFrom sf st_as_sf st_is_longlat st_set_geometry st_transform

#' @export
runListingApp <- function() {
  shiny::addResourcePath("www", system.file("www", package = "susolisting"))

  withr::local_options(
    # Temporary change of environment options
    list(
      shiny.maxRequestSize=5000*1024^2,
      spinner.color.background="#0d47a1"
    ))

  appObj<-shiny::shinyApp(ui = main_ui, server = main_server)
  shiny::runApp(appObj)
}
