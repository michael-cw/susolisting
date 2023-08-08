#' Start the Survey Solutions Listing Application on Shiny Server
#'
#' @description A wrappter function to start the application on the SERVER. Please make sure you have read the
#' documentation on how to use the app under <https://datanalytics.worldbank.org/SpatialSamplingManual/>
#'
#' @param admuser user name for admin
#' @param admpass user password for admin
#'
#' @details
#' This function only works on shiny server to do so, create a script in the app directory and
#' run this function. An example myListingApp.R file could be:
#'
#' ```
#' susolisting::runListingAppServer()
#'
#' ```
#'
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinydashboard
#' @import DT
#' @import googleway
#' @importFrom data.table as.IDate data.table as.data.table as.ITime copy setnames setorderv tstrsplit rbindlist
#' @importFrom shinyjs show
#' @importFrom fst read_fst write_fst
#' @importFrom readr write_csv
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs runjs
#' @importFrom sf st_as_sf st_is_longlat st_set_geometry st_transform
#' @importFrom waiter spin_fading_circles
#' @importFrom graphics text
#' @importFrom stats runif setNames

#' @export

runListingAppServer<-function(admuser = NULL, admpass = NULL){
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
  shiny::shinyApp(ui = susolisting:::main_ui, server = susolisting:::main_server, onStart = changeoptions)
}
