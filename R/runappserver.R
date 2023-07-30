#' Start the Survey Solutions Listing Application on Shiny Server
#'
#' @description A wrappter function to start the application on the SERVER. Please make sure you have read the
#' documentation on how to use the app under <https://datanalytics.worldbank.org/SpatialSamplingManual/>
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
#' @importFrom data.table rbindlist data.table as.data.table
#' @importFrom shinyjs show
#' @importFrom fst read_fst write_fst
#' @importFrom readr write_csv
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs runjs
#' @importFrom sf st_as_sf st_is_longlat st_set_geometry st_transform

#' @export

runListingAppServer<-function(){
  shiny::addResourcePath("www", system.file("www", package = "susolisting"))
  shiny::shinyApp(ui = susolisting:::main_ui, server = susolisting:::main_server)
}
