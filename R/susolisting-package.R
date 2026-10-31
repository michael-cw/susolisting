#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinydashboard
#' @import DT
#' @import googleway
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table as.IDate as.data.table as.ITime copy data.table fwrite rbindlist setnames setorderv tstrsplit
#' @importFrom fst read_fst write_fst
#' @importFrom graphics barplot grid legend par text
#' @importFrom grDevices dev.off png
#' @importFrom leaflet addLayersControl addMarkers addPolygons clearGroup fitBounds hideGroup invokeMethod layersControlOptions leaflet leafletOptions leafletOutput leafletProxy pathOptions renderLeaflet
#' @importFrom sf st_area st_as_sf st_bbox st_buffer st_convex_hull st_crs st_geometry st_intersection st_intersects st_is_empty st_is_longlat st_make_grid st_make_valid st_set_geometry st_transform st_union
#' @importFrom shiny NS tagList
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs runjs show
#' @importFrom stats ave median runif sd setNames
#' @importFrom utils packageVersion
#' @importFrom vantorr add_vantor_basemap add_vantor_seamlines vantor_auth vantor_has_auth
#' @importFrom waiter spin_fading_circles useWaiter waiter_hide waiter_show
## usethis namespace: end
NULL

if (getRversion() >= "3.3.0") {
  utils::globalVariables(c(
    ".",
    "area_id",
    "coverage_pct",
    "coverage_status",
    "ea",
    "ean",
    "Example",
    "Expression",
    "Featured",
    "idvars",
    "Instruction",
    "IsLocked",
    "LastEntryDate",
    "locvars",
    "Message",
    "PublicKey",
    "qCOUNTER",
    "QuestionnaireId",
    "QuestionnaireIdVersion",
    "QuestionText",
    "QuestionTextTrans",
    "rep_cont",
    "Reset",
    "Section",
    "siteID",
    "Team",
    "time",
    "Title",
    " type",
    "type",
    "Type",
    "UserName",
    "VariableName",
    "Version"
  ))
}
