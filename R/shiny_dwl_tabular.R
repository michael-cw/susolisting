#' Shiny module UI for downloading tabular data as a zip archive
#'
#' @param id Character string specifying the module namespace ID.
#' @param label Character string for the download action button label.
#' @param style CSS style string for the action button.
#'
#' @return A `shiny.tag.list` object containing the download controls.
#' @noRd
#' @keywords internal
dwl_dataUI <- function(id,
                       label = "Download Data",
                       style = "color: #FFFFFF; width: 180px; background-color: #1976D2; border-color: #0d47a1") {
  invisibleButton <- "color: #FFFFFF; background-color: #FFFFFF; visibility: hidden; border-color: #FFFFFF; margin: 0; height: 1px;"

  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::column(
        10,
        shiny::actionButton(
          ns("generateDownloadOfTable"),
          label = label,
          shiny::icon("download"),
          width = "100%",
          style = style
        )
      ),
      shiny::column(1)
    ),
    shiny::fluidRow(
      shiny::downloadButton(ns("dwl_table"), "Not visible", style = invisibleButton)
    )
  )
}

#' Shiny module server for downloading tabular data as a zip archive
#'
#' @param id Character string specifying the module namespace ID.
#' @param file_name Reactive expression returning the base filename (without extension).
#' @param content Reactive expression returning the tabular data to export.
#'
#' @return Invisible `NULL` (used for its Shiny download handler side-effects).
#' @noRd
#' @keywords internal
download_csv_server <- function(id, file_name = NULL, content = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$generateDownloadOfTable, {
      shiny::validate(shiny::need(content(), message = FALSE))

      fn <- file.path(tempdir(), paste0(file_name(), ".csv"))
      data.table::fwrite(content(), file = fn)
      fnzip <- file.path(tempdir(), paste0(file_name(), ".zip"))
      zip::zip(zipfile = fnzip, files = fn, mode = "cherry-pick")

      shinyjs::click("dwl_table")
    })

    output$dwl_table <- shiny::downloadHandler(
      filename = function() {
        paste0(file_name(), ".zip")
      },
      content = function(file) {
        fn <- file.path(tempdir(), paste0(file_name(), ".csv"))
        fnzip <- file.path(tempdir(), paste0(file_name(), ".zip"))
        on.exit({
          if (file.exists(fnzip)) file.remove(fnzip)
          if (file.exists(fn)) file.remove(fn)
        }, add = TRUE)
        file.copy(fnzip, file)
      },
      contentType = "application/zip"
    )
  })
}
