#' Shiny UI module for uploading a text (CSV/TSV) or zipped CSV file
#'
#' @param id Character string specifying the module namespace ID.
#' @param label Character string for the file input label.
#' @param accept Character vector of accepted MIME types or extensions.
#'
#' @return A `shiny.tag.list` object containing the [shiny::fileInput()] control.
#' @noRd
#' @keywords internal
zipFileInput_ui <- function(id, label, accept) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("frame_file"), label, multiple = FALSE, accept = accept)
  )
}

#' Shiny server module for uploading a text (CSV/TSV) or zipped CSV file
#'
#' @param id Character string specifying the module namespace ID.
#' @param sep Character string specifying the column separator (default `","`).
#' @param zipInput Logical indicating whether the uploaded file is a zip archive.
#'
#' @return A reactive expression returning a `data.table` parsed via
#'   [data.table::fread()].
#' @noRd
#' @keywords internal
zipFileInput_server <- function(id, sep = ",", zipInput = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    userFile <- shiny::reactive({
      shiny::validate(shiny::need(input$frame_file, message = FALSE))
      input$frame_file
    })

    frameFile <- shiny::reactive({
      if (!zipInput) {
        filein <- data.table::fread(userFile()$datapath, sep = sep)
      } else {
        shpFiles <- file.path(tempdir(), "zip_csv_input")
        unlink(shpFiles, recursive = TRUE)
        dir.create(shpFiles, recursive = TRUE, showWarnings = FALSE)

        zip::unzip(
          userFile()$datapath,
          exdir = shpFiles,
          overwrite = TRUE,
          junkpaths = TRUE
        )
        fileList <- dir(shpFiles, recursive = TRUE, full.names = TRUE)
        csvFile <- fileList[grep("\\.csv$", fileList)]
        if (length(csvFile) == 0) {
          shiny::showModal(shiny::modalDialog(
            title = "Wrong Format!",
            "Your file is not provided in the correct format (zipped CSV file). Please check and upload again.",
            easyClose = TRUE,
            footer = NULL
          ))
          shiny::req(FALSE)
        }
        filein <- data.table::fread(file = csvFile[1], sep = sep)
      }
      return(filein)
    })

    return(frameFile)
  })
}
