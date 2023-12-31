#' shiny module for  download of tabular data as zip file
#'
#'
#'
#' @noRd
#' @keywords internal
#'




# ui
dwl_dataUI<-function(id,
                     label = "Download Data",
                     style = "color: #FFFFFF; width: 180px;background-color: #1976D2;border-color: #0d47a1") {
  styleActButtonActivate<-c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1; margin:0% 20% 0% 0%;")

  styleActButtonActivate<-c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1; margin:0% 20% 0% 0%;")
  styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")

  invisibleButton<-c("color: #FFFFFF; background-color: #FFFFFF; visibility: hidden;
                  border-color: #FFFFFF; margin:0% 0% 0% 0%;height:1px;")

  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(10,
             ## 1. Action button to start the dwl
             actionButton(ns("generateDownloadOfTable"),
                          label = label,
                          icon("download"), width = "100%",
                          style=style)
      ),
      column(1)
    ),
    fluidRow(
      ## 2. DWL Button is INVISIBLE, activated by shinyjs::click
      downloadButton(ns("dwl_table"), "Not visible", style=invisibleButton)
    )
  )
  ####################FIN UI####################################################
}
#'
#' @noRd
#' @keywords internal
#'

# server
download_csv_server <- function(id,
                                file_name = NULL,
                                content = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      # # enable download button
      # observe({
      #   req(content())
      #   shinyjs::enable("generateDownloadOfTable")
      # })

      observeEvent(input$generateDownloadOfTable, {
        shiny::validate(need(content(), message = F))

        fn<-file.path(tempdir(), paste0(file_name(), ".csv"))
        data.table::fwrite(content(), file = fn)
        fnzip<-file.path(tempdir(), paste0(file_name(), ".zip"))
        zip::zip(zipfile=fnzip, files=fn, mode = "cherry-pick")

        ## 2.5. Click DWL button
        shinyjs::click("dwl_table")
      })

      output$dwl_table <- downloadHandler(
        filename = function() {
          paste0(file_name(), ".zip")
        },
        content = function(file) {
          fn<-file.path(tempdir(), paste0(file_name(), ".csv"))
          fnzip<-file.path(tempdir(), paste0(file_name(), ".zip"))
          on.exit(
            file.remove(fnzip),
            file.remove(fn)
          )
          file.copy(fnzip, file)
        }, contentType = "application/zip")

      ####################FIN SERVER####################################################




    }
  )
}
