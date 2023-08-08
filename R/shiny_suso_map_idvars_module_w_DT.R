#'
#' @noRd
#' @keywords internal
#'

suso_map_idUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script("
    $(document).on('change', 'select', function() {
      var selectId = this.id;
      var selectValue = this.value;
      Shiny.onInputChange(selectId, selectValue);
    });
  "),
    actionButton(ns("suso_idvars"),
                 "Map Variables for Assignment Creation",
                 width = "100%",
                 icon("upload"),
                 style="color: #FFFFFF;
                 background-color: #1976D2;
                 border-color: #1976D2")
  )

}

#'
#' @noRd
#' @keywords internal
#'

suso_map_idUI_maptable<-function(id, height) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("fullmaptable"), height = height)
  )

}

#'
#' @noRd
#' @keywords internal
#'

suso_map_idSRV <- function(id,
                            varsLocal = reactive(NULL),
                            varsSuso = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    # Namespace
    ns<-session$ns

    stopifnot(
      shiny::is.reactive(varsLocal),
      shiny::is.reactive(varsSuso)
    )

    # styles
    styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")
    action_btn_close <-c("color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1")

    # Create the data.frame with selectInput fields for datatable
    df_for_table <- reactive({
      req(varsLocal(), varsSuso())

      # create suso choices
      susoChoices<-setNames(
        c("", paste(varsSuso()$VariableName)),
        c("", paste(varsSuso()$VariableName, varsSuso()$type))
      )
      # create local choices
      localChoices<-setNames(
        c("", paste(varsLocal()$VariableName)),
        c("", paste(varsLocal()$VariableName, varsLocal()$type))
      )
      data.frame(
        `Sample Variables` = sapply(1:nrow(varsLocal()), function(i) {
          as.character(selectInput(ns(paste0("select_", i)),
                                   "",
                                   choices = localChoices,
                                   selected = NULL ))
        }),
        `ID variables` = sapply(1:nrow(varsLocal()), function(i) {
          as.character(selectInput(ns(paste0("select2_", i)),
                                   "",
                                   choices = susoChoices,
                                   selected = NULL))
        })
      )
    })

    # Create table for modal
    output$maptable <- DT::renderDataTable({
      req(df_for_table())
      datatable(df_for_table(),
                escape = FALSE,
                rownames = F,
                colnames = c("Sample Variables", "ID variables"),
                options = list(
                  dom = 'tp',
                  paging = T,
                  searching = FALSE,
                  pageLength = 10,
                  columnDefs = list(list(className = 'dt-center', targets = '_all',
                                         width = '50%', targets = c(0),
                                         width = '50%', targets = c(1)
                  ))
                ),
                selection = "none") %>%
        formatStyle(1,
                    fontWeight = 'bold',
                    textAlign = 'center',
                    color = '#1976D2') %>%
        formatStyle(2,
                    fontWeight = 'bold',
                    textAlign = 'center',
                    color = '#1976D2')
    }, server = FALSE)

    # Modal for mapping
    observeEvent(input$suso_idvars, {
      req(df_for_table())

      showModal(modalDialog(title =tags$div(
        HTML("<center><font color='#0d47a1'><big>Mapping of Variables</big></font></center>")),
        fluidRow(
          div(style = "text-align: center;",
          helpText("Please select the variable from the corresponding variables from the sample to be mapped to the questionnaire.")
          )
        ),
        fluidRow(
          div(style = "text-align: center;",
          DT::dataTableOutput(ns("maptable"))
          )
        ),
        br(),
        fluidRow(
          column(8,
                 actionButton(ns("submit"),"Submit",
                              style=styleDwlButton)
          ),
          column(4,
                 actionButton(ns("cancel"),"Cancel",
                              style=action_btn_close)
          )
        ),
        footer = NULL,
        easyClose = T, size = "l"
      ))

    })
    # close modal (no action)
    observeEvent(input$cancel, {
      removeModal()
    })

    FULLVARMAP<-reactiveVal(NULL)
    observeEvent(input$submit, {
      removeModal()
      selected_in_col1 <- sapply(1:nrow(varsLocal()), function(i) input[[paste0("select_", i)]])
      selected_in_col2 <- sapply(1:nrow(varsLocal()), function(i) input[[paste0("select2_", i)]])

      # mapping table
      fullmap<-data.table::data.table(locvars=unlist(selected_in_col1), idvars=unlist(selected_in_col2))
      FULLVARMAP(fullmap)

      print(paste("Selected variable names from varsLocal():", paste(selected_in_col1, collapse = ", ")))
      print(paste("Selected variable names from varsSuso():", paste(selected_in_col2, collapse = ", ")))
    })

    # Create table for map overview
    output$fullmaptable <- DT::renderDataTable({
      req(FULLVARMAP())
      tabDT<-datatable(FULLVARMAP(),
                       escape = FALSE,
                       rownames = F,
                       colnames = c("Sample", "ID"),
                       options = list(
                         dom = 'tp',
                         paging = T,
                         searching = FALSE,
                         pageLength = 3,
                         columnDefs = list(list(className = 'dt-center', targets = '_all',
                                                width = '50%', targets = c(0),
                                                width = '50%', targets = c(1)
                         ))
                       ),
                       selection = "none") %>%
        formatStyle(1,
                    fontWeight = 'bold',
                    textAlign = 'center',
                    color = '#1976D2') %>%
        formatStyle(2,
                    fontWeight = 'bold',
                    textAlign = 'center',
                    color = '#1976D2')

      return(tabDT)
    }, server = FALSE)

    ########################
    # RETURNS
    return(FULLVARMAP)

  })
}

