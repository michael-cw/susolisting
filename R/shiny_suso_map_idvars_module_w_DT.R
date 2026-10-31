#' Shiny module UI button for mapping sample variables to Survey Solutions IDs
#'
#' @param id Character string specifying the module namespace ID.
#'
#' @return A `shiny.tag.list` object containing the action button and input script.
#' @noRd
#' @keywords internal
suso_map_idUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$script(shiny::HTML("
    $(document).on('change', 'select', function() {
      var selectId = this.id;
      var selectValue = this.value;
      if (window.Shiny && typeof Shiny.setInputValue === 'function') {
        Shiny.setInputValue(selectId, selectValue);
      }
    });
    ")),
    shiny::actionButton(
      ns("suso_idvars"),
      "Map Variables for Assignment Creation",
      width = "100%",
      shiny::icon("upload"),
      style = "color: #FFFFFF; background-color: #1976D2; border-color: #1976D2"
    )
  )
}

#' Shiny module UI table output for displaying mapped variables
#'
#' @param id Character string specifying the module namespace ID.
#' @param height CSS height string for the DT table.
#'
#' @return A `shiny.tag.list` object containing the DT output.
#' @noRd
#' @keywords internal
suso_map_idUI_maptable <- function(id, height) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("fullmaptable"), height = height)
  )
}

#' Shiny module server for mapping sample variables to Survey Solutions ID variables
#'
#' @param id Character string specifying the module namespace ID.
#' @param varsLocal Reactive expression returning a `data.table` of local variables
#'   (`VariableName`, `type`).
#' @param varsSuso Reactive expression returning a `data.table` of Survey Solutions
#'   featured ID variables (`VariableName`, `type`).
#'
#' @return A reactive value (`reactiveVal`) holding a `data.table` with columns
#'   `locvars` and `idvars`.
#' @noRd
#' @keywords internal
suso_map_idSRV <- function(id,
                           varsLocal = shiny::reactive(NULL),
                           varsSuso = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    stopifnot(
      shiny::is.reactive(varsLocal),
      shiny::is.reactive(varsSuso)
    )

    styleDwlButton <- "color: #FFFFFF; width: 180px; background-color: #1976D2; border-color: #1976D2; margin:0 20% 0 20%;"
    action_btn_close <- "color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1"

    df_for_table <- shiny::reactive({
      shiny::req(varsLocal(), varsSuso())

      susoChoices <- stats::setNames(
        c("", paste(varsSuso()$VariableName)),
        c("", paste(varsSuso()$VariableName, varsSuso()$type))
      )
      localChoices <- stats::setNames(
        c("", paste(varsLocal()$VariableName)),
        c("", paste(varsLocal()$VariableName, varsLocal()$type))
      )
      n_rows <- nrow(varsLocal())
      data.frame(
        `Sample Variables` = vapply(seq_len(n_rows), function(i) {
          as.character(shiny::selectInput(
            ns(paste0("select_", i)),
            "",
            choices = localChoices,
            selected = NULL
          ))
        }, character(1)),
        `ID variables` = vapply(seq_len(n_rows), function(i) {
          as.character(shiny::selectInput(
            ns(paste0("select2_", i)),
            "",
            choices = susoChoices,
            selected = NULL
          ))
        }, character(1))
      )
    })

    output$maptable <- DT::renderDT({
      shiny::req(df_for_table())
      DT::datatable(
        df_for_table(),
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Sample Variables", "ID variables"),
        options = list(
          dom = "tp",
          paging = TRUE,
          searching = FALSE,
          pageLength = 10,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = "50%", targets = c(0, 1))
          )
        ),
        selection = "none"
      ) |>
        DT::formatStyle(
          1,
          fontWeight = "bold",
          textAlign = "center",
          color = "#1976D2"
        ) |>
        DT::formatStyle(
          2,
          fontWeight = "bold",
          textAlign = "center",
          color = "#1976D2"
        )
    }, server = FALSE)

    shiny::observeEvent(input$suso_idvars, {
      shiny::req(df_for_table())

      shiny::showModal(shiny::modalDialog(
        title = shiny::tags$div(
          shiny::HTML("<center><font color='#0d47a1'><big>Mapping of Variables</big></font></center>")
        ),
        shiny::fluidRow(
          shiny::column(1),
          shiny::column(
            10,
            shiny::div(
              id = "helpTmodal",
              style = "text-align: center !important; color: #0d47a1; font-weight: bold;",
              shiny::h5(
                "Please select the variables from the available variables in the sample to be mapped to
                 the ID variables from the questionnaire. Make sure that the variable types match, as otherwise
                 the assignment will fail."
              )
            )
          ),
          shiny::column(1)
        ),
        shiny::fluidRow(
          shiny::div(
            style = "text-align: center;",
            DT::DTOutput(ns("maptable"))
          )
        ),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            8,
            shiny::actionButton(ns("submit"), "Submit", style = styleDwlButton)
          ),
          shiny::column(
            4,
            shiny::actionButton(ns("cancel"), "Cancel", style = action_btn_close)
          )
        ),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      ))
    })

    shiny::observeEvent(input$cancel, {
      shiny::removeModal()
    })

    FULLVARMAP <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$submit, {
      shiny::removeModal()
      n_rows <- nrow(varsLocal())
      selected_in_col1 <- lapply(seq_len(n_rows), function(i) input[[paste0("select_", i)]] %||% "")
      selected_in_col2 <- lapply(seq_len(n_rows), function(i) input[[paste0("select2_", i)]] %||% "")

      fullmap <- data.table::data.table(
        locvars = unlist(selected_in_col1),
        idvars = unlist(selected_in_col2)
      )
      fullmap <- fullmap[!is.na(locvars) & !is.na(idvars) & locvars != "" & idvars != ""]
      FULLVARMAP(fullmap)
    })

    output$fullmaptable <- DT::renderDT({
      shiny::req(FULLVARMAP())
      DT::datatable(
        FULLVARMAP(),
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Sample", "ID"),
        options = list(
          dom = "tp",
          paging = TRUE,
          searching = FALSE,
          pageLength = 3,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = "50%", targets = c(0, 1))
          )
        ),
        selection = "none"
      ) |>
        DT::formatStyle(
          1,
          fontWeight = "bold",
          textAlign = "center",
          color = "#1976D2"
        ) |>
        DT::formatStyle(
          2,
          fontWeight = "bold",
          textAlign = "center",
          color = "#1976D2"
        )
    }, server = FALSE)

    return(FULLVARMAP)
  })
}

`%||%` <- function(x, y) if (is.null(x)) y else x
