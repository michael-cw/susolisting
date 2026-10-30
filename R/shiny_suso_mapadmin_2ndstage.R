#' Shiny UI module for Survey Solutions Assignment Admin
#'
#' @description Provides the main panel UI for connecting to a Survey Solutions
#' server, mapping sample variables to questionnaire ID variables, and assigning
#' sampled building units to supervisor teams.
#'
#' @param id Character string specifying the module namespace ID.
#'
#' @return A `shiny.tag.list` object containing the main-panel UI elements.
#' @noRd
#' @keywords internal
#' @importFrom shiny NS tagList
mapadminUI_st2 <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    shiny::fluidRow(
      shinydashboard::box(
        width = 6,
        status = "success",
        height = "35vh",
        solidHeader = TRUE,
        background = NULL,
        title = "Survey Solutions API Settings",
        shiny::div(
          id = ns("susosettings"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                ns("susoServer"),
                "Survey Solutions Server",
                placeholder = "Provide Server"
              )
            ),
            shiny::column(
              6,
              shiny::textInput(
                ns("susoWS"),
                "Workspace",
                placeholder = "Provide Workspace"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                ns("susoUser"),
                "API user",
                placeholder = "Provide User"
              )
            ),
            shiny::column(
              6,
              shiny::passwordInput(
                ns("susoPass"),
                "API password",
                placeholder = "Provide Pass"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(2),
            shiny::column(
              8,
              shiny::actionButton(
                ns("checkSuso"),
                "Check Server Settings",
                shiny::icon("check"),
                width = "100%",
                style = "color: #fff; background-color: #337ab7; border-color: #337ab7; margin: 0;"
              )
            ),
            shiny::column(2)
          )
        ),
        shiny::br(),
        shiny::div(
          id = "serversetreset",
          shiny::fluidRow(
            shiny::column(1),
            shiny::column(
              10,
              DT::DTOutput(ns("apiSummary"), height = "10vh")
            ),
            shiny::column(1)
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(2),
            shiny::column(
              8,
              shinyjs::hidden(
                shiny::actionButton(
                  ns("resetSuso"),
                  "Reset Server Settings",
                  shiny::icon("ban"),
                  width = "100%",
                  style = "color: #FFFFFF; background-color: #7f0000; border-color: #7f0000"
                )
              )
            ),
            shiny::column(2)
          )
        )
      ),
      shinydashboard::box(
        width = 6,
        status = "success",
        height = "35vh",
        solidHeader = TRUE,
        background = NULL,
        title = "ID variable selection",
        shiny::h4(
          "Select variables for preloading",
          align = "center",
          style = "font-weight:bold; color:#0d47a1; margin:0;"
        ),
        shiny::helpText(
          "All selected variables must be present in the dataset.",
          style = "text-align: center;"
        ),
        shiny::fluidRow(
          shiny::column(3),
          shiny::column(
            6,
            suso_map_idUI(ns("dtModule"))
          ),
          shiny::column(3)
        ),
        shiny::br(),
        shiny::h4(
          "Survey Solutions Upload File Mapping",
          align = "center",
          style = "font-weight:bold; color:#0d47a1; margin:0;"
        ),
        shiny::helpText(
          "This is the mapping of the variables in the sample to the identifier variables in your questionnaire, based on your selection.",
          style = "text-align: center;"
        ),
        shiny::fluidRow(
          shiny::column(2),
          shiny::column(
            8,
            suso_map_idUI_maptable(ns("dtModule"), height = "5vh")
          ),
          shiny::column(2)
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        status = "success",
        solidHeader = TRUE,
        background = NULL,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Team Assignment",
        shiny::fluidRow(
          shiny::column(4),
          shiny::column(4),
          shiny::column(4)
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            DT::DTOutput(ns("assignTable"), height = "28vh")
          ),
          shiny::column(
            4,
            DT::DTOutput(ns("teamTable"), height = "28vh")
          ),
          shiny::column(
            2,
            shiny::actionButton(
              ns("suso_assignPoints"),
              "Create Assignments",
              width = "100%",
              shiny::icon("upload"),
              style = "color: #FFFFFF; background-color: #1976D2; border-color: #1976D2"
            )
          )
        )
      )
    )
  )
}

#' Shiny sidebar UI module for Survey Solutions Map Admin
#'
#' @description Sidebar UI for displaying available questionnaires and the
#' assignment overview log.
#'
#' @param id Character string specifying the module namespace ID.
#'
#' @return A `shiny.tag.list` object containing the sidebar UI elements.
#' @noRd
#' @keywords internal
#' @importFrom shiny NS tagList
mapadminUI2_st2 <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(2),
      shiny::column(
        8,
        shinyjs::hidden(
          shiny::div(
            id = ns("headquest"),
            shiny::h4(
              "Available Questionnaire",
              style = "color: #0d47a1; text-align: center !important;"
            )
          )
        )
      ),
      shiny::column(2)
    ),
    shiny::fluidRow(
      shiny::column(1),
      shiny::column(
        10,
        DT::DTOutput(ns("qTableIn"), width = "10vw")
      ),
      shiny::column(1)
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(2),
      shiny::column(
        8,
        shinyjs::hidden(
          shiny::div(
            id = ns("headass"),
            shiny::h4(
              "Assignment Overview",
              style = "color: #0d47a1; text-align: center !important;"
            )
          )
        )
      ),
      shiny::column(2)
    ),
    shiny::fluidRow(
      shiny::column(1),
      shiny::column(
        10,
        DT::DTOutput(ns("assignoverviewTable"), width = "23vw")
      ),
      shiny::column(1)
    )
  )
}

#' Shiny server module for Survey Solutions Map Admin
#'
#' @param id Character string specifying the module namespace ID.
#' @param pointsfile Reactive expression returning the sampled points `data.table`.
#'
#' @return A list of reactives: `apiCheck`, `susoServer`, and `susoWS`.
#' @importFrom shiny NS tagList
#' @noRd
#' @keywords internal
mapadminSRV_st2 <- function(id, pointsfile = shiny::reactive({ NULL })) {
  shiny::moduleServer(id, function(input, output, session) {
    smTab <- list(dom = "t")

    infoTable <- function(dt) {
      DT::formatStyle(
        dt,
        1,
        color = "#FFFFFF",
        backgroundColor = "#0d47a1",
        fontWeight = "bold"
      )
    }

    styleActButton <- "color: #FFFFFF; background-color: #7f0000; border-color: #7f0000; margin:0 20% 0 20%;"

    apiCheck <- shiny::reactiveVal(NULL)
    susoServer <- shiny::reactiveVal(NULL)
    susoWS <- shiny::reactiveVal(NULL)

    # Check Survey Solutions API Credentials
    shiny::observeEvent(input$checkSuso, {
      SurveySolutionsAPIv2::suso_set_key(
        suso_server = input$susoServer,
        suso_user = input$susoUser,
        suso_password = input$susoPass,
        workspace = input$susoWS
      )
      checkAPI <- SurveySolutionsAPIv2::suso_PwCheck(workspace = input$susoWS)
      status_code <- if (is.list(checkAPI) && !is.null(checkAPI$status_code)) {
        checkAPI$status_code
      } else {
        checkAPI
      }

      if (identical(as.integer(status_code), 200L)) {
        apiCheck(200L)
        susoServer(input$susoServer)
        susoWS(input$susoWS)
        shinyjs::disable("susosettings")
        shinyjs::show("resetSuso")
      } else {
        shiny::showNotification(
          "Survey Solutions Credentials not valid! Please correct.",
          type = "warning"
        )
      }
    }, ignoreInit = TRUE)

    # Enable variable mapping button when API is valid and sample points are ready
    shiny::observe({
      shiny::validate(shiny::need(apiCheck() == 200, message = FALSE))
      shiny::req(pointsfile())
      shinyjs::enable("suso_idvars")
    })

    # Reset API connection
    shiny::observeEvent(input$resetSuso, {
      shiny::validate(shiny::need(apiCheck() == 200, message = FALSE))
      shinyjs::enable("susosettings")
      shinyjs::disable("suso_idvars")
      shiny::updateTextInput(
        session = session,
        inputId = "susoServer",
        label = "Survey Solutions Server",
        placeholder = "Provide Server",
        value = ""
      )
      shiny::updateTextInput(
        session = session,
        inputId = "susoWS",
        label = "Workspace",
        placeholder = "Provide Workspace",
        value = ""
      )
      shiny::updateTextInput(
        session = session,
        inputId = "susoUser",
        label = "API user",
        placeholder = "Provide User",
        value = ""
      )
      shiny::updateTextInput(
        session = session,
        inputId = "susoPass",
        label = "API password",
        placeholder = "Provide Pass",
        value = ""
      )
      shinyjs::hide("resetSuso")
      apiCheck(NULL)
    }, ignoreInit = TRUE)

    # Table API Check Summary
    output$apiSummary <- DT::renderDT({
      shiny::validate(shiny::need(apiCheck() == 200, message = FALSE))
      tab <- cbind(
        c("API Credentials", "Server", "Workspace"),
        c("Valid", susoServer(), susoWS())
      )
      DT::datatable(
        tab,
        smTab,
        selection = "none",
        rownames = FALSE,
        colnames = c("", ""),
        style = "bootstrap"
      ) %>% infoTable()
    })

    # Questionnaire list
    qData <- shiny::reactive({
      shiny::validate(shiny::need(apiCheck() == 200, message = FALSE))
      tab <- SurveySolutionsAPIv2::suso_getQuestDetails(workspace = input$susoWS)

      if (is.null(tab) || !is.data.frame(tab) || nrow(tab) == 0) {
        shiny::showNotification(
          "No questionnaire loaded on the provided server/workspace. Please import questionnaires first!",
          type = "error"
        )
        shiny::req(FALSE)
      }

      tab <- data.table::as.data.table(tab)
      data.table::setkeyv(tab, c("Title", "Version"))

      if ("LastEntryDate" %in% names(tab) && is.character(tab$LastEntryDate)) {
        tab[, c("date", "time") := data.table::tstrsplit(LastEntryDate, "T", fixed = TRUE)]
        tab[, time := data.table::as.ITime(time)]
        tab[, date := data.table::as.IDate(date)]
        data.table::setorderv(tab, c("date", "time"), -1)
      } else {
        data.table::setorderv(tab, c("Title", "Version"), c(1, -1))
      }
      tab[, QuestionnaireIdVersion := sprintf("%s_%d", QuestionnaireId, Version)]
      return(tab[])
    })

    questIdIn <- shiny::reactiveVal(NULL)
    output$qTableIn <- DT::renderDT({
      shiny::req(qData())
      tab <- qData()
      questIdIn(tab[, .(QuestionnaireId, Version, Title)])
      tab <- DT::datatable(
        tab[, .(Title, Version)],
        list(dom = "tp"),
        selection = "single",
        rownames = FALSE,
        style = "bootstrap"
      )
      shinyjs::show("headquest")
      return(tab)
    })

    IDVARS <- shiny::reactiveVal(NULL)
    LOCVARS <- shiny::reactiveVal(NULL)
    GPSVAR <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$qTableIn_rows_selected, {
      sel <- input$qTableIn_rows_selected
      waiter::waiter_show(
        color = "rgba(13, 71, 161, 0.7)",
        html = shiny::tagList(
          waiter::spin_fading_circles(),
          "Loading Questionnaire ..."
        )
      )
      on.exit(waiter::waiter_hide(), add = TRUE)

      iddata <- shiny::req(qData())
      qid <- iddata[sel, QuestionnaireId]
      v <- iddata[sel, Version]

      qestStruct <- SurveySolutionsAPIv2::suso_getQuestDetails(
        workspace = input$susoWS,
        questID = qid,
        version = v,
        operation.type = "structure"
      )

      idvars <- qestStruct$q[Featured == TRUE & type != "GpsCoordinateQuestion", .(VariableName, type)]
      gpsidvar <- qestStruct$q[Featured == TRUE & type == "GpsCoordinateQuestion", VariableName]
      shinyjs::enable("suso_idvars")
      IDVARS(idvars)

      pts <- shiny::req(pointsfile())
      locvars <- data.table::data.table(
        VariableName = names(pts),
        type = vapply(pts, function(col) class(col)[1], character(1))
      )
      locvars <- locvars[!(VariableName %in% c("lon_WGS84", "lat_WGS84"))]
      LOCVARS(locvars)
      GPSVAR(gpsidvar)
    }, ignoreInit = TRUE)

    # Query supervisor teams
    teamtablequery <- shiny::reactive({
      shiny::validate(shiny::need(apiCheck() == 200, message = FALSE))
      SurveySolutionsAPIv2::suso_getSV(workspace = input$susoWS)
    })

    # Sample units table for assignment
    output$assignTable <- DT::renderDT({
      tab <- shiny::req(pointsfile())
      if (!data.table::is.data.table(tab)) {
        tab <- data.table::as.data.table(tab)
      }
      tab <- tab[, .(ea, area_id, siteID)]
      DT::datatable(
        tab,
        escape = FALSE,
        rownames = FALSE,
        selection = "multiple",
        options = list(
          pageLength = 500,
          scrollY = "500px",
          scrollX = "300px",
          scrollCollapse = TRUE,
          paging = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = c(0:2)),
            list(width = "20%", targets = c(0)),
            list(width = "10%", targets = c(1)),
            list(width = "5%", targets = c(2))
          )
        )
      )
    })

    assingTabProxy <- DT::dataTableProxy(
      "assignTable",
      session = shiny::getDefaultReactiveDomain(),
      deferUntilFlush = TRUE
    )

    # Supervisor teams table
    output$teamTable <- DT::renderDT({
      tab <- shiny::req(teamtablequery())
      shiny::validate(shiny::need(nrow(tab) > 0, message = "No supervisor created!"))
      tab <- tab[, .(UserName, IsLocked)]
      DT::datatable(
        tab,
        escape = FALSE,
        rownames = FALSE,
        selection = "single",
        options = list(
          pageLength = 500,
          scrollY = "500px",
          scrollCollapse = TRUE,
          paging = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = c(0:1)),
            list(width = "20%", targets = c(0)),
            list(width = "10%", targets = c(1))
          )
        )
      )
    })

    shiny::observeEvent(input$select_button, {
      shinyalert::shinyalert(
        "Attention!",
        paste0(
          "Are you sure you want to reset the status of this Interview?\nID: ",
          input$select_button,
          "\nIf yes, please re-type the ID to confirm."
        ),
        type = "input",
        inputId = "confirmReset",
        inputType = "text",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        showCancelButton = TRUE,
        showConfirmButton = TRUE
      )
    })

    # Map assignment & log
    toassingoverview <- shiny::reactiveVal(
      data.table::data.table(Team = character(0), AssignmentCount = integer(0))
    )

    shiny::observeEvent(input$suso_assignPoints, {
      tabMAP <- pointsfile()
      tabSV <- teamtablequery()
      fullmap <- FULLMAP()
      shiny::req(tabMAP, tabSV, fullmap)
      shiny::validate(shiny::need(nrow(tabMAP) > 0 && nrow(tabSV) > 0, message = FALSE))

      mapsel <- tabMAP[input$assignTable_rows_selected]
      svsel <- tabSV[input$teamTable_rows_selected]
      shiny::validate(shiny::need(nrow(mapsel) > 0 && nrow(svsel) > 0, message = FALSE))

      mapsvselall <- data.table::copy(toassingoverview())
      if ("Reset" %in% names(mapsvselall)) {
        mapsvselall[, Reset := NULL]
      }
      mapsvsel <- data.table::data.table(
        Team = svsel$UserName,
        AssignmentCount = nrow(mapsel)
      )
      mapsvselall <- data.table::rbindlist(list(mapsvselall, mapsvsel), fill = TRUE)
      toassingoverview(data.table::copy(mapsvselall))
      DT::selectRows(assingTabProxy, selected = NULL)

      # Assignment creation
      mapsel <- mapsel[, c(fullmap$locvars, "lat_WGS84", "lon_WGS84"), with = FALSE]
      AAout <- data.table::data.table(
        mapsel,
        ResponsibleName = svsel$UserName,
        Quantity = 1,
        gpsstring = paste0(round(mapsel$lat_WGS84, 8), "$", round(mapsel$lon_WGS84, 8))
      )
      gpsvar <- shiny::req(GPSVAR())
      for (i in fullmap$locvars) {
        idn <- fullmap[locvars == i, idvars]
        data.table::setnames(AAout, i, idn)
      }
      data.table::setnames(AAout, "gpsstring", gpsvar)
      AAout[, c("lon_WGS84", "lat_WGS84") := NULL]

      if (nrow(AAout) > 0) {
        iddata <- shiny::req(qData())
        sel <- input$qTableIn_rows_selected
        qid <- iddata[sel, QuestionnaireId]
        v <- iddata[sel, Version]
        status_list <- vector("list", nrow(AAout))
        for (i in seq_len(nrow(AAout))) {
          AAsing <- AAout[i]
          status_list[[i]] <- SurveySolutionsAPIv2::suso_createASS(
            df = AAsing,
            workspace = input$susoWS,
            questID = qid,
            version = v
          )
        }
      }
    }, ignoreInit = TRUE)

    # Assignment overview table
    output$assignoverviewTable <- DT::renderDT({
      shiny::validate(shiny::need(apiCheck() == 200, message = FALSE))
      tab <- data.table::copy(toassingoverview())
      shiny::req(nrow(tab) > 0)

      tab[, Reset := shinyInput(
        shiny::actionButton,
        nrow(tab),
        tab$Team,
        label = "Reset",
        style = styleActButton,
        onclick = paste0('Shiny.setInputValue(\"', session$ns("reset_button"), '\", this.id, {priority: \"event\"})')
      )]

      tab <- DT::datatable(
        tab,
        escape = FALSE,
        rownames = FALSE,
        selection = "single",
        options = list(
          pageLength = 500,
          scrollY = "500px",
          scrollCollapse = TRUE,
          paging = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = c(0:2)),
            list(width = "20%", targets = c(0)),
            list(width = "10%", targets = c(1)),
            list(width = "5%", targets = c(2))
          )
        )
      )
      shinyjs::show("headass")
      return(tab)
    })

    # Reset Map assignment
    shiny::observeEvent(input$reset_button, {
      ns <- session$ns
      shinyalert::shinyalert(
        type = "error",
        title = "Attention!",
        html = TRUE,
        text = shiny::tagList(
          shiny::HTML(
            paste0(
              "Are you sure you want to reset the team assignment for\nID:
               <br><center><font color='#0d47a1'><big>",
              input$reset_button,
              "</big></font></center><br>\nIf yes, please re-type the ID to confirm."
            )
          ),
          shiny::textInput(ns("confirmResetId"), "")
        ),
        inputId = "confirmReset",
        inputType = "text",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        showCancelButton = TRUE,
        showConfirmButton = TRUE,
        animation = "slide-from-top"
      )
    })

    shiny::observeEvent(input$confirmReset, {
      mapreset <- shiny::req(input$confirmResetId)
      mapsvselall <- toassingoverview()
      mapsvselall <- mapsvselall[Team != mapreset, ]
      toassingoverview(mapsvselall)
    }, ignoreInit = TRUE)

    # Variable Mapping Module
    FULLMAP <- suso_map_idSRV("dtModule", varsLocal = LOCVARS, varsSuso = IDVARS)

    list(
      apiCheck = apiCheck,
      susoServer = susoServer,
      susoWS = susoWS
    )
  })
}

#' Generate a vector of HTML Shiny inputs for embedding in a DataTable
#'
#' @param FUN Shiny input function (e.g., [shiny::actionButton]).
#' @param len Integer number of inputs to generate.
#' @param id Character vector of input IDs (length `len`).
#' @param ... Additional arguments passed to `FUN`.
#'
#' @return A character vector of serialized HTML strings.
#' @noRd
#' @keywords internal
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id[i]), ...))
  }
  inputs
}
