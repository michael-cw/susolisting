#' Shiny module main-panel UI for admin sampling
#'
#' @param id Character string specifying the module namespace ID.
#'
#' @return A `shiny.tag.list` object containing the main-panel admin UI.
#' @noRd
#' @keywords internal
modal_adminspsample_ui_tabmain <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    modal_spsample2stage_ui(ns("sample"), inputMode = "app")
  )
}

#' Shiny module sidebar UI for admin frame generation and summary tables
#'
#' @param id Character string specifying the module namespace ID.
#' @param label Character string for the frame generation button label.
#' @param style CSS style string for the action button.
#'
#' @return A `shiny.tag.list` object containing the sidebar admin UI.
#' @noRd
#' @keywords internal
modal_adminspsample_ui_tabside <- function(id,
                                           label = "Generate Frame",
                                           style = "color: #FFFFFF; background-color: #1976D2;border-color: #0d47a1") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinyjs::disabled(
        shiny::column(1),
        shiny::column(
          10,
          shiny::actionButton(
            ns("generateFrame"),
            label = label,
            shiny::icon("download"),
            width = "15vw",
            style = style
          )
        ),
        shiny::column(1)
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(2),
      shiny::column(
        8,
        shinyjs::hidden(
          shiny::div(
            id = ns("headframe"),
            shiny::h4(
              "Frame Summary",
              style = "color: #0d47a1;text-align: center;"
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
        DT::DTOutput(ns("framesummary"), width = "15vw")
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
            id = ns("headsample"),
            shiny::h4(
              "Sample Summary",
              style = "color: #0d47a1;text-align: center;"
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
        DT::DTOutput(ns("samplesummary"), width = "15vw")
      ),
      shiny::column(1)
    )
  )
}

#' Shiny module server for admin authentication, frame compilation, and 2nd-stage sampling
#'
#' @param id Character string specifying the module namespace ID.
#' @param side Reactive expression returning the currently active sidebar tab.
#'
#' @return A list with two reactive elements: `admcheckok` (logical status of
#'   admin verification) and `sampleData` (a `data.table` of the 2nd-stage sample).
#' @noRd
#' @keywords internal
modal_adminspsample_server <- function(id, side = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    admcheckResult <- shiny::reactiveVal(FALSE)

    # Prompt for admin credentials when Sample or SuSo tab is selected
    shiny::observeEvent(side(), {
      ns <- session$ns
      if (!admcheckResult() && (identical(side(), "Sample") || identical(side(), "SuSo"))) {
        shinyalert::shinyalert(
          inputId = "admincheck",
          title = "Sample Generation & Assignment creation",
          text = shiny::tagList(
            shiny::h4(
              "Please make sure you have read the relevant documentation, available under: https://datanalytics.worldbank.org/SpatialSamplingManual/
               To continue please provide your credentials. In this section you will be able to compile the frame from all collaborators, and
               create the 2nd stage sample, which can either be downloaded or directly uploaded to your Survey Solutions Server."
            ),
            shiny::textInput(
              inputId = ns("admuser"),
              label = "",
              placeholder = "Admin User Name"
            ),
            shiny::br(),
            shiny::passwordInput(
              inputId = ns("admpass"),
              label = "",
              placeholder = "Admin Password"
            )
          ),
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = TRUE,
          type = "success",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "www/logoWBDG.png",
          animation = TRUE
        )
      } else if (admcheckResult()) {
        admcheckResult(TRUE)
        shinyjs::enable("generateFrame")
      } else {
        admcheckResult(FALSE)
        shinyjs::disable("generateFrame")
      }
    }, ignoreInit = TRUE)

    # Verify admin credentials
    shiny::observeEvent(input$admincheck, {
      if (is.null(getOption("admuser")) || is.null(getOption("admpass"))) {
        shiny::showNotification(
          "No admin credentials have been provided at start-up! Please set admin credentials first, to use the sampling and assignment module.",
          type = "error",
          id = "noadm"
        )
        shiny::req(FALSE)
      } else {
        shiny::req(input$admuser, input$admpass)
        if (input$admuser == getOption("admuser") && input$admpass == getOption("admpass")) {
          admcheckResult(TRUE)
          shiny::showNotification("Success!", type = "message", id = "admsuccess")
          shinyjs::enable("generateFrame")
        } else {
          admcheckResult(FALSE)
          shinyjs::disable("generateFrame")
        }
      }
    })

    # Scan user directories to build frame file list
    framefiles <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$generateFrame, {
      appdir <- get_app_download_dir()
      ndirs <- list.dirs(appdir, recursive = FALSE, full.names = TRUE)
      if (length(ndirs) == 0) {
        shiny::showNotification("No Listing data available!", type = "error", id = "nolisting")
      } else {
        allfiles <- list()
        for (di in ndirs) {
          dilist <- data.table::data.table(
            listfiles = list.files(di, pattern = "\\.fst$", full.names = TRUE)
          )
          if (nrow(dilist) == 0) next
          allfiles[[basename(di)]] <- dilist
        }
        allfiles <- data.table::rbindlist(allfiles, idcol = "dir")
        shiny::req(nrow(allfiles) > 0)

        framefiles(allfiles)
        shinyalert::shinyalert(
          inputId = "framecreatconfirm",
          title = "Generate the sampling frame",
          text = shiny::tagList(
            shiny::h4(shiny::HTML(sprintf(
              'This will create the 2nd stage sampling frame for further processing. There are currently
               <p style="color:red; font-weight: bold;">%d user directories and %d files </p> to process.
               Do you want to continue?',
              length(ndirs), nrow(allfiles)
            )))
          ),
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = TRUE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Generate the frame",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "www/logoWBDG.png",
          animation = TRUE
        )
      }
    })

    # Compile frame from .fst files
    dt_frame <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$framecreatconfirm, {
      ac <- admcheckResult()
      if (isTRUE(ac) && isTRUE(input$framecreatconfirm)) {
        fnall <- shiny::req(framefiles())
        points_dt <- list()
        for (i in seq_len(nrow(fnall))) {
          fn <- fnall$listfiles[i]
          if (file.exists(fn)) {
            eaname <- basename(tools::file_path_sans_ext(fn))
            points_dt[[eaname]] <- fst::read_fst(fn, as.data.table = TRUE)
          }
        }
        points_dt <- data.table::rbindlist(points_dt, idcol = "ea")
        data.table::setorderv(points_dt, c("ea", "long", "lat"))
        dt_frame(points_dt)
      }
    })

    # Frame summary table
    output$framesummary <- DT::renderDT({
      dtframe <- shiny::req(dt_frame())
      dtframegr <- dtframe[, .(ean = .N), by = .(ea)]
      tab <- data.table::data.table(
        var = c(
          "Number of Segments",
          "Total Frame Population",
          "Average population per Segment",
          "Minimum Population per Segment",
          "Maximum Population per Segment"
        ),
        val = round(c(
          dplyr::n_distinct(dtframe$ea),
          nrow(dtframe),
          mean(dtframegr$ean),
          min(dtframegr$ean),
          max(dtframegr$ean)
        ))
      )

      tab <- DT::datatable(
        tab,
        list(dom = "t"),
        selection = "none",
        rownames = FALSE,
        colnames = c("", ""),
        style = "bootstrap"
      )
      shinyjs::show("headframe")
      return(tab)
    })

    # Invoke 2nd-stage sampling module (returns sf object)
    sampleDataSF <- modal_spsample2stage_server(
      "sample",
      inputMode = "app",
      framepoints_sf = dt_frame,
      patternlat = "lat",
      patternlong = "long"
    )

    sampleData <- shiny::eventReactive(sampleDataSF(), {
      tabsf <- shiny::req(sampleDataSF())
      tab <- as.data.frame(sf::st_set_geometry(tabsf, NULL))
      data.table::as.data.table(tab)
    })

    # Sample summary table
    output$samplesummary <- DT::renderDT({
      dtframe <- shiny::req(sampleData())
      tab <- data.table::data.table(
        var = c(
          "Number of Segments",
          "Total Sample Population",
          "Average Sampling Weight",
          "Minimum Sampling Weight",
          "Maximum Sampling Weight",
          "Sampling Weight CV"
        ),
        val = round(c(
          dplyr::n_distinct(dtframe$ea),
          nrow(dtframe),
          mean(dtframe$wgt),
          min(dtframe$wgt),
          max(dtframe$wgt),
          stats::sd(dtframe$wgt) / mean(dtframe$wgt)
        ), 3)
      )

      tab <- DT::datatable(
        tab,
        list(dom = "t"),
        selection = "none",
        rownames = FALSE,
        colnames = c("", ""),
        style = "bootstrap"
      )
      shinyjs::show("headsample")
      return(tab)
    })

    return(
      list(
        admcheckok = admcheckResult,
        sampleData = sampleData
      )
    )
  })
}
