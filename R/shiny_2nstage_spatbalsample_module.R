#' Shiny module UI for 2nd-stage spatially balanced sampling using spsurvey
#'
#' @description Module UI to create the 2nd-stage sample from listed building
#' units for each cluster. Available sampling modes are:
#' - Spatially balanced sample (`"Balanced"`)
#' - Spatially balanced sample with minimum distance (`"Balanced w. minimum distance"`)
#' - Spatial random sample (`"Random"`)
#'
#' @param id Character string specifying the module namespace ID.
#' @param style CSS style string for action buttons.
#' @param inputMode Character string specifying the data input mode: one of
#'   `"upload"`, `"app"`, or `"hybrid"`.
#'
#' @return A `shiny.tag.list` object containing the module UI elements.
#' @noRd
#' @keywords internal
modal_spsample2stage_ui <- function(id,
                                    style = "color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1",
                                    inputMode = c("upload", "app", "hybrid")) {
  inputMode <- tolower(inputMode[1])
  stopifnot(inputMode %in% c("upload", "app", "hybrid"))
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          4,
          if (inputMode == "upload") {
            zipFileInput_ui(ns("userupload"), "Upload Data", accept = "text/csv")
          } else if (inputMode == "app") {
            shinyjs::disabled(
              shiny::div(
                id = "noupload",
                zipFileInput_ui(ns("userupload"), "Upload Data", accept = "text/csv")
              )
            )
          }
        ),
        shiny::column(
          4,
          shinyjs::disabled(
            shiny::selectInput(
              inputId = ns("clustervar"),
              label = "Select Cluster Variable",
              choices = c("Load Data First!")
            )
          )
        ),
        shiny::column(
          4,
          shiny::numericInput(
            inputId = ns("sampSEED"),
            label = "Set Seed",
            value = floor(stats::runif(1, 1000, 9999)),
            min = 0,
            step = 1
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectInput(
            inputId = ns("samplemode"),
            label = "Select Sampling Mode",
            choices = c(
              "Balanced",
              "Balanced w. minimum distance",
              "Random"
            )
          )
        ),
        shiny::column(
          4,
          shinyjs::disabled(
            shiny::numericInput(
              inputId = ns("mindist"),
              label = "Minimum distance in m",
              step = 1,
              value = 10
            )
          )
        ),
        shiny::column(
          4,
          shiny::numericInput(
            inputId = ns("n_sample"),
            label = "Cluster Sample Size",
            step = 1,
            value = 12
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shinyjs::disabled(
            shiny::actionButton(
              ns("createSample"),
              "Generate Balanced Sample",
              shiny::icon("gears"),
              width = "100%",
              style = style
            )
          )
        ),
        shiny::column(
          6,
          dwl_dataUI(ns("dwlsample"), style = style)
        )
      )
    )
  )
}

#' Shiny module server for 2nd-stage spatially balanced sampling using spsurvey
#'
#' @param id Character string specifying the module namespace ID.
#' @param framepoints_sf Reactive expression returning the frame data table/data frame.
#' @param patternlat Regular expression pattern for the latitude column name.
#' @param patternlong Regular expression pattern for the longitude column name.
#' @param inputMode Character string specifying the data input mode: one of
#'   `"upload"`, `"app"`, or `"hybrid"`.
#'
#' @return A reactive value (`reactiveVal`) holding the sampled `sf` object in
#'   EPSG:4326.
#' @noRd
#' @keywords internal
modal_spsample2stage_server <- function(id,
                                        framepoints_sf = shiny::reactive({ NULL }),
                                        patternlat = "lat",
                                        patternlong = "long",
                                        inputMode = c("upload", "app", "hybrid")) {
  shiny::moduleServer(id, function(input, output, session) {
    inputMode <- tolower(inputMode[1])
    stopifnot(inputMode %in% c("upload", "app", "hybrid"))

    if (inputMode == "upload") {
      framedata <- zipFileInput_server("userupload")
    } else if (inputMode == "app") {
      framedata <- framepoints_sf
    }

    # Update cluster variable selection
    shiny::observe({
      shiny::req(framedata())
      shinyjs::enable("clustervar")
      cluvar <- names(framedata())
      cluvar <- cluvar[!(grepl(
        x = cluvar,
        pattern = sprintf("(%s)|(%s)", patternlat, patternlong),
        ignore.case = TRUE
      ))]
      cluvar <- stats::setNames(cluvar, cluvar)
      shiny::updateSelectInput(
        session = session,
        inputId = "clustervar",
        label = "Select Cluster Variable",
        choices = cluvar
      )
    })

    # Enable minimum distance field when mode requires it
    shiny::observeEvent(input$samplemode, {
      if (input$samplemode == "Balanced w. minimum distance") {
        shinyjs::enable("mindist")
      } else {
        shinyjs::disable("mindist")
      }
    })

    # Enable sampling button once cluster variable is chosen
    shiny::observe({
      shiny::req(input$clustervar != "Load Data First!")
      shinyjs::enable("createSample")
    })

    # Transform frame data to spatial & project to UTM
    framedatasf <- shiny::reactive({
      shiny::req(framedata())
      frdat <- framedata()
      shiny::showNotification("Transforming data to spatial.")

      lat <- names(frdat)[grepl(x = names(frdat), pattern = patternlat, ignore.case = TRUE)][1]
      long <- names(frdat)[grepl(x = names(frdat), pattern = patternlong, ignore.case = TRUE)][1]

      frdat <- frdat %>%
        dplyr::filter(!is.na(.data[[lat]]) & !is.na(.data[[long]])) %>%
        dplyr::group_by(.data[[input$clustervar]]) %>%
        dplyr::mutate(CLUSTERCOUNT = dplyr::n()) %>%
        as.data.frame()

      frdat <- sf::st_as_sf(frdat, coords = c(long, lat), crs = 4326)
      frdat <- project_to_utm(frdat)
      return(frdat)
    })

    # Take sample
    mapsampledata <- shiny::reactiveVal(NULL)
    dwlsampledata <- shiny::reactiveVal(NULL)
    sample_seed <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$createSample, {
      shiny::req(framedatasf(), input$sampSEED, input$clustervar)
      frdatsf <- framedatasf()
      shiny::showNotification("Taking the sample.")
      sample_seed(input$sampSEED)
      ss <- input$n_sample

      # Compute sample size per stratum (capped at cluster population size)
      cluster_ids <- as.character(frdatsf[[input$clustervar]])
      cluster_counts <- table(cluster_ids)
      strata_n <- pmin(as.integer(ss), as.integer(cluster_counts))
      names(strata_n) <- names(cluster_counts)

      if (input$samplemode == "Balanced") {
        set.seed(input$sampSEED)
        samp <- spsurvey::grts(
          frdatsf,
          n_base = strata_n,
          stratum_var = input$clustervar
        )
      } else if (input$samplemode == "Balanced w. minimum distance") {
        set.seed(input$sampSEED)
        shiny::req(input$mindist)
        samp <- spsurvey::grts(
          frdatsf,
          mindis = input$mindist,
          n_base = strata_n,
          stratum_var = input$clustervar
        )
      } else if (input$samplemode == "Random") {
        set.seed(input$sampSEED)
        samp <- spsurvey::irs(
          frdatsf,
          n_base = strata_n,
          stratum_var = input$clustervar
        )
      }

      tabsf <- sf::st_transform(samp$sites_base, 4326)
      mapsampledata(tabsf)
      tab <- as.data.frame(sf::st_set_geometry(tabsf, NULL))
      dwlsampledata(tab)
    })

    # Download sample as zipped CSV
    download_csv_server(
      "dwlsample",
      content = dwlsampledata,
      file_name = shiny::reactive(
        paste0(
          "SpatSamp_2stage_",
          input$samplemode,
          ifelse(
            input$samplemode == "Balanced w. minimum distance",
            paste0("_mindist_", input$mindist, "m"),
            ""
          ),
          "_seed_",
          input$sampSEED
        )
      )
    )

    return(mapsampledata)
  })
}
