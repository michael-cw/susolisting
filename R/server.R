#' Server function for the Survey Solutions Listing Application
#'
#' @description Main Shiny server function managing user login, boundary
#' shapefile upload, Google Maps satellite rendering, building marker
#' collection, local `.fst` persistence, archive download, and integration
#' with the second-stage sampling and Survey Solutions assignment modules.
#'
#' @param input,output,session Internal Shiny server parameters.
#'
#' @return Invisible `NULL` (used for its Shiny server side-effects).
#' @noRd
#' @keywords internal
main_server <- function(input, output, session) {
  provider <- resolve_map_provider()

  # Disable INDIVIDUAL download when admuser/admpass options are set
  shiny::observe({
    if (!is.null(getOption("admuser")) && !is.null(getOption("admpass"))) {
      shinyjs::disable("downloadActivate")
    }
  })

  ## Start-up Modal
  shiny::observe({
    if (identical(provider, "vantor")) {
      shinyalert::shinyalert(
        inputId = "startup",
        title = "Welcome to the Survey Solutions Listing Application!",
        text = shiny::tagList(
          shiny::h4(
            "Please make sure you have read the relevant documentation, available under: https://datanalytics.worldbank.org/SpatialSamplingManual/
             To continue please provide your Vantor / Maxar API credentials and your username."
          ),
          shiny::passwordInput(
            inputId = "mapkey",
            label = "",
            placeholder = "Vantor / Maxar API Key (or set VANTOR_API_KEY)"
          ),
          shiny::selectInput(
            inputId = "vantor_product",
            label = "Vivid Mosaic Product",
            choices = c(
              "Vivid Standard (30 cm)" = "VIVID_STANDARD_30",
              "Vivid Advanced (15 cm)" = "VIVID_ADVANCED_15"
            ),
            selected = "VIVID_STANDARD_30",
            width = "100%"
          ),
          shiny::textInput(
            inputId = "vantor_username",
            label = "",
            placeholder = "Vantor OAuth2 Username / Email (optional)"
          ),
          shiny::passwordInput(
            inputId = "vantor_password",
            label = "",
            placeholder = "Vantor OAuth2 Password (optional)"
          ),
          shiny::br(),
          shiny::textInput(
            inputId = "user",
            label = "",
            placeholder = "User Name"
          )
        ),
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    } else {
      shinyalert::shinyalert(
        inputId = "startup",
        title = "Welcome to the Survey Solutions Listing Application!",
        text = shiny::tagList(
          shiny::h4(
            "Please make sure you have read the relevant documentation, available under: https://datanalytics.worldbank.org/SpatialSamplingManual/
             To continue please provide your Google API key and your username."
          ),
          shiny::passwordInput(
            inputId = "mapkey",
            label = "",
            placeholder = "Google API key"
          ),
          shiny::br(),
          shiny::textInput(
            inputId = "user",
            label = "",
            placeholder = "User Name"
          )
        ),
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })

  mapkey <- shiny::reactiveVal(NULL)
  vantorProduct <- shiny::reactiveVal("VIVID_STANDARD_30")
  user <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$startup, {
    resolved_key <- if (!is.null(input$mapkey)) trimws(input$mapkey) else ""
    if (identical(provider, "vantor") && !nzchar(resolved_key)) {
      resolved_key <- Sys.getenv("VANTOR_API_KEY", unset = Sys.getenv("MAXAR_API_KEY", unset = ""))
    }

    if (!nzchar(resolved_key)) {
      key_title <- if (identical(provider, "vantor")) "Vantor API key missing!" else "Google API key missing!"
      key_msg <- if (identical(provider, "vantor")) {
        "You have not provided a Vantor / Maxar API key. Without an API key, it won't be possible to stream satellite imagery in Leaflet. Please refresh the page and provide the correct key."
      } else {
        "You have not provided a key. Without a Google API key, it won't be possible to continue. Please refresh the page and provide the correct key."
      }
      shinyalert::shinyalert(
        key_title,
        key_msg,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      shiny::req(FALSE)
    } else if (is.null(input$user) || input$user == "") {
      shinyalert::shinyalert(
        "User name missing!",
        "You have not provided a user name. Since this is required to store the files in your personal directory, please refresh the page and provide a user name.",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      shiny::req(FALSE)
    } else {
      if (identical(provider, "vantor")) {
        v_user <- if (!is.null(input$vantor_username)) trimws(input$vantor_username) else ""
        v_pass <- if (!is.null(input$vantor_password)) input$vantor_password else ""
        v_prod <- if (!is.null(input$vantor_product) && nzchar(input$vantor_product)) {
          input$vantor_product
        } else {
          "VIVID_STANDARD_30"
        }
        vantorProduct(v_prod)
        vantorr::vantor_auth(
          api_key = resolved_key,
          username = if (nzchar(v_user)) v_user else NULL,
          password = if (nzchar(v_pass)) v_pass else NULL,
          verbose = FALSE
        )
      }
      mapkey(resolved_key)

      # Sanitize user name & show notification with path
      usr <- sanitize_string(input$user)
      provider_label <- if (identical(provider, "vantor")) "Vantor API credentials" else "Google API key"

      appdir <- get_app_download_dir()
      raw_msg <- sprintf(
        "You are successfully logged in and have provided the required %s. Your files for this session will be stored
         in your personal user directory under, <b>%s/%s_[upload file name]</b>. After uploading the file, you can see the complete file
         path.",
        provider_label, appdir, usr
      )
      notmessage <- shiny::HTML(trimws(gsub("[[:space:]]+", " ", gsub("\n", "", raw_msg))))

      shiny::showNotification(
        ui = notmessage,
        duration = NULL,
        id = "userinfostart",
        type = "message",
        session = session,
        closeButton = TRUE
      )
      user(usr)
    }
  })

  ##############################################################################
  ## 1. FILE UPLOAD
  ##############################################################################
  boundaryFile <- shiny::reactiveVal(NULL)
  uplFileName <- shiny::reactiveVal()

  shiny::observeEvent(input$new_shape, {
    shiny::validate(shiny::need(input$new_shape, message = "Select file first!"))
    shpFiles <- file.path(tempdir(), "input")
    unlink(paste0(shpFiles, "/*"), recursive = TRUE)

    zip::unzip(
      input$new_shape$datapath,
      exdir = shpFiles,
      overwrite = TRUE,
      junkpaths = TRUE
    )

    fileList <- list.files(file.path(shpFiles), full.names = TRUE)
    shpFile <- fileList[grep("\\.shp$", fileList)]
    if (length(shpFile) == 0) {
      shiny::showModal(shiny::modalDialog(
        title = "Wrong File!",
        "Only files from the Survey Solutions Spatial Sampling App are accepted. Files must be zipped directly and without a folder!",
        easyClose = TRUE,
        footer = NULL
      ))
    }

    ufn <- gsub("\\.zip$", "", input$new_shape$name)
    uplFileName(ufn)
    shiny::validate(shiny::need(shpFile, message = FALSE))

    tmp.shp <- shapeLoad2(path = shpFile)[[2]]
    if (!sf::st_is_longlat(tmp.shp)) {
      tmp.shp <- sf::st_transform(tmp.shp, 4326)
    }

    tmp.shp$CID <- row.names(tmp.shp)
    shiny::updateSelectizeInput(
      session = session,
      inputId = "area_id",
      label = "Identification Variable",
      choices = names(sf::st_set_geometry(tmp.shp, NULL)),
      options = list(
        placeholder = "Select variable below",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
    boundaryFile(tmp.shp)
  })

  ##############################################################################
  ## Create File Path for storage and download per user
  ##############################################################################
  fp <- shiny::reactiveVal()
  shiny::observe({
    appdir <- get_app_download_dir()
    fp(file.path(appdir, paste0(user(), "_", uplFileName())))
  })

  ##############################################################################
  ## Selection of Area
  ##############################################################################
  counter <- shiny::reactiveVal(1)
  areaIDs <- shiny::eventReactive(input$area_id, {
    shiny::validate(shiny::need(input$area_id, message = FALSE))
    ids_df <- sf::st_set_geometry(boundaryFile(), NULL)
    ids <- ids_df[[input$area_id]]
    if (nrow(boundaryFile()) != length(unique(ids))) {
      shiny::showModal(shiny::modalDialog(
        title = "ID is not unique across Enumeration Areas",
        shiny::HTML(
          paste0(
            "The selected Identification Variable <b>",
            input$area_id,
            "</b> is not unique. Please choose the correct one!"
          )
        ),
        easyClose = TRUE,
        footer = NULL
      ))
      shiny::req(FALSE)
    }
    counter(1)
    return(ids)
  })

  ## Show Navigation only AFTER variable is selected
  shiny::observeEvent(input$area_id, {
    shinyjs::show("navigation")
  }, ignoreInit = TRUE)

  ## Disable back button when counter is 1
  shiny::observeEvent(counter(), {
    shiny::req(counter())
    if (counter() <= 1) {
      shinyjs::disable("loadPrevEA")
    } else {
      shinyjs::enable("loadPrevEA")
    }
  }, ignoreInit = FALSE)

  ## Marker collector for active EA
  pointsCollectorGoogle <- shiny::reactiveVal(
    data.table::data.table(long = numeric(0), lat = numeric(0), area_id = character(0))
  )
  areaRefreshTrigger <- shiny::reactiveVal(0L)

  ## Helper to persist current pointsCollectorGoogle() for a given area ID
  persist_area_points <- function(target_area_id, is_first_area = FALSE) {
    points <- pointsCollectorGoogle()
    if (nrow(points) == 0 || is.null(target_area_id) || is.na(target_area_id)) {
      return(invisible(FALSE))
    }
    curr_fp <- fp()
    clean_id <- gsub("[!@#$%^&*/\\\\]*", "", as.character(target_area_id))

    if (is_first_area) {
      provider_label <- if (identical(provider, "vantor")) "Vantor API credentials" else "Google API key"
      raw_full_msg <- sprintf(
        "You are successfully logged in and have provided the required %s. Your files for this session will be stored
         in your personal user directory under, <b>%s</b>. After uploading the file, you can see the complete file
         path.",
        provider_label, curr_fp
      )
      notmessage <- shiny::HTML(trimws(gsub("[[:space:]]+", " ", gsub("\n", "", raw_full_msg))))
      shiny::removeNotification(id = "userinfostart")
      shiny::showNotification(
        ui = notmessage,
        duration = NULL,
        id = "userinfoFull",
        type = "message",
        session = session,
        closeButton = TRUE
      )
    }

    if (!dir.exists(curr_fp)) {
      dir.create(curr_fp, recursive = TRUE, showWarnings = FALSE)
    }
    fn <- file.path(curr_fp, paste0("area_", clean_id, ".fst"))
    if (file.exists(fn)) {
      old_points <- fst::read_fst(fn, as.data.table = TRUE)
      points <- data.table::rbindlist(list(old_points, points), fill = TRUE)
    }

    # Calculate spatial grid coverage against the EA polygon when saving
    poly_sel <- NULL
    if (!is.null(boundaryFile()) && !is.null(input$area_id) && input$area_id %in% names(boundaryFile())) {
      b_shp <- boundaryFile()
      poly_sel <- b_shp[as.character(b_shp[, input$area_id, drop = TRUE]) == as.character(target_area_id), ]
    }
    cov_res <- calculate_polygon_coverage(
      polygon_sf = poly_sel,
      points_df = points,
      n_grid = resolve_n_grid(),
      low_threshold = 20
    )
    points[, coverage_pct := cov_res$coverage_pct]
    points[, coverage_status := cov_res$coverage_status]

    if (isTRUE(cov_res$low_coverage)) {
      shiny::showNotification(
        sprintf(
          "Low spatial coverage (%.1f%%) detected in Enumeration Area %s (< 20%% threshold). Please verify that all sections of the polygon have been listed.",
          cov_res$coverage_pct,
          as.character(target_area_id)
        ),
        type = "warning",
        duration = 8,
        session = session
      )
    }

    tryCatch(
      fst::write_fst(points, fn),
      error = function(e) {
        shiny::showNotification("Could not write area file!", type = "error")
      }
    )
    pointsCollectorGoogle(
      data.table::data.table(long = numeric(0), lat = numeric(0), area_id = character(0))
    )
    areaRefreshTrigger(areaRefreshTrigger() + 1L)
    invisible(TRUE)
  }

  ## NEXT area (saves points from current area, then advances counter if not at last area)
  shiny::observeEvent(input$loadNextEA, {
    tmp.counter <- counter()
    ids <- areaIDs()
    shiny::validate(shiny::need(length(ids) > 0, message = FALSE))
    curr_area_id <- ids[tmp.counter]

    persist_area_points(curr_area_id, is_first_area = (tmp.counter == 1))

    if (tmp.counter < length(ids)) {
      counter(tmp.counter + 1L)
    } else {
      shiny::showNotification(
        "Reached the last Enumeration Area. Current area markers have been saved.",
        type = "message"
      )
      if (tmp.counter > 1) {
        shinyjs::enable("loadPrevEA")
      }
    }
  }, ignoreInit = TRUE)

  ## PREV area
  shiny::observeEvent(input$loadPrevEA, {
    tmp.counter <- counter()
    shiny::validate(shiny::need(tmp.counter > 1, message = FALSE))
    counter(tmp.counter - 1L)
  }, ignoreInit = TRUE)

  ## Existing Area dropdown selection
  shiny::observeEvent(input$loadFile, {
    lf <- input$loadFile
    shiny::validate(shiny::need(lf != "", message = TRUE))
    tmp.counter <- counter()
    tmp.areaIDs <- areaIDs()
    tmp.counter.update <- match(as.character(lf), as.character(tmp.areaIDs))
    if (is.na(tmp.counter.update)) {
      counter(tmp.counter)
      shiny::showNotification(
        "No correspondence in uploaded boundary file, please select correct area!",
        type = "error"
      )
    } else {
      counter(tmp.counter.update)
    }
  }, ignoreInit = TRUE)

  ## Selected EA polygon
  lgaSel <- shiny::reactive({
    tmp.shp <- shiny::req(boundaryFile())
    ids <- shiny::req(areaIDs())
    tmp.counter <- shiny::req(counter())
    shiny::validate(shiny::need(tmp.counter >= 1 && tmp.counter <= length(ids), message = FALSE))
    tmp.areaID <- ids[tmp.counter]
    tmp.shp.sel <- tmp.shp[tmp.shp[, input$area_id, drop = TRUE] == tmp.areaID, ]
    return(tmp.shp.sel)
  })

  ## Trigger for map updates when navigating between areas
  clearMap <- shiny::reactive({
    shiny::req(input$area_id)
    list(counter(), input$loadPrevEA, input$loadNextEA)
  })

  ## Existing saved points for active EA
  lgaPoint <- shiny::reactive({
    clearMap()
    areaRefreshTrigger()
    ids <- shiny::req(areaIDs())
    tmp.counter <- shiny::req(counter())
    shiny::validate(shiny::need(tmp.counter >= 1 && tmp.counter <= length(ids), message = FALSE))
    tmp.areaID <- ids[tmp.counter]
    fn <- gsub("[!@#$%^&*/\\\\]*", "", as.character(tmp.areaID))
    fn <- file.path(fp(), paste0("area_", fn, ".fst"))
    if (file.exists(fn)) {
      points_old <- fst::read_fst(fn, as.data.table = TRUE)
      if (nrow(points_old) > 0) {
        points_sf <- sf::st_as_sf(
          x = points_old,
          coords = c("long", "lat"),
          crs = 4326
        )
        return(points_sf)
      }
    }
    return(NULL)
  })

  ##############################################################################
  ## SIDEBAR: Check existing files and summary table
  ##############################################################################
  savedFiles <- shiny::eventReactive(list(input$area_id, areaRefreshTrigger()), {
    FP <- fp()
    shiny::validate(shiny::need(dir.exists(FP), message = "Nothing to view yet!"))
    point_file_list <- list.files(FP, pattern = "\\.fst$", full.names = TRUE)
    shiny::validate(shiny::need(length(point_file_list) > 0, message = FALSE))
    point_file_names <- basename(tools::file_path_sans_ext(point_file_list))
    point_file_names <- sub("^area_", "", point_file_names)
    return(point_file_names)
  }, ignoreInit = TRUE)

  shiny::observe({
    sv <- savedFiles()
    names(sv) <- as.character(sv)
    shiny::updateSelectizeInput(
      session = session,
      inputId = "loadFile",
      label = "Select existing file",
      choices = as.character(sv),
      options = list(
        placeholder = "Select variable below",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })

  ## LGA and Marker Summary Table
  lgaSelPoint <- shiny::reactiveVal(NULL)
  output$AreaSummary <- DT::renderDT({
    lga.sel <- lgaSel()
    shiny::validate(shiny::need(lga.sel, message = FALSE))
    areaRefreshTrigger()
    new_pts <- pointsCollectorGoogle()

    id_area <- lga.sel[, input$area_id, drop = TRUE]
    clean_id <- gsub("[!@#$%^&*/\\\\]*", "", as.character(id_area))
    FP <- fp()
    FPfile <- file.path(FP, paste0("area_", clean_id, ".fst"))

    saved_pts <- NULL
    if (file.exists(FPfile)) {
      lga.sel.point <- fst::read_fst(FPfile, as.data.table = TRUE)
      N_saved <- nrow(lga.sel.point)
      lgaSelPoint(lga.sel.point)
      saved_pts <- lga.sel.point
    } else {
      N_saved <- 0L
    }
    N_point <- N_saved + nrow(new_pts)

    # Display persisted coverage from saved .fst file (calculated when clicking Next)
    if (!is.null(saved_pts) && N_saved > 0L) {
      if ("coverage_pct" %in% names(saved_pts) && any(!is.na(saved_pts$coverage_pct))) {
        cov_val <- as.numeric(saved_pts$coverage_pct[!is.na(saved_pts$coverage_pct)])[1]
        cov_flag <- if (cov_val < 20) "LOW" else "OK"
      } else {
        cov_res <- calculate_polygon_coverage(polygon_sf = lga.sel, points_df = saved_pts)
        cov_val <- cov_res$coverage_pct
        cov_flag <- cov_res$coverage_status
      }
      cov_label <- sprintf("%.1f%%", cov_val)
    } else {
      cov_label <- "0.0%"
      cov_flag <- "LOW"
    }

    tab <- data.table::data.table(
      ID = id_area,
      `Listed Units` = N_point,
      `Coverage` = cov_label,
      `Status` = cov_flag
    )
    dt_out <- DT::datatable(tab, options = list(dom = "t"), rownames = FALSE)
    DT::formatStyle(
      dt_out,
      "Status",
      color = DT::styleEqual(c("LOW", "OK"), c("#D32F2F", "#2E7D32")),
      fontWeight = "bold"
    )
  })

  ##############################################################################
  ## CREATE AND UPDATE SATELLITE MAP (GOOGLE OR VANTOR / LEAFLET)
  ##############################################################################
  if (identical(provider, "vantor")) {
    output$detailSampMap <- leaflet::renderLeaflet({
      map_key <- shiny::req(mapkey())
      shiny::req(boundaryFile(), areaIDs())
      lga.sel <- shiny::isolate(lgaSel())
      shiny::req(lga.sel)
      tmp.points_sf <- shiny::isolate(lgaPoint())
      vantor_prod <- shiny::isolate(vantorProduct())
      bb <- as.numeric(sf::st_bbox(lga.sel))

      map <- leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 21L)) |>
        vantorr::add_vantor_basemap(
          product = vantor_prod,
          service = "WMTS",
          api_key = map_key,
          group = "Vantor Vivid",
          layerId = "vantor_base",
          verbose = FALSE
        ) |>
        vantorr::add_vantor_seamlines(
          product = vantor_prod,
          mode = "wms",
          api_key = map_key,
          group = "Vantor Seamlines",
          verbose = FALSE
        ) |>
        leaflet::hideGroup("Vantor Seamlines") |>
        leaflet::addPolygons(
          data = lga.sel,
          layerId = "bounds",
          group = "bounds",
          color = "#FF0000",
          weight = 2,
          fillColor = "#FF0000",
          fillOpacity = 0.1,
          options = leaflet::pathOptions(interactive = FALSE, clickable = FALSE)
        ) |>
        leaflet::fitBounds(
          lng1 = bb[1], lat1 = bb[2],
          lng2 = bb[3], lat2 = bb[4]
        ) |>
        leaflet::addLayersControl(
          baseGroups = c("Vantor Vivid"),
          overlayGroups = c("Vantor Seamlines"),
          options = leaflet::layersControlOptions(collapsed = TRUE, position = "topleft")
        )

      if (!is.null(tmp.points_sf)) {
        map <- map |>
          leaflet::addMarkers(data = tmp.points_sf, group = "house")
      }

      map |> add_marker_drawing()
    })
  } else {
    output$detailSampMap <- googleway::renderGoogle_map({
      map_key <- shiny::req(mapkey())
      shiny::req(boundaryFile(), areaIDs())
      lga.sel <- shiny::isolate(lgaSel())
      shiny::req(lga.sel)
      tmp.points_sf <- shiny::isolate(lgaPoint())

      map <- googleway::google_map(
        key = map_key,
        event_return_type = "list",
        map_type = "satellite",
        libraries = c("visualization", "geometry", "places")
      ) |>
        googleway::add_polygons(
          data = lga.sel,
          layer_id = "bounds",
          focus_layer = TRUE,
          fill_opacity = 0.1
        )

      if (!is.null(tmp.points_sf)) {
        map <- map |>
          googleway::add_markers(data = tmp.points_sf, layer_id = "house")
      }

      map |> add_marker_drawing()
    })
  }
  shiny::outputOptions(output, "detailSampMap", suspendWhenHidden = FALSE)

  ## Collect markers placed on the Satellite Map
  shiny::observeEvent(input$detailSampMap_markercomplete, {
    pos <- input$detailSampMap_markercomplete$position
    tmp.counter <- counter()
    tmp.areaID <- areaIDs()[tmp.counter]
    shiny::validate(shiny::need(!is.null(pos), message = FALSE))

    points <- list(
      old = pointsCollectorGoogle(),
      new = data.table::data.table(
        long = as.numeric(pos$lng),
        lat = as.numeric(pos$lat),
        area_id = as.character(tmp.areaID)
      )
    )

    if (nrow(points[["new"]]) > 0) {
      shinyjs::disable("loadPrevEA")
    }
    pointsCollectorGoogle(data.table::rbindlist(points, fill = TRUE))
  })

  ## Update Satellite Map when switching areas or refreshing
  shiny::observeEvent(clearMap(), {
    lga.sel <- lgaSel()
    shiny::req(lga.sel)
    tmp.points_sf <- lgaPoint()

    if (identical(provider, "vantor")) {
      bb <- as.numeric(sf::st_bbox(lga.sel))
      upd <- leaflet::leafletProxy("detailSampMap", session = session) |>
        clear_marker_drawing() |>
        leaflet::clearGroup("bounds") |>
        leaflet::clearGroup("house") |>
        leaflet::addPolygons(
          data = lga.sel,
          layerId = "bounds",
          group = "bounds",
          color = "#FF0000",
          weight = 2,
          fillColor = "#FF0000",
          fillOpacity = 0.1,
          options = leaflet::pathOptions(interactive = FALSE, clickable = FALSE)
        ) |>
        leaflet::fitBounds(
          lng1 = bb[1], lat1 = bb[2],
          lng2 = bb[3], lat2 = bb[4]
        )

      if (!is.null(tmp.points_sf)) {
        upd <- upd |>
          leaflet::addMarkers(data = tmp.points_sf, group = "house")
      }
      upd |> add_marker_drawing()
    } else {
      upd <- googleway::google_map_update(map_id = "detailSampMap") |>
        clear_marker_drawing() |>
        googleway::clear_polygons(layer_id = "bounds") |>
        googleway::clear_markers(layer_id = "house") |>
        googleway::add_polygons(
          data = lga.sel,
          layer_id = "bounds",
          focus_layer = TRUE,
          fill_opacity = 0.1
        )

      if (!is.null(tmp.points_sf)) {
        upd <- upd |>
          googleway::add_markers(data = tmp.points_sf, layer_id = "house")
      }
      upd |> add_marker_drawing()
    }
  }, ignoreInit = TRUE)

  ##############################################################################
  ## CLEAR AREA
  ##############################################################################
  shiny::observeEvent(input$clearAllArea, {
    tmp.counter <- counter()
    tmp.areaID <- areaIDs()[tmp.counter]
    shiny::showModal(shiny::modalDialog(
      "Area cleared!",
      footer = NULL,
      size = "s",
      easyClose = TRUE
    ))
    curr_fp <- fp()
    fn <- gsub("[!@#$%^&*/\\\\]*", "", as.character(tmp.areaID))
    if (!dir.exists(curr_fp)) {
      dir.create(curr_fp, recursive = TRUE, showWarnings = FALSE)
    }

    fn <- file.path(curr_fp, paste0("area_", fn, ".fst"))
    if (file.exists(fn)) {
      file.remove(fn)
    }

    pointsCollectorGoogle(
      data.table::data.table(long = numeric(0), lat = numeric(0), area_id = character(0))
    )
    if (identical(provider, "vantor")) {
      leaflet::leafletProxy("detailSampMap", session = session) |>
        clear_marker_drawing() |>
        leaflet::clearGroup("house")
    } else {
      googleway::google_map_update(map_id = "detailSampMap") |>
        clear_marker_drawing() |>
        googleway::clear_markers(layer_id = "house")
    }
    areaRefreshTrigger(areaRefreshTrigger() + 1L)
    shinyjs::disable("loadPrevEA")
  }, ignoreInit = TRUE)

  ##############################################################################
  ## DOWNLOAD
  ##############################################################################
  dwlFile <- shiny::reactiveVal()
  shiny::observeEvent(input$downloadActivate, {
    tmp.shp <- shiny::req(boundaryFile())
    # Persist any unsaved markers from the active area before packaging download
    if (nrow(pointsCollectorGoogle()) > 0 && length(areaIDs()) >= counter()) {
      persist_area_points(areaIDs()[counter()])
    }

    tmp.shp <- sf::st_set_geometry(tmp.shp, NULL)
    temp.dir <- file.path(tempdir(), "dwl_export")
    if (dir.exists(temp.dir)) {
      unlink(temp.dir, recursive = TRUE)
    }
    dir.create(temp.dir, recursive = TRUE, showWarnings = FALSE)

    ts_str <- gsub("[^0-9_]", "", format(Sys.time(), "%Y%m%d_%H%M%S"))
    fs <- character(2)
    fs[1] <- file.path(temp.dir, paste0("AreaswithMarker-", ts_str, ".csv"))
    fs[2] <- file.path(temp.dir, paste0("AllAreas-", ts_str, ".csv"))
    fs.zip <- file.path(temp.dir, paste0("BuildingFrame-", ts_str, ".zip"))

    FP <- fp()
    shiny::validate(shiny::need(dir.exists(FP), message = "Nothing to view yet!"))
    point_file_list <- list.files(FP, pattern = "\\.fst$", full.names = TRUE)
    shiny::validate(shiny::need(length(point_file_list) > 0, message = FALSE))

    point_files <- vector("list", length = length(point_file_list))
    names(point_files) <- basename(point_file_list)
    for (i in point_file_list) {
      point_files[[basename(i)]] <- fst::read_fst(i, as.data.table = TRUE)
      file.remove(i)
    }
    point_files <- data.table::rbindlist(point_files, fill = TRUE)

    shiny::withProgress(
      message = "Preparing data for download",
      value = 0,
      {
        data.table::fwrite(point_files, fs[1])
        data.table::fwrite(tmp.shp, fs[2])
        shiny::incProgress(1 / 2)
        zip::zip(zipfile = fs.zip, files = fs, mode = "cherry-pick")
        dwlFile(fs.zip)
        shiny::incProgress(1 / 2)
      }
    )
    if (file.exists(fs.zip)) {
      shinyjs::runjs("$('#dwl_pop')[0].click();")
    }
  })

  output$dwl_pop <- shiny::downloadHandler(
    filename = function() {
      paste0("BuildingFrame-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".zip")
    },
    content = function(file) {
      shiny::withProgress(
        message = "Downloading Data...",
        value = 0,
        {
          fs.zip <- shiny::req(dwlFile())
          file.copy(fs.zip, file)
        }
      )
    },
    contentType = "application/zip"
  )

  ##############################################################################
  ## SECOND STAGE SAMPLE & SURVEY SOLUTIONS ASSIGNMENT
  ##############################################################################
  shiny::observe({
    shiny::hideTab(inputId = "mapType", target = "samp", session = session)
    shiny::hideTab(inputId = "mapType", target = "suso", session = session)
  })

  shiny::observeEvent(sampleadmreturn$admcheckok(), {
    ac <- sampleadmreturn$admcheckok()
    if (isTRUE(ac)) {
      shiny::showTab(inputId = "mapType", target = "samp", session = session)
      shiny::showTab(inputId = "mapType", target = "suso", session = session)
    } else {
      shiny::hideTab(inputId = "mapType", target = "samp", session = session)
      shiny::hideTab(inputId = "mapType", target = "suso", session = session)
    }
  })

  sampleadmreturn <- modal_adminspsample_server("sampleadm", side = shiny::reactive(input$side))
  mapadminSRV_st2("susoassign", pointsfile = sampleadmreturn$sampleData)
}
