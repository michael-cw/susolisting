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

  # Disable INDIVIDUAL download when admuser/admpass options are set
  shiny::observe({
    if (!is.null(getOption("admuser")) && !is.null(getOption("admpass"))) {
      shinyjs::disable("downloadActivate")
    }
  })

  ## Start-up Modal
  shiny::observe({
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
  })

  mapkey <- shiny::reactiveVal(NULL)
  user <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$startup, {
    if (is.null(input$mapkey) || input$mapkey == "") {
      shinyalert::shinyalert(
        "Google API key missing!",
        "You have not provided a key. Without a Google API key, it won't be possible to continue. Please refresh the page and provide the correct key.",
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
      mapkey(input$mapkey)

      # Sanitize user name & show notification with path
      usr <- sanitize_string(input$user)

      appdir <- file.path(tools::R_user_dir("susolisting", which = "data"), "download")
      notmessage <- shiny::HTML(
        sprintf(
          "You are successfully logged in and have provided the required Google API key. Your files for this session will be stored
           in your personal user directory under, <b>%s/%s_[upload file name]</b>. After uploading the file, you can see the complete file
           path.",
          appdir, usr
        ) %>%
          stringr::str_remove_all("\\n") %>%
          stringr::str_squish()
      )

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

    ufn <- stringr::str_remove_all(input$new_shape$name, pattern = "\\.zip$")
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
    appdir <- file.path(tools::R_user_dir("susolisting", which = "data"), "download")
    if (!dir.exists(appdir)) {
      dir.create(appdir, recursive = TRUE, showWarnings = FALSE)
    }
    fp(file.path(appdir, paste0(user(), "_", uplFileName())))
  })

  ##############################################################################
  ## Selection of Area
  ##############################################################################
  counter <- shiny::reactiveVal(1)
  areaIDs <- shiny::eventReactive(input$area_id, {
    shiny::validate(shiny::need(input$area_id, message = FALSE))
    ids_df <- boundaryFile() %>%
      dplyr::select(dplyr::all_of(input$area_id)) %>%
      sf::st_set_geometry(NULL)
    ids <- ids_df[[input$area_id]]
    if (nrow(boundaryFile()) != dplyr::n_distinct(ids)) {
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
    clean_id <- stringr::str_replace_all(as.character(target_area_id), "[!@#$%^&*/\\\\]*", "")

    if (is_first_area) {
      notmessage <- shiny::HTML(
        sprintf(
          "You are successfully logged in and have provided the required Google API key. Your files for this session will be stored
           in your personal user directory under, <b>%s</b>. After uploading the file, you can see the complete file
           path.",
          curr_fp
        ) %>%
          stringr::str_remove_all("\\n") %>%
          stringr::str_squish()
      )
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

  ## Trigger for map updates
  clearMap <- shiny::reactive({
    list(counter(), input$area_id, input$loadPrevEA, input$loadNextEA, areaRefreshTrigger())
  })

  ## Existing saved points for active EA
  lgaPoint <- shiny::eventReactive(clearMap(), {
    ids <- shiny::req(areaIDs())
    tmp.counter <- shiny::req(counter())
    shiny::validate(shiny::need(tmp.counter >= 1 && tmp.counter <= length(ids), message = FALSE))
    tmp.areaID <- ids[tmp.counter]
    fn <- stringr::str_replace_all(as.character(tmp.areaID), "[!@#$%^&*/\\\\]*", "")
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
    point_file_names <- stringr::str_remove(point_file_names, "^area_")
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
    clean_id <- stringr::str_replace_all(as.character(id_area), "[!@#$%^&*/\\\\]*", "")
    FP <- fp()
    FPfile <- file.path(FP, paste0("area_", clean_id, ".fst"))

    if (file.exists(FPfile)) {
      lga.sel.point <- fst::read_fst(FPfile, as.data.table = TRUE)
      N_saved <- nrow(lga.sel.point)
      lgaSelPoint(lga.sel.point)
    } else {
      N_saved <- 0L
    }
    N_point <- N_saved + nrow(new_pts)
    tab <- data.table::data.table(ID = id_area, `Listed Units` = N_point)
    DT::datatable(tab, options = list(dom = "t"), rownames = FALSE)
  })

  ##############################################################################
  ## CREATE AND UPDATE GOOGLE MAP
  ##############################################################################
  output$detailSampMap <- googleway::renderGoogle_map({
    map_key <- shiny::req(mapkey())
    lga.sel <- lgaSel()
    shiny::req(lga.sel)
    tmp.points_sf <- lgaPoint()

    map <- googleway::google_map(
      key = map_key,
      event_return_type = "list",
      map_type = "satellite",
      libraries = c("visualization", "geometry", "places")
    ) %>%
      googleway::add_polygons(
        data = lga.sel,
        layer_id = "bounds",
        focus_layer = TRUE,
        fill_opacity = 0.1
      )

    if (!is.null(tmp.points_sf)) {
      map <- map %>%
        googleway::add_markers(data = tmp.points_sf, layer_id = "house")
    }

    map %>% add_marker_drawing()
  })

  ## Collect markers placed on the Google Map
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

  ## Update Google Map when switching areas or refreshing
  shiny::observeEvent(clearMap(), {
    lga.sel <- lgaSel()
    shiny::req(lga.sel)
    tmp.points_sf <- lgaPoint()

    upd <- googleway::google_map_update(map_id = "detailSampMap") %>%
      clear_marker_drawing() %>%
      googleway::clear_polygons(layer_id = "bounds") %>%
      googleway::clear_markers(layer_id = "house") %>%
      googleway::add_polygons(
        data = lga.sel,
        layer_id = "bounds",
        focus_layer = TRUE,
        fill_opacity = 0.1
      )

    if (!is.null(tmp.points_sf)) {
      upd <- upd %>%
        googleway::add_markers(data = tmp.points_sf, layer_id = "house")
    }
    upd %>% add_marker_drawing()
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
    fn <- stringr::str_replace_all(as.character(tmp.areaID), "[!@#$%^&*/\\\\]*", "")
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
    googleway::google_map_update(map_id = "detailSampMap") %>%
      clear_marker_drawing() %>%
      googleway::clear_markers(layer_id = "house")
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

    ts_str <- stringr::str_remove_all(format(Sys.time(), "%Y%m%d_%H%M%S"), "[^0-9_]")
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
        readr::write_csv(point_files, fs[1])
        readr::write_csv(tmp.shp, fs[2])
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
