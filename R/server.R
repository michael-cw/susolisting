#' Server function for listing App
#'
#' @noRd
#' @keywords internal
#'




    ## Server Function
main_server<-function(input, output, session) {
  # Changing option for duration of session
  # shiny.maxRequestSizeold<-getOption("shiny.maxRequestSize")
  # shiny::shinyOptions(shiny.maxRequestSize=5000*1024^2)
  # on.exit(options(shiny.maxRequestSize=shiny.maxRequestSizeold))
  #
  # spinner.color.backgroundold<-getOption("spinner.color.background")
  # options(spinner.color.background="#0d47a1")
  # on.exit(options(spinner.color.background=spinner.color.backgroundold))

  # DT style
  smTab<-list(dom="tp")

  ############################
  ##  Get User
  ##    -each user gets own user directory
  # user<-reactive({
  #   user<-session$user
  #   user<-ifelse(is.null(user), "superuser", user)
  #   #user<-"encovi_2019_hq4"
  #   return(user)
  # })



  ##  Start-up Modal
  observe({
    shinyalert::shinyalert(
      inputId = "startup",
      title = paste("Welcome to the Survey Solutions Listing Application!"),
      text = tagList(
        shiny::h4("Please make sure you have read the relevant documentation, avialable under: https://datanalytics.worldbank.org/SpatialSamplingManual/
          To continue please provide your Google API key and your username."),
        shiny::passwordInput(
          inputId = "mapkey",
          label = "",
          placeholder = "Google API key"
        ),br(),
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
      animation = TRUE)
  })

  mapkey<-reactiveVal(NULL)
  user<-reactiveVal(NULL)
  observeEvent(input$startup, {
    if(input$mapkey=="") {
      shinyalert::shinyalert(paste("Google API key missing!"),
                             "You have not provided a key. Without an Google API key, it won't be possible to continue. Please refresh the page and
                             provide the correct key.",
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
                             animation = TRUE)
      req(FALSE)
    } else {
      mapkey(input$mapkey)
      user(input$user)
    }
  })
  #################################################################################
  ##   1. FILE UPLOAD                                                             #
  #################################################################################
  ## 1. Input, Transformation and update of dropdown
  boundaryFile<-reactiveVal(NULL)
  uplFileName<-reactiveVal()
  observeEvent(input$new_shape, {
    shiny::validate(need(input$new_shape, message = "Select file first!"))
    ## ATTENTION: files from sampling application are now in the top folder
    shpFiles<-file.path(tempdir(), "input")
    unlink(paste0(shpFiles, "/*"), recursive = T)
    #unzip(file.path(input$new_shape$datapath), exdir = shpFiles)

    zip::unzip(input$new_shape$datapath,
               exdir = shpFiles,
               overwrite = T,
               junkpaths = T)
    fileList<-dir(shpFiles, recursive = TRUE, full.names = T)
    csvFile<-fileList[grep(".csv$", fileList)]

    fileList<-list.files(file.path(shpFiles), full.names = T)
    shpFile<-fileList[grep(".shp$", fileList)]
    if (length(shpFile)==0) {
      showModal(modalDialog(
        title = "Wrong File!",
        "Only files from the Survey Solutions Spatial Sampling App are accepted. Files must be zipped directly and without
        a folder!",
        easyClose = TRUE,
        footer = NULL
      ))

    }

    ufn<-stringr::str_remove_all(input$new_shape$name, pattern = ".zip")
    uplFileName(ufn)
    shpFiles<-file.path(shpFile)
    shiny::validate(need(shpFile, message = F))
    ## Function takes file and returns list w file (2) and crs(1)
    tmp.shp<-shapeLoad2(path=shpFile)
    tmp.shp<-tmp.shp[[2]]

    #################################
    ## Transform CRS to longLat
    if(!sf::st_is_longlat(tmp.shp)) {
      tmp.shp<-tmp.shp %>% st_transform(4326)
    }
    #################################
    ## CID: should come from Samplin App
    tmp.shp$CID<-row.names(tmp.shp)
    updateSelectizeInput(session = session, inputId = "area_id",
                         label = "Identification Variable",
                         choices = names(tmp.shp %>% st_set_geometry(NULL)),
                         options = list(
                           placeholder = 'Select variable bellow',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
    boundaryFile(tmp.shp)
  })
  ###########################################
  ##  Create File Path for storage and dwl
  ##   per user
  ###########################################
  fp<-reactiveVal()
  observe({
    #appdir<-path.expand(file.path("~","susolisting_data", "download"))
    appdir<-file.path(tools::R_user_dir("susolisting", which = "data"), "download")
    #print(shiny::getShinyOption("DT.options"))
    #print(getOption("spinner.color.background"))
    if(!dir.exists(appdir)){
      # !only if not exists
      # 1. Data dir
      dir.create(appdir, recursive = TRUE, showWarnings = FALSE)
    }
    fp(file.path(appdir, paste0(user(), "_" , uplFileName())))})
  ###########################################
  ##  Selection of Area
  ##  1. Select ID var
  counter<-reactiveVal(1)
  areaIDs<-eventReactive(input$area_id, {
    shiny::validate(need(input$area_id, message = F))
    ids<-boundaryFile() %>% dplyr::select(input$area_id) %>% st_set_geometry(NULL)
    ids<-ids[,input$area_id]
    if(nrow(boundaryFile())!=dplyr::n_distinct(ids)) {
      showModal(modalDialog(
        title = "ID is not unique across Enumeration Areas",
        HTML(
          paste0(
            "The selected Identification Variable <b>",
            input$area_id,
            "</b> is not unique. Please choose the correct one!"
          )
        ),
        easyClose = TRUE,
        footer = NULL
      ))
      req(FALSE)
    }
    return(ids)
  })
  ## 2. Show Navigation only AFTER variable is selected
  observeEvent(input$area_id,{
    shinyjs::show("navigation")
    #shinyjs::show("dwlButton")
  }, ignoreInit = TRUE)

  ## 3. NEXT area
  observeEvent(input$loadNextEA,{
    tmp.counter<-counter()
    shiny::validate(need(tmp.counter<=length(areaIDs()), message = F))
    counter(tmp.counter+1)
  }, ignoreInit = TRUE)
  ## 4. PREV area
  observeEvent(input$loadPrevEA,{
    tmp.counter<-counter()
    shiny::validate(need(tmp.counter>1, message = F))
    counter(tmp.counter-1)
  }, ignoreInit = TRUE)
  ## 5. Existing Area
  observeEvent(input$loadFile,{
    lf<-input$loadFile
    shiny::validate(need(lf!="", message = T))
    lf<-as.numeric(lf)
    tmp.counter<-counter()
    tmp.areaIDs<-areaIDs()
    tmp.counter.update<-match(lf, tmp.areaIDs)
    if(is.na(tmp.counter.update)){
      counter(tmp.counter)
      shiny::showNotification("No correspondence in uploaded boundary file,
                              please select correct area!", type = "error")
    } else {
      counter(tmp.counter.update)
    }
  }, ignoreInit = T)
  ## 6. SELECT area
  lgaSel<-reactive({
    tmp.shp<-boundaryFile()
    tmp.counter<-counter()
    tmp.areaID<-areaIDs()[tmp.counter]
    tmp.shp.sel<-tmp.shp[tmp.shp[,input$area_id, drop=T]==tmp.areaID, ]
    return(tmp.shp.sel)
  })
  ## 6. CHECK for existing points
  lgaPoint<-reactive({
    tmp.counter<-counter()
    tmp.areaID<-areaIDs()[tmp.counter]
    fn<-stringr::str_replace_all(tmp.areaID, "[!@#$%^&*/\\\\]*", "")
    fn<-file.path(fp(),paste0("area_", fn,".fst"))
    if(file.exists(fn)) {
      points_old<-fst::read_fst(fn, as.data.table = T)
      points_sf<-st_as_sf(x = points_old,
                          coords = c("long", "lat"),
                          crs = 4326)
      return(points_sf)
    } else {
      return(NULL)
    }
  })

  ####################################################################
  ##  1. SIDEBAR
  ##  1.1. Check files and list
  ##      -- user can select from file dropdown
  ##  1.1.1. Existing files
  savedFiles<-eventReactive(input$area_id, {
    FP<-fp()

    shiny::validate(need(dir.exists(FP), message = "Nothing to view yet!"))
    print(list.files(FP, full.names = T))
    print(FP)
    point_file_list<-list.files(FP, pattern = ".fst$", full.names = T)
    shiny::validate(need(length(point_file_list)>0, message = F))
    # point_file_names<-stringr::str_split(point_file_list, "/", simplify = T)[,4]
    point_file_names<-basename(tools::file_path_sans_ext(point_file_list))
    point_file_names<-as.numeric(stringr::str_extract(point_file_names, "\\d+"))
    ## ii. LOAD and APPEND all files--FST

  }, ignoreInit = T)
  ## 1.1.2. update input
  observe({
    sv<-savedFiles()
    names(sv)<-as.character(sv)
    updateSelectizeInput(session = session, "loadFile", "Select existing file",
                         choices = as.character(sv),
                         options = list(
                           placeholder = 'Select variable bellow',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
  })


  # ##  1.2. LGA and Marker Summary Table
  lgaSelPoint<-reactiveVal(NULL)
  output$AreaSummary<-DT::renderDataTable({
    lga.sel<-lgaSel()
    shiny::validate(need(lga.sel, message = F))
    ## area id
    id_area<-lga.sel[,input$area_id, drop=T]
    ## check for available file
    FP<-fp()
    FPfile<-file.path(FP, paste0("area_", id_area, ".fst"))
    ## if file available report counte else 0
    if(file.exists(FPfile)) {
      lga.sel.point<-fst::read_fst(FPfile, as.data.table = T)
      N_point<-nrow(lga.sel.point)
      lgaSelPoint(lga.sel.point)
    } else {
      N_point<-0
    }
    tab<-data.table::data.table(ID=id_area, `Listed Units`=N_point)
    tab<-DT::datatable(tab, options = list(dom="t"))
    return(tab)
  })



  ## CREATE google map
  output$detailSampMap<-googleway::renderGoogle_map({
    map_key<-req(mapkey())
    lga.sel<-lgaSel()
    req(lga.sel)
    tmp.points_sf<-lgaPoint()
    if(is.null(tmp.points_sf)) {
      map<-google_map(key = map_key, event_return_type="list") %>%
        add_polygons(data=lga.sel, layer_id = "bounds", focus_layer = T,
                     fill_opacity = 0.1) %>%
        add_drawing(drawing_modes = "marker")
    } else {
      map<-google_map(key = map_key, event_return_type="list") %>%
        add_polygons(data=lga.sel, layer_id = "bounds", focus_layer = T,
                     fill_opacity = 0.1) %>%
        add_markers(data = tmp.points_sf, layer_id = "house")%>%
        add_drawing(drawing_modes = "marker")
    }
    return(map)
  })
  ## google map return test
  pointsCollectorGoogle<-reactiveVal(data.table::data.table(long=numeric(0), lat=numeric(0), area_id=character(0)))
  observeEvent(input$detailSampMap_markercomplete, {
    pos<-input$detailSampMap_markercomplete$position
    tmp.counter<-counter()
    tmp.areaID<-areaIDs()[tmp.counter]
    shiny::validate(need(!is.null(pos), message = F))
    points<-list()
    points[["old"]]<-pointsCollectorGoogle()
    long<-pos$lng
    lat<-pos$lat
    points[["new"]]<-data.table::data.table(long=long,lat=lat, area_id=tmp.areaID)
    points<-rbindlist(points)
    pointsCollectorGoogle(points)
  })
  ## UPDATE google map
  ## i. create summary reactive trigger
  clearMap<-reactive({
    list(counter(), input$mapType, input$area_id, input$loadPrevEA, input$loadNextEA)
  })
  ## ii. summary triggers the map upadate
  observeEvent(clearMap(),{
    lga.sel<-lgaSel()
    req(lga.sel)
    tmp.points_sf<-lgaPoint()
    checkp<-tmp.points_sf
    if(is.null(tmp.points_sf)) {
      google_map_update(map_id = "detailSampMap") %>%
        clear_polygons(layer_id = "bounds") %>%
        clear_markers(layer_id = "house") %>%
        add_polygons(data=lga.sel, layer_id = "bounds", focus_layer = T,
                     fill_opacity = 0.1)
    } else {
      google_map_update(map_id = "detailSampMap") %>%
        clear_polygons(layer_id = "bounds") %>%
        clear_markers(layer_id = "house") %>%
        add_polygons(data=lga.sel, layer_id = "bounds", focus_layer = T,
                     fill_opacity = 0.1) %>%
        add_markers(data = tmp.points_sf, layer_id = "house")
    }
  }, ignoreInit = F)



  ## 3. SAVE points by area
  observeEvent(input$loadNextEA,{
    ## Less one since it changes already
    tmp.counter<-counter()-1
    tmp.areaID<-areaIDs()[tmp.counter]
    points<-pointsCollectorGoogle()
    shiny::validate(need(nrow(points)>0, message=F))

    fp<-fp()
    fn<-stringr::str_replace_all(tmp.areaID, "[!@#$%^&*/\\\\]*", "")
    if(!dir.exists(fp)) {
      ##  i. Check for existing USER dir, otherwise create
      dir.create(fp)
    }
    fn<-file.path(fp,paste0("area_", fn,".fst"))
    if(file.exists(fn)){
      old_points<-fst::read_fst(fn, as.data.table = T)
      points<-rbindlist(list(old_points, points))
      tryCatch(
        fst::write_fst(points,fn),
        error = function(e) {
          showNotification("File already exists!")
        }
      )
    } else {
      tryCatch(
        fst::write_fst(points,fn),
        error = function(e) {
          showNotification("File already exists!")
        }
      )
    }
    ## RESET the collector
    pointsCollectorGoogle(data.table(long=numeric(0), lat=numeric(0), area_id=character(0)))
  })

  #########################################################################
  ##    CLEAR AREA
  observeEvent(input$clearAllArea,{
    tmp.counter<-counter()
    tmp.areaID<-areaIDs()[tmp.counter]
    showModal(modalDialog("Area cleared!", footer = "To see changes click next and go back again!", size = "s",
                          easyClose = T))
    fp<-fp()
    fn<-stringr::str_replace_all(tmp.areaID, "[!@#$%^&*/\\\\]*", "")
    if(!dir.exists(fp)) {
      ##  i. Check for existing USER dir, otherwise create
      dir.create(fp)
    }

    fn<-file.path(fp,paste0("area_", fn,".fst"))
    if(file.exists(fn)){
      file.remove(fn)
    }
    google_map_update(map_id = "detailSampMap") %>%
      clear_drawing()
  }, ignoreInit = F)


  #########################################################################
  ##    DOWNLOAD
  ##  1. Generate
  dwlFile<-reactiveVal()
  observeEvent(input$downloadActivate,{
    ## i. Generate file path for zip & get tmp dir
    tmp.shp<-boundaryFile()
    req(tmp.shp)
    tmp.shp<-tmp.shp %>% st_set_geometry(NULL)
    ## Clear TMP dir
    unlink(paste0(tempdir(),"/*"))
    temp.dir<-tempdir()
    ## create file names
    fs<-character(2)
    fs[1] <- file.path(temp.dir, paste("AreaswithMarker-", stringr::str_remove_all(Sys.time(), "(:)|(-)|( )"), ".csv", sep=""))
    fs[2] <- file.path(temp.dir, paste("AllAreas-", stringr::str_remove_all(Sys.time(), "(:)|(-)|( )"), ".csv", sep=""))
    fs.zip<-file.path(temp.dir, paste("BuildingFrame-", stringr::str_remove_all(Sys.time(), "(:)|(-)|( )"), ".zip", sep=""))

    ## ii. CHECK LOCAL directory
    FP<-fp()
    shiny::validate(need(dir.exists(FP), message = "Nothing to view yet!"))
    point_file_list<-list.files(FP, pattern = ".fst$", full.names = T)
    shiny::validate(need(length(point_file_list)>0, message = F))
    ## ii. LOAD and APPEND all files--FST
    point_files<-vector("list", length = length(point_file_list))
    for(i in point_file_list) {
      point_files[[basename(i)]]<-fst::read_fst(i, as.data.table = T)
      file.remove(i)
    }
    ## iii. Single file write to disk
    point_files<-rbindlist(point_files)
    #wdOld<-getwd()
    #setwd(temp.dir)
    withProgress(message = paste('Preparing data for download'),
                 value = 0, {
                   ##  CSV only (too big for excel)
                   write_csv(point_files, fs[1])
                   write_csv(tmp.shp, fs[2])
                   incProgress(1/2)
                   zip::zip(zipfile=fs.zip, files=fs, mode = "cherry-pick")
                   dwlFile(fs.zip)
                   incProgress(1/2)
                 })
    if(file.exists(fs.zip)){
      runjs("$('#dwl_pop')[0].click();")
    }
  })

  ##  DWL
  output$dwl_pop <- downloadHandler( filename = function() {
    paste("BuildingFrame-", Sys.time(), ".zip", sep="")
  },
  content = function(file) {
    withProgress(message = paste('Downloading Data...'),
                 value = 0, {

                   fs.zip <- req(dwlFile())
                   file.copy(fs.zip, file)
                 })

  }, contentType = "application/zip")


  ###############################################################################################
}
