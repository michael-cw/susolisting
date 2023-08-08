#' shiny module for  UI for admin
#'
#'
#' @noRd
#' @keywords internal
#'

modal_adminspsample_ui_tabmain<-function(id) {
  ns<-NS(id)
  tagList(
    # modal for second stage sample, with in app data
    modal_spsample2stage_ui(ns("sample"), inputMode = "app")
  )
}

modal_adminspsample_ui_tabside<-function(id, label = "Generate Frame",
                                         style = "color: #FFFFFF; width: 180px;background-color: #1976D2;border-color: #0d47a1") {
  ns<-NS(id)
  tagList(
    fluidRow(
      ## 1. Action button to start the dwl
      shinyjs::disabled(
        column(1),
        column(10,
        actionButton(ns("generateFrame"),
                     label = label,
                     icon("download"), width = "100vw",
                     style=style)
        ),
        column(1)
      )
    )
  )
}

#'
#' @noRd
#' @keywords internal
#'

# server
modal_adminspsample_server <- function(id,
                                       side = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {

      # permission check
      # get credentials for admin
      observeEvent(side(),{
        ns<-NS(id)
        # show TAB when sidebar sample tab is selected
        if(side() =="Sample"){
          shinyalert::shinyalert(
            inputId = "admincheck",
            title = paste("Sample Generation & Assignment creation"),
            text = tagList(
              shiny::h4("Please make sure you have read the relevant documentation, avialable under: https://datanalytics.worldbank.org/SpatialSamplingManual/
          To continue please provide your credentials. In this section you will be able, to compile the frame from all collaborators, and
                  create the 2nd stage sample, which can either be downloaded or directly uploaded to you Survey Solutions Server."),
              shiny::textInput(
                inputId = ns("admuser"),
                label = "",
                placeholder = "Admin User Name"
              ),br(),
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
            animation = TRUE)
        } else {
          # hide TAB when sidebar sample tab is not selected
          admcheckResult(FALSE)
          shinyjs::disable("generateFrame")
        }
      }, ignoreInit = T)

      # check credentials for admin
      admcheckResult<-reactiveVal(FALSE)
      observeEvent(input$admincheck, {
        req(input$admuser, input$admpass)
        # show tab when credentials match
        if(input$admuser==getOption("admuser") && input$admpass == getOption("admpass")) {
          admcheckResult(TRUE)
          showNotification("Success!", type = "message", id = "admsuccess")
          shinyjs::enable("generateFrame")
        } else {
          admcheckResult(FALSE)
          shinyjs::disable("generateFrame")
        }

      })

      # generate the frame
      framefiles<-reactiveVal(NULL)
      observeEvent(input$generateFrame, {
        # get app dir
        appdir<-file.path(tools::R_user_dir("susolisting", which = "data"), "download")
        ndirs<-list.dirs(appdir, recursive = F, full.names = T)
        if(length(ndirs)==0){
         # if dir is empty notify
          showNotification("No Listing data available!", type = "error", id = "nolisting")
        } else if (length(ndirs)>0) {
          allfiles<-list()
          for(di in ndirs) {
            # get dir info
            dilist<-data.table::data.table(listfiles=list.files(di, pattern = ".fst$", full.names = T))
            # skip if no files
            if(nrow(dilist)==0) next()
            # export for processing
            allfiles[[basename(di)]]<-dilist

          }
          allfiles<-data.table::rbindlist(allfiles, idcol = "dir")
          req(nrow(allfiles)>0)
          # get ea code: if code unique-->no change, if not, username

          framefiles(allfiles)
          shinyalert::shinyalert(
            inputId = "framecreatconfirm",
            title = paste("Generate the sampling frame"),
            text = tagList(
              shiny::h4(HTML(sprintf('This will create the 2nd stage sampling frame for further processing. There are currently
              <p style="color:red; font-weight: bold;">%d user directories and %d files </p> to process.
                        Do you want to continue?', length(ndirs), nrow(allfiles))))
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
            animation = TRUE)


        }
      })

      dt_frame<-reactiveVal(NULL)

      observeEvent(input$framecreatconfirm, {
        ac<-admcheckResult()
        if(ac & input$framecreatconfirm) {
          print("success")
          fnall<-req(framefiles())
          #fn<-file.path(fp(),paste0("area_", fn,".fst"))
          points_dt<-list()
          for(i in 1:nrow(fnall)){
            fn<-fnall$listfiles[i]
            if(file.exists(fn)) {
              #eaname<-paste0(fnall$dir[i], basename(tools::file_path_sans_ext(fnall$listfiles[i])))
              eaname<-basename(tools::file_path_sans_ext(fnall$listfiles[i]))
              points_dt[[eaname]]<-fst::read_fst(fn, as.data.table = T)

            } else {
              next()
            }
          }

          # bind dts generate EA from list name
          points_dt<-data.table::rbindlist(points_dt, idcol = "ea")
          data.table::setorderv(points_dt, "ea")
          # return
          dt_frame(points_dt)
        }
      })

      # Returns SF!!!!!
      sampleDataSF<-modal_spsample2stage_server("sample", inputMode = "app",
                                  framepoints_sf = dt_frame,
                                  patternlat = "lat", patternlong = "long")

      # transform sf to points
      sampleData<-eventReactive(sampleDataSF(), {
        tabsf<-req(sampleDataSF())
        tab<-as.data.frame(tabsf %>% sf::st_set_geometry(NULL))
        tab<-data.table::as.data.table(tab)
        return(tab)
      })

      # RETURN LIST
      return(
        list(
          admcheckok=admcheckResult,
          sampleData = sampleData
        )
      )



    }
  )}
