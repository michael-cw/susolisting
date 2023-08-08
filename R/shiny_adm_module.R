#' shiny module for  UI for admin
#'
#'
#' @noRd
#' @keywords internal
#'

# main module UI with sampling module
modal_adminspsample_ui_tabmain<-function(id) {
  ns<-NS(id)
  tagList(
    # modal for second stage sample, with in app data
    modal_spsample2stage_ui(ns("sample"), inputMode = "app")
  )
}

# sidebar module UI with frame generation, frame overview table, and sample overview table
modal_adminspsample_ui_tabside<-function(id, label = "Generate Frame",
                                         style = "color: #FFFFFF; background-color: #1976D2;border-color: #0d47a1") {
  ns<-NS(id)
  tagList(
    fluidRow(
      ## 1. Action button to start the dwl
      shinyjs::disabled(
        column(1),
        column(10,
        actionButton(ns("generateFrame"),
                     label = label,
                     icon("download"),
                     width = "15vw",
                     style=style)
        ),
        column(1)
      )
    ),br(),
    fluidRow(
      column(2),
      column(8,
             shinyjs::hidden(
               div(id = ns("headframe"),
                   h4("Frame Summary",
                      style = "color: #0d47a1;text-align: center;")
               )
             )
      ),
      column(2)
    ),
    fluidRow(
      column(1),
      column(10,
             DT::dataTableOutput(ns("framesummary"),
                                 width = "15vw")
             ),
      column(1)
    ),br(),
    fluidRow(
      column(2),
      column(8,
             shinyjs::hidden(
               div(id = ns("headsample"),
                   h4("Sample Summary",
                      style = "color: #0d47a1;text-align: center;")
               )
             )
      ),
      column(2)
    ),
    fluidRow(
      column(1),
      column(10,
             DT::dataTableOutput(ns("samplesummary"),
                                 width = "15vw")
      ),
      column(1)
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
        # show TAB when sidebar sample or suso tab is selected and admin check has not been performed
        if(!(admcheckResult()) & (side() =="Sample"| side()=="SuSo")){
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
        } else if (admcheckResult()) {
          # do not check when already exists
          admcheckResult(TRUE)
          shinyjs::enable("generateFrame")

        } else {
          # hide TAB when sidebar sample tab is not selected
          admcheckResult(FALSE)
          shinyjs::disable("generateFrame")
        }
      }, ignoreInit = T)

      # check credentials for admin
      admcheckResult<-reactiveVal(FALSE)
      observeEvent(input$admincheck, {
        # show tab when credentials match
        if(is.null(getOption("admuser")) || is.null(getOption("admpass"))) {
          showNotification("No admin credentials have been provided at start-up! Please set admin
                           credentials first, to use the sampling and assignment module.", type = "error", id = "noadm")
          req(FALSE)

        } else {
          req(input$admuser, input$admpass)

          if(input$admuser==getOption("admuser") && input$admpass == getOption("admpass")) {
            admcheckResult(TRUE)
            showNotification("Success!", type = "message", id = "admsuccess")
            shinyjs::enable("generateFrame")
          } else {
            admcheckResult(FALSE)
            shinyjs::disable("generateFrame")
          }
        }

      })

      # generate the frame
      framefiles<-reactiveVal(NULL)
      observeEvent(input$generateFrame, {
        # get app dir
        appdir<-file.path(tools::R_user_dir("susolisting", which = "data"), "download")
        # list all user directories
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
      # compile the frame
      dt_frame<-reactiveVal(NULL)
      observeEvent(input$framecreatconfirm, {
        ac<-admcheckResult()
        if(ac & input$framecreatconfirm) {
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
          data.table::setorderv(points_dt, c("ea", "long", "lat"))
          # return
          dt_frame(points_dt)
        }
      })

      # overview table for the frame
      output$framesummary<-DT::renderDT({
        dtframe<-req(dt_frame())
        # summary: N ea, N total, Mean EA, Min EA, Max EA
        dtframegr<-dtframe[,.(ean=.N), by=.(ea)]
        tab<-data.table::data.table(
          var=c("Number of Segments", "Total Frame Population", "Average population per Segment",
                "Minimum Population per Segment", "Maximum Population per Segment"),
          val=round(c(dplyr::n_distinct(dtframe$ea), nrow(dtframe), mean(dtframegr$ean), min(dtframegr$ean), max(dtframegr$ean)))
        )

        tab<-DT::datatable(tab,
                           list(dom="t"),
                           selection = "none",
                           rownames = F,
                           colnames = c("", ""),
                           style = "bootstrap")
        # enable header
        shinyjs::show("headframe")
        return(tab)

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

      # create sample summary table
      output$samplesummary<-DT::renderDT({
        dtframe<-req(sampleData())
        # summary: N ea, N total, Mean EA, Min EA, Max EA
        tab<-data.table::data.table(
          var=c("Number of Segments", "Total Sample Population", "Average Sampling Weight",
                "Minimum Sampling Weight", "Maximum Sampling Weight", "Sampling Weight CV"),
          val=round(c((dplyr::n_distinct(dtframe$ea)),
                      nrow(dtframe),
                      mean(dtframe$wgt),
                      min(dtframe$wgt),
                      max(dtframe$wgt),
                      stats::sd(dtframe$wgt)/mean(dtframe$wgt)), 3)
        )

        tab<-DT::datatable(tab,
                           list(dom="t"),
                           selection = "none",
                           rownames = F,
                           colnames = c("", ""),
                           style = "bootstrap")
        # enable header
        shinyjs::show("headsample")
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
