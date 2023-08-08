#' Shiny UI module for Survey Solutions Assignment Admin
#'
#'
#' @description Requires a points dataframe as input, as well as Survey
#' Solutions credentials.Retrieves id variables and allows mapping them to
#' dataframe variables, to subsequently create assignment file at the TEAM
#' level.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList



mapadminUI_st2<-function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::useShinydashboard(),
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    waiter::use_waiter(),
    fluidRow(
      shinydashboard::box(width = 6, status = "success", height = "35vh",
                          solidHeader = T, background = NULL,
                          ##    SuSo API
                          title = "Survey Solutions API Settings",
                          div(id = ns("susosettings"),
                              fluidRow(
                                column(6,
                                       textInput(ns("susoServer"), "Survey Solutions Server",
                                                 placeholder = "Provide Server")
                                ),
                                column(6,
                                       textInput(ns("susoWS"), "Workspace",
                                                 placeholder = "Provide User" )
                                )),
                              fluidRow(
                                column(6,
                                       textInput(ns("susoUser"), "API user",
                                                 placeholder = "Provide User" )
                                ),
                                column(6,
                                       passwordInput(ns("susoPass"), "API password",
                                                     placeholder = "Provide Pass")
                                )
                              ),
                              fluidRow(
                                column(2),
                                column(8,
                                       actionButton(ns("checkSuso"), "Check Server Settings",
                                                    icon("check"),
                                                    width = "100%",
                                                    style="color: #fff;
                                                              background-color: #337ab7;
                                                              border-color: #337ab7;
                                                                margin: 0 0% 0 0%;")
                                ),
                                column(2)
                              )
                          ), br(),
                          div(id = "serversetreset",
                              fluidRow(
                                column(1),
                                column(10,
                                       DT::dataTableOutput(ns("apiSummary"), height = "10vh")
                                ),
                                column(1)
                              ),br(),
                              fluidRow(
                                column(2),
                                column(8,
                                       shinyjs::hidden(
                                         actionButton(ns("resetSuso"), "Reset Server Settings",
                                                      icon("ban"),
                                                      width = "100%",
                                                      style="color: #FFFFFF;background-color: #7f0000;border-color: #7f0000")
                                       )
                                ),
                                column(2)
                              )
                          )
      ),
      shinydashboard::box(width = 6, status = "success",height = "35vh",
                          solidHeader = T, background = NULL,
                          ##    UPLOAD BOX
                          title = "ID variable selection",
                          h4("Select variables for preloading",
                             align="center",
                             style="font-weight:bold;color:#0d47a1; margin:0 0 0 0;"),

                          helpText("All selected variables must be present in the dataset.",
                                   style = "text-align: center;"),

                          fluidRow(column(3),
                                   column(6,
                                          suso_map_idUI(ns("dtModule"))
                                   ),
                                   column(3)
                          ),br(),
                          h4("Survey Solutions Upload File Mapping",
                             align="center",
                             style="font-weight:bold;color:#0d47a1; margin:0 0 0 0;"),
                          helpText("This is the mapping of the variables in the sample to the identifier variables in your questionnaire,
                                   based on your selection.",
                                   style = "text-align: center;"),
                          fluidRow(column(2),
                                   column(8,
                                          suso_map_idUI_maptable(ns("dtModule"), height = "5vh")
                                          ),
                                   column(2)
                          )
      )
    ),
    fluidRow(
      shinydashboard::box(width = 12, status = "success",
                          solidHeader = T, background = NULL, collapsible = T, collapsed = T,
                          ##    Maps on server
                          title = "Team Assignment",
                          fluidRow(
                            column(4),
                            column(4),
                            column(4)
                          ),
                          fluidRow(
                            column(6,
                                   DT::dataTableOutput(ns("assignTable"),  height = "28vh")
                            ),
                            column(4,
                                   DT::dataTableOutput(ns("teamTable"),  height = "28vh")
                            ),
                            column(2,
                                   actionButton(ns("suso_assignPoints"), "Create Assignments", width = "100%",
                                                icon("upload"),
                                                style="color: #FFFFFF;
                                                     background-color: #1976D2;
                                                     border-color: #1976D2")
                            )
                          )
      )
    )
  )
  ####################FIN UI####################################################
}

#' Shiny UI module for Survey Solutions Map Admin
#'
#'
#' @description ui for assignment creation log, placed in different tab
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mapadminUI2_st2<-function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2),
      column(8,
             shinyjs::hidden(
               div(id = ns("headquest"),
                   h4("Available Questionnaire",
                      style = "color: #0d47a1;text-align: center !important;")
               )
             )
      ),
      column(2)
    ),
    fluidRow(
      column(1),
      column(10,
             DT::dataTableOutput(ns("qTableIn"), width = "10vw")
      ),
      column(1)
    ), br(),
    fluidRow(
      column(2),
      column(8,
             shinyjs::hidden(
               div(id = ns("headass"),
                   h4("Assignment Overview",
                      style = "color: #0d47a1;text-align: center !important;")
               )
             )
      ),
      column(2)
    ),
    fluidRow(
      column(1),
      column(10,
             DT::dataTableOutput(ns("assignoverviewTable"), width = "23vw")
      ),
      column(1)
    )
  )
}



#' Shiny server module for Survey Solutions Map Admin
#'
#'
#' @importFrom shiny NS tagList
#' @noRd
#'
mapadminSRV_st2 <- function(id, pointsfile=reactive({NULL})) {
  moduleServer(
    id,
    function(input, output, session) {

      # table specificaitons
      ##  2. Info table (no selection, first column is Names)
      smTab<-list(dom="t")

      infoTable<-.%>% formatStyle(1,  color = '#FFFFFF',
                                  backgroundColor = '#0d47a1',
                                  fontWeight = 'bold')

      inputTable<-.%>% formatStyle(2,
                                   fontWeight = 'bold',
                                   textAlign = 'center')


      ##  3. View table (no selcetion, all columns the same)
      viewTable<-.%>% formatStyle(1,  color = '#FFFFFF',
                                  backgroundColor = '#33D2FF',
                                  fontWeight = 'bold')

      #########################################
      ## CSS/UI Styles
      styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")
      smTabDir<-list(dom="t", pagelength=500, scrollY="250px", scrollcollapse=TRUE, paging=FALSE)

      action_btn_close <-c("color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1")
      styleActButton<-c("color: #FFFFFF; background-color: #7f0000;
                  border-color: #7f0000; margin:0 20% 0 20%;")

      # API BASE Set-up
      apiCheck<-reactiveVal(NULL); susoServer<-reactiveVal(NULL); susoWS<-reactiveVal(NULL)
      # Check Credentials
      observeEvent(input$checkSuso, {
        # set credentials
        SurveySolutionsAPI::suso_set_key(input$susoServer, input$susoUser, input$susoPass)
        checkAPI<-SurveySolutionsAPI::suso_PwCheck(workspace = input$susoWS)
        if(checkAPI$status_code==200) {
          apiCheck(checkAPI$status_code)
          susoServer(input$susoServer)
          susoWS(input$susoWS)
          # Disable Settings
          shinyjs::disable("susosettings")
          # Enable upload
          # Show Reset
          shinyjs::show("resetSuso")
        } else {
          shiny::showNotification(
            "Survey Solutions Credentials not valid! Please correct.",
            type = "warning"
          )
        }
      }, ignoreInit = T)

      # enable shape upload only when file is available
      observe({
        shiny::validate(need(apiCheck()==200, message = F))
        req(pointsfile())
        shinyjs::enable("suso_idvars")
      })

      # Reset API connection
      observeEvent(input$resetSuso, {
        shiny::validate(need(apiCheck()==200, message = F))
        # Enable settings
        shinyjs::enable("susosettings")
        # Disable Upload
        shinyjs::disable("suso_idvars")
        # update inputs
        updateTextInput(session = session, ("susoServer"), "Survey Solutions Server",
                        placeholder = "Provide Server", value = "")
        updateTextInput(session = session,("susoWS"), "Workspace",
                        placeholder = "Provide User", value = "")
        updateTextInput(session = session,("susoUser"), "API user",
                        placeholder = "Provide User", value = "")
        updateTextInput(session = session, ("susoPass"), "API password",
                        placeholder = "Provide Pass", value = "")
        # hide reset
        shinyjs::hide("resetSuso")
        # set api check to NULL
        apiCheck(NULL)

      }, ignoreInit = T)

      # Table API Check Summary
      output$apiSummary<-DT::renderDataTable({
        shiny::validate(need(apiCheck()==200, message = F))
        tab<-cbind(
          c("API Credentials", "Server", "Workspace"),
          c("Valid", susoServer(), susoWS())
        )
        DT::datatable(tab, smTab, selection = "none", rownames = F,
                      colnames = c("",""),
                      style = "bootstrap") %>% infoTable
      })


      # Table Questionnaire
      qData<-reactive({
        shiny::validate(need(apiCheck()==200, message = F))
        tab<-SurveySolutionsAPI::suso_getQuestDetails(workspace = input$susoWS)

        if(length(tab)==1 | is.null(tab)) {
          showNotification("No questionnaire loaded on the provided server/workspace. Please import questionnaires first!", type = "error")
          req(FALSE)
        } else {
          tab<-data.table::data.table(tab,
                                      key = c("Title", "Version"))
        }



        tab[,c("date", "time"):=data.table::tstrsplit(LastEntryDate, "T", fixed=TRUE)][]
        tab[,time:=as.ITime(time)]
        tab[,date:=as.IDate(date)]
        tab[,QuestionnaireIdVersion:=sprintf("%s_%d", QuestionnaireId, Version)]
        setorderv(tab, c("date", "time"), -1)
        return(tab)
      })
      qDataOut<-reactiveVal(NULL)
      ###################################################################
      ## Questionnaire for Incoming Variables
      questIdIn<-reactiveVal(NULL); questFullListIn<-reactiveVal(NULL)
      questIDselIn<-reactiveVal(NULL); questV_selIn<-reactiveVal(NULL)
      output$qTableIn <- DT::renderDataTable({
        req(qData())
        tab<-qData()
        questIdIn(tab[,.(QuestionnaireId, Version, Title)])
        tab<-tab[,.(Title, Version, QuestionnaireId, LastEntryDate)]
        ## Export
        #questFullListIn(tab)
        tab<-DT::datatable(tab[,.(Title, Version)], list(dom="tp"), selection = "single",  rownames = F,
                           style = "bootstrap")
        # enable header
        shinyjs::show("headquest")
        return(tab)
      })
      IDVARS<-reactiveVal(NULL); LOCVARS<-reactiveVal(NULL)
      GPSVAR<-reactiveVal(NULL)
      observeEvent(input$qTableIn_rows_selected, {
        sel<-input$qTableIn_rows_selected
        waiter::waiter_show(
          color = "rgba(13, 71, 161, 0.7)",
          html = tagList(
            spin_fading_circles(),
            "Loading Questionnaire ..."
          )
        )
        iddata<-req(qData())
        qid<-(iddata[sel, QuestionnaireId])
        v<-(iddata[sel, Version])
        # get the questions
        qestStruct<-SurveySolutionsAPI::suso_getQuestDetails(workspace = input$susoWS,
                                                             quid = qid,
                                                             version = v,
                                                             operation.type = "structure")
        # take id vars
        idvars<-qestStruct$q[Featured==T & type!="GpsCoordinateQuestion", .(VariableName, type)]
        # take geo vars
        gpsidvar<-qestStruct$q[Featured==T & type=="GpsCoordinateQuestion", VariableName]
        shinyjs::enable("suso_idvars")
        IDVARS(idvars)

        # get local variables with type
        locvars<-data.table(VariableName = names(pointsfile()), type=sapply(pointsfile(), class))
        locvars<-locvars[!(VariableName %in% c("lon_WGS84", "lat_WGS84"))]
        LOCVARS(locvars)

        # get GPS var
        GPSVAR(gpsidvar)

        waiter::waiter_hide()
        #
      }, ignoreInit = T)


      # API queries
      # query teams
      teamtablequery<-reactive({
        shiny::validate(need(apiCheck()==200, message = F))
        tab<-SurveySolutionsAPI::suso_getSV(workspace = input$susoWS)
        return(tab)
      })

      # Map-to-User tables
      # Map Table
      output$assignTable<-DT::renderDataTable({
        tab<-pointsfile()
        req(tab)
        if(!data.table::is.data.table(tab)) tab<-data.table(tab)
        tab<-tab[,.(ea, area_id, siteID)] #, size, importDateUtc, wkid)]
        datatable(tab, escape = F, rownames = F, selection = "multiple",
                  options = list(pagelength=500,
                                 scrollY="500px",
                                 scrollX="300px",
                                 scrollcollapse=TRUE,
                                 paging=FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = c(0:2),
                                                        width = '20%', targets = c(0),
                                                        width = '10%', targets = c(1),
                                                        width = '5%', targets = c(2)
                                 ))))
      })

      assingTabProxy<-dataTableProxy(
        "assignTable",
        session = shiny::getDefaultReactiveDomain(),
        deferUntilFlush = TRUE
      )

      # Teams table
      output$teamTable<-DT::renderDataTable({
        tab<-teamtablequery()
        req(tab)
        shiny::validate(need(nrow(tab)>0, message = "No supervisor created!"))
        tab<-tab[,.(UserName, IsLocked)]
        datatable(tab, escape = F, rownames = F, selection = "single",
                  options = list(pagelength=500,
                                 scrollY="500px",
                                 scrollcollapse=TRUE,
                                 paging=FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = c(0:1),
                                                        width = '20%', targets = c(0),
                                                        width = '10%', targets = c(1)
                                 ))))
      })

      observeEvent(input$select_button,{
        shinyalert::shinyalert(paste("Attention!"),
                               paste0("Are you sure you want to reset the status of this Interview?\n", "ID:", input$select_button,
                                      "\nIf yes, please re-type the ID to confirm."),
                               type="input", inputId = "confirmReset", inputType = text,
                               closeOnEsc=T, closeOnClickOutside=T, showCancelButton=T, showConfirmButton=T)
      })

      #   MAP ASSIGNMENT & LOG
      toassingoverview<-reactiveVal(data.table(Team=character(0), AssignmentCount=character(0)))
      observeEvent(input$suso_assignPoints,{
        tabMAP<-pointsfile()
        tabSV<-teamtablequery()
        fullmap<-FULLMAP()
        req(tabMAP, tabSV, fullmap)
        shiny::validate(need(nrow(tabMAP)>0 & nrow(tabSV)>0, message = F))
        mapsel<-(tabMAP[input$assignTable_rows_selected])
        svsel<-(tabSV[input$teamTable_rows_selected])
        # get existing log
        mapsvselall<-toassingoverview()
        # Correct possible side effect from data.table
        if(nrow(mapsvselall)>0) mapsvselall[,Reset:=NULL]
        # add new rows
        mapsvsel<-data.table(Team=svsel$UserName, AssignmentCount=nrow(mapsel))
        mapsvselall<-data.table::rbindlist(list(mapsvselall, mapsvsel))
        # send to proxy for dt update
        toassingoverview(data.table::copy(mapsvselall))
        # Reset row selection in DT
        DT::selectRows(assingTabProxy, selected = NULL)

        # ASSIGNMENT CREATION

        # subset to mapped variables
        mapsel<-mapsel[,c(fullmap$locvars, "lat_WGS84", "lon_WGS84"), with=F]
        AAout<-data.table(mapsel,
                          ResponsibleName=svsel$UserName,
                          Quantity=1,
                          gpsstring=paste0(round(mapsel$lat_WGS84, 8), "$", round(mapsel$lon_WGS84, 8)))
        gpsvar<-req(GPSVAR())
        # rename
        for(i in fullmap$locvars) {
          idn<-fullmap[locvars==i, idvars]
          data.table::setnames(AAout, i, idn)
        }
        data.table::setnames(AAout, "gpsstring", gpsvar); AAout[, c("lon_WGS84", "lat_WGS84"):=NULL]
        ###############################################
        if(nrow(AAout)>0) {
          iddata<-req(qData())
          sel<-input$qTableIn_rows_selected
          qid<-(iddata[sel, QuestionnaireId])
          v<-(iddata[sel, Version])
          status_list<-list()
          for(i in 1:nrow(AAout)) {
            print(i)
            AAsing<-AAout[i]
            status_list[[i]]<-SurveySolutionsAPI::suso_createASS(df=AAsing,
                                                                 QUID = qid,
                                                                 version = v)
          }
        }
      }, ignoreInit = T)

      # Base table
      output$assignoverviewTable<-DT::renderDataTable({
        # create empty table for start and add lines through proxy
        shiny::validate(need(apiCheck()==200, message = F))
        # tab<-data.table(Team=character(0), Maps=character(0))
        tab<-toassingoverview()
        req(nrow(tab)>0)
        # add reset button
        ns<-NS(id)
        tab[, Reset:=shinyInput(actionButton,
                                nrow(tab),
                                tab$Team,
                                label = "Reset",
                                style=styleActButton,
                                onclick = paste0('Shiny.onInputChange(\"', session$ns("reset_button"), '\",  this.id)' ))]

        tab<-datatable(tab, escape = F, rownames = F, selection = "single",
                  options = list(pagelength=500,
                                 scrollY="500px",
                                 scrollcollapse=TRUE,
                                 paging=FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = c(0:2),
                                                        width = '20%', targets = c(0),
                                                        width = '10%', targets = c(1),
                                                        width = '5%', targets = c(2)
                                 ))))
        # enable header
        shinyjs::show("headass")
        return(tab)
      })

      # Reset Map assignment
      observeEvent(input$reset_button,{
        ns<-NS(id)
        shinyalert::shinyalert(
          type = "error",
          title = paste("Attention!"),
          html = T,
          text = tagList(
            HTML(
              paste0("Are you sure you want to reset the team assignment for\n", "ID:
                      <br><center><font color='#0d47a1'><big>",
                     input$reset_button,
                     "</big></font></center><br>\nIf yes,
                     please re-type the ID to confirm.")
            ),
            textInput(ns("confirmResetId"), "")
          ),
          inputId = "confirmReset",
          inputType = text,
          closeOnEsc=T,
          closeOnClickOutside=T,
          showCancelButton=T,
          showConfirmButton=T,
          animation = "slide-from-top"
        )
      })
      observeEvent(session$ns(input$confirmReset), {
        mapreset<-(input$confirmResetId)
        # get existing log
        mapsvselall<-toassingoverview()
        # subset
        mapsvselall<-mapsvselall[Team!=mapreset,]
        toassingoverview(mapsvselall)
      }, ignoreInit = T)

      # Modal for Mapping -->MODULE
      FULLMAP<-suso_map_idSRV("dtModule", varsLocal = LOCVARS, varsSuso = IDVARS)


      ####################FIN SERVER####################################################
      # return list
      list(
        apiCheck=apiCheck,
        susoServer=susoServer,
        susoWS=susoWS
      )

    }
  )
}

# Helper Function
## Function for creation of DT input button in table cell
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id[i]), ...))
  }
  inputs
}

