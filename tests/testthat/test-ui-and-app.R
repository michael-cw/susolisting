test_that("main_ui builds without startup warnings (no useShinydashboard or useShinyalert warnings)", {
  expect_no_warning({
    ui <- main_ui(NULL)
  })
  expect_s3_class(ui, "shiny.tag")
  ui_html <- as.character(ui)
  expect_match(ui_html, "JDC - WB Listing Application")
  expect_match(ui_html, "detailSampMap")
  expect_match(ui_html, "add_suso_drawing")
})

test_that("runListingAppServer returns a valid shiny.appobj without warnings", {
  expect_no_warning({
    app <- runListingAppServer(admuser = "admin", admpass = "secret")
  })
  expect_s3_class(app, "shiny.appobj")
})

test_that("UI modules construct valid Shiny tag objects", {
  expect_s3_class(modal_spsample2stage_ui("test_samp", inputMode = "app"), "shiny.tag.list")
  expect_s3_class(modal_spsample2stage_ui("test_samp_up", inputMode = "upload"), "shiny.tag.list")
  expect_error(modal_spsample2stage_ui("bad", inputMode = "invalid"))

  expect_s3_class(modal_adminspsample_ui_tabmain("adm_main"), "shiny.tag.list")
  expect_s3_class(modal_adminspsample_ui_tabside("adm_side"), "shiny.tag.list")

  expect_s3_class(mapadminUI_st2("suso_main"), "shiny.tag.list")
  expect_s3_class(mapadminUI2_st2("suso_side"), "shiny.tag.list")

  expect_s3_class(suso_map_idUI("id_map"), "shiny.tag.list")
  expect_s3_class(suso_map_idUI_maptable("id_map", height = "10vh"), "shiny.tag.list")

  expect_s3_class(dwl_dataUI("dwl_mod"), "shiny.tag.list")
  expect_s3_class(zipFileInput_ui("zip_mod", "Upload", "text/csv"), "shiny.tag.list")

  btns <- shinyInput(shiny::actionButton, 2, c("btn1", "btn2"), label = "Click")
  expect_type(btns, "character")
  expect_length(btns, 2)
})
