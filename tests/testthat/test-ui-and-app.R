test_that("main_ui builds without startup warnings for both google and vantor providers", {
  old_opt <- getOption("susolisting.provider")
  on.exit(options(susolisting.provider = old_opt), add = TRUE)

  options(susolisting.provider = "google")
  expect_no_warning({
    ui_google <- main_ui(NULL)
  })
  expect_s3_class(ui_google, "shiny.tag")
  ui_google_html <- as.character(ui_google)
  expect_match(ui_google_html, "JDC - WB Listing Application")
  expect_match(ui_google_html, "Google Map")
  expect_match(ui_google_html, "detailSampMap")
  expect_match(ui_google_html, "add_suso_drawing")

  options(susolisting.provider = "vantor")
  expect_no_warning({
    ui_vantor <- main_ui(NULL)
  })
  expect_s3_class(ui_vantor, "shiny.tag")
  ui_vantor_html <- as.character(ui_vantor)
  expect_match(ui_vantor_html, "Vantor Map")
  expect_match(ui_vantor_html, "leaflet")
  expect_match(ui_vantor_html, "detailSampMap")
})

test_that("runListingAppServer returns a valid shiny.appobj without mutating global options", {
  old_opt <- getOption("susolisting.provider")
  on.exit(options(susolisting.provider = old_opt), add = TRUE)
  options(susolisting.provider = "initial_sentinel")

  expect_no_warning({
    app_g <- runListingAppServer(admuser = "admin", admpass = "secret", provider = "google")
  })
  expect_s3_class(app_g, "shiny.appobj")
  expect_equal(getOption("susolisting.provider"), "initial_sentinel")

  expect_no_warning({
    app_v <- runListingAppServer(admuser = "admin", admpass = "secret", provider = "vantor")
  })
  expect_s3_class(app_v, "shiny.appobj")
  expect_equal(getOption("susolisting.provider"), "initial_sentinel")

  expect_no_warning({
    app_m <- runListingAppServer(admuser = "admin", admpass = "secret", provider = "maxar", n_grid = 15L)
  })
  expect_s3_class(app_m, "shiny.appobj")
  expect_equal(getOption("susolisting.provider"), "initial_sentinel")
  expect_error(runListingAppServer(n_grid = 0L), "`n_grid` must be a positive integer")
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
  expect_s3_class(sampling_report_ui("rep_mod"), "shiny.tag.list")
  expect_s3_class(zipFileInput_ui("zip_mod", "Upload", "text/csv"), "shiny.tag.list")

  btns <- shinyInput(shiny::actionButton, 2, c("btn1", "btn2"), label = "Click")
  expect_type(btns, "character")
  expect_length(btns, 2)
})
