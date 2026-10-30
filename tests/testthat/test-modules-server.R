test_that("zipFileInput_server parses CSV and zipped CSV uploads", {
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  dt_in <- data.table::data.table(ea = c("EA1", "EA2"), long = c(10.1, 10.2), lat = c(50.1, 50.2))
  data.table::fwrite(dt_in, tmp_csv)

  shiny::testServer(
    zipFileInput_server,
    args = list(sep = ",", zipInput = FALSE),
    {
      session$setInputs(frame_file = list(datapath = tmp_csv, name = "test.csv"))
      res <- session$returned()
      expect_s3_class(res, "data.table")
      expect_equal(nrow(res), 2L)
      expect_equal(res$ea, c("EA1", "EA2"))
    }
  )

  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)
  zip::zip(zipfile = tmp_zip, files = tmp_csv, mode = "cherry-pick")

  shiny::testServer(
    zipFileInput_server,
    args = list(sep = ",", zipInput = TRUE),
    {
      session$setInputs(frame_file = list(datapath = tmp_zip, name = "test.zip"))
      res <- session$returned()
      expect_s3_class(res, "data.table")
      expect_equal(nrow(res), 2L)
    }
  )
})

test_that("suso_map_idSRV maps local sample variables to Survey Solutions ID variables", {
  loc_vars <- shiny::reactiveVal(
    data.table::data.table(
      VariableName = c("ea", "area_id", "siteID"),
      type = c("character", "character", "character")
    )
  )
  suso_vars <- shiny::reactiveVal(
    data.table::data.table(
      VariableName = c("id_ea", "id_building"),
      type = c("TextQuestion", "TextQuestion")
    )
  )

  shiny::testServer(
    suso_map_idSRV,
    args = list(varsLocal = loc_vars, varsSuso = suso_vars),
    {
      session$setInputs(
        select_1 = "ea",
        select2_1 = "id_ea",
        select_2 = "siteID",
        select2_2 = "id_building",
        select_3 = "",
        select2_3 = "",
        submit = 1L
      )
      mapped <- session$returned()
      expect_s3_class(mapped, "data.table")
      expect_equal(nrow(mapped), 2L)
      expect_equal(mapped$locvars, c("ea", "siteID"))
      expect_equal(mapped$idvars, c("id_ea", "id_building"))
    }
  )
})

test_that("modal_spsample2stage_server draws balanced and random 2nd-stage samples", {
  set.seed(42)
  frame_dt <- data.table::data.table(
    ea = c(rep("EA_1", 15), rep("EA_2", 5)),
    area_id = c(rep("1", 15), rep("2", 5)),
    long = stats::runif(20, 10.0, 10.05),
    lat = stats::runif(20, 50.0, 50.05)
  )
  frame_rx <- shiny::reactiveVal(frame_dt)

  shiny::testServer(
    modal_spsample2stage_server,
    args = list(
      framepoints_sf = frame_rx,
      patternlat = "lat",
      patternlong = "long",
      inputMode = "app"
    ),
    {
      session$setInputs(
        clustervar = "ea",
        sampSEED = 1234,
        samplemode = "Balanced",
        n_sample = 8,
        mindist = 10
      )
      session$setInputs(createSample = 1L)
      samp_sf <- session$returned()
      expect_s3_class(samp_sf, "sf")
      # EA_1 gets 8 units, EA_2 gets all 5 units (capped at cluster size) = 13 total
      expect_equal(nrow(samp_sf), 13L)
      expect_true(all(c("lon_WGS84", "lat_WGS84", "wgt", "siteID") %in% names(samp_sf)))

      # Test Random mode as well
      session$setInputs(samplemode = "Random", createSample = 2L)
      samp_rand <- session$returned()
      expect_s3_class(samp_rand, "sf")
      expect_equal(nrow(samp_rand), 13L)
    }
  )
})

test_that("modal_adminspsample_server verifies admin credentials", {
  old_admuser <- getOption("admuser")
  old_admpass <- getOption("admpass")
  on.exit({
    options(admuser = old_admuser, admpass = old_admpass)
  }, add = TRUE)
  options(admuser = "test_admin", admpass = "test_pass")

  side_rx <- shiny::reactiveVal("Controls")
  shiny::testServer(
    modal_adminspsample_server,
    args = list(side = side_rx),
    {
      expect_false(session$returned$admcheckok())
      session$setInputs(
        admuser = "wrong_user",
        admpass = "wrong_pass",
        admincheck = TRUE
      )
      expect_false(session$returned$admcheckok())

      session$setInputs(
        admuser = "test_admin",
        admpass = "test_pass",
        admincheck = TRUE
      )
      expect_true(session$returned$admcheckok())
    }
  )
})
