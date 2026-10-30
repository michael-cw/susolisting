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

test_that("build_sampling_report_docx and sampling_report_server create a World Bank styled Word report", {
  frame_dt <- data.table::data.table(
    ea = c(rep("EA_1", 15), rep("EA_2", 8)),
    area_id = c(rep("1", 15), rep("2", 8)),
    long = seq(10.01, 10.23, length.out = 23),
    lat = seq(50.01, 50.23, length.out = 23)
  )
  sample_dt <- data.table::data.table(
    siteID = sprintf("Site-%02d", 1:12),
    ea = c(rep("EA_1", 8), rep("EA_2", 4)),
    wgt = c(rep(15 / 8, 8), rep(8 / 4, 4)),
    lon_WGS84 = seq(10.01, 10.20, length.out = 12),
    lat_WGS84 = seq(50.01, 50.20, length.out = 12)
  )
  params <- list(
    clustervar = "ea",
    seed = 4321L,
    samplemode = "Balanced w. minimum distance",
    n_sample = 8L,
    mindist = 15,
    utm_epsg = 32632L,
    timestamp = as.POSIXct("2026-10-30 12:00:00", tz = "UTC")
  )

  tmp_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp_docx), add = TRUE)

  logo_file <- resolve_wbg_logo_path()
  expect_true(nzchar(logo_file))
  expect_true(file.exists(logo_file))

  out_path <- build_sampling_report_docx(
    frame_data = frame_dt,
    sample_data = sample_dt,
    sampling_params = params,
    file = tmp_docx
  )
  expect_equal(out_path, tmp_docx)
  expect_true(file.exists(tmp_docx))
  expect_gt(file.info(tmp_docx)$size, 10000)

  doc_parsed <- officer::read_docx(tmp_docx)
  summary_df <- officer::docx_summary(doc_parsed)
  full_text <- paste(summary_df$text, collapse = "\n")
  expect_match(full_text, "Second-Stage Spatial Sampling Report")
  expect_match(full_text, "1\\. Executive Summary")
  expect_match(full_text, "2\\. Second-Stage Sampling Frame")
  expect_match(full_text, "3\\. Sampling Methodology and Process")
  expect_match(full_text, "4\\. Resulting Second-Stage Sample")
  expect_match(full_text, "4321")
  expect_match(full_text, "15 meters")

  # Verify embedded logo image and plot in Word package media directory
  unpack_dir <- tempfile("docx_unpack_")
  on.exit(unlink(unpack_dir, recursive = TRUE), add = TRUE)
  officer::unpack_folder(file = tmp_docx, folder = unpack_dir)
  media_files <- list.files(file.path(unpack_dir, "word", "media"))
  expect_gte(length(media_files), 2L)

  # Test sampling_report_server module
  frame_rx <- shiny::reactiveVal(frame_dt)
  sample_rx <- shiny::reactiveVal(sample_dt)
  params_rx <- shiny::reactiveVal(params)
  shiny::testServer(
    sampling_report_server,
    args = list(
      frame_data = frame_rx,
      sample_data = sample_rx,
      sampling_params = params_rx,
      file_name = shiny::reactive("UnitTest_SamplingReport")
    ),
    {
      session$setInputs(generateReport = 1L)
      expect_true(!is.null(report_path()))
      expect_true(file.exists(report_path()))
      on.exit(unlink(report_path()), add = TRUE)
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

test_that("main_server renders detailSampMap after startup, shapefile upload, and area_id selection", {
  tmp_dir <- file.path(tempdir(), "test_main_server_flow")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  poly1 <- sf::st_polygon(list(rbind(
    c(10.0, 50.0), c(10.1, 50.0), c(10.1, 50.1), c(10.0, 50.1), c(10.0, 50.0)
  )))
  poly2 <- sf::st_polygon(list(rbind(
    c(10.2, 50.0), c(10.3, 50.0), c(10.3, 50.1), c(10.2, 50.1), c(10.2, 50.0)
  )))
  poly3 <- sf::st_polygon(list(rbind(
    c(10.4, 50.0), c(10.5, 50.0), c(10.5, 50.1), c(10.4, 50.1), c(10.4, 50.0)
  )))
  sfc <- sf::st_sfc(poly1, poly2, poly3, crs = 4326)
  shp_in <- sf::st_sf(ea_code = c("EA_01", "EA_02", "EA_03"), geometry = sfc)
  shp_path <- file.path(tmp_dir, "bounds.shp")
  sf::st_write(shp_in, shp_path, quiet = TRUE)

  shp_files <- list.files(tmp_dir, full.names = TRUE)
  zip_path <- file.path(tempdir(), "test_bounds_flow.zip")
  on.exit(unlink(zip_path), add = TRUE)
  zip::zip(zipfile = zip_path, files = shp_files, mode = "cherry-pick")

  shiny::testServer(main_server, {
    # 1. Startup login (before file upload)
    session$setInputs(mapkey = "fake_google_key", user = "unit_tester", startup = TRUE)
    expect_error(output$detailSampMap, class = "shiny.silent.error")

    # 2. Upload boundary shapefile and select identification variable
    session$setInputs(new_shape = list(datapath = zip_path, name = "test_bounds_flow.zip"))
    session$setInputs(area_id = "ea_code")

    # 3. Map must now render with polygons and custom marker drawing handler
    map_json <- output$detailSampMap
    expect_type(map_json, "character")
    expect_match(map_json, "add_polygons")
    expect_match(map_json, "add_suso_drawing")

    # 4. Simulate placing a building marker on EA_01
    session$setInputs(
      detailSampMap_markercomplete = list(
        position = list(lat = 50.05, lng = 10.05),
        randomValue = 0.5
      )
    )
    expect_equal(nrow(pointsCollectorGoogle()), 1L)
    expect_equal(pointsCollectorGoogle()$area_id, "EA_01")

    # 5. Click Next EA to persist marker and advance to EA_02
    session$setInputs(loadNextEA = 1L)
    expect_equal(counter(), 2L)
    expect_equal(lgaSel()$ea_code, "EA_02")
    expect_equal(nrow(pointsCollectorGoogle()), 0L)

    # 6. Place marker on EA_02 and advance to EA_03
    session$setInputs(
      detailSampMap_markercomplete = list(
        position = list(lat = 50.05, lng = 10.25),
        randomValue = 0.6
      )
    )
    expect_equal(nrow(pointsCollectorGoogle()), 1L)
    expect_equal(pointsCollectorGoogle()$area_id, "EA_02")
    session$setInputs(loadNextEA = 2L)
    expect_equal(counter(), 3L)
    expect_equal(lgaSel()$ea_code, "EA_03")
    expect_true(all(c("EA_01", "EA_02") %in% savedFiles()))
    on.exit(unlink(fp(), recursive = TRUE), add = TRUE)
  })
})

test_that("main_server supports Vantor (Maxar) Leaflet map provider and marker collection", {
  old_opt <- getOption("susolisting.provider")
  on.exit({
    options(susolisting.provider = old_opt)
    vantorr::vantor_auth_clear(verbose = FALSE)
  }, add = TRUE)
  options(susolisting.provider = "vantor")

  tmp_dir <- file.path(tempdir(), "test_vantor_server_flow")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  poly1 <- sf::st_polygon(list(rbind(
    c(10.0, 50.0), c(10.1, 50.0), c(10.1, 50.1), c(10.0, 50.1), c(10.0, 50.0)
  )))
  poly2 <- sf::st_polygon(list(rbind(
    c(10.2, 50.0), c(10.3, 50.0), c(10.3, 50.1), c(10.2, 50.1), c(10.2, 50.0)
  )))
  sfc <- sf::st_sfc(poly1, poly2, crs = 4326)
  shp_in <- sf::st_sf(ea_code = c("EA_V1", "EA_V2"), geometry = sfc)
  shp_path <- file.path(tmp_dir, "bounds_vantor.shp")
  sf::st_write(shp_in, shp_path, quiet = TRUE)

  shp_files <- list.files(tmp_dir, full.names = TRUE)
  zip_path <- file.path(tempdir(), "test_bounds_vantor.zip")
  on.exit(unlink(zip_path), add = TRUE)
  zip::zip(zipfile = zip_path, files = shp_files, mode = "cherry-pick")

  shiny::testServer(main_server, {
    # 1. Startup login with Vantor API key and product
    session$setInputs(
      mapkey = "fake_vantor_api_key",
      vantor_product = "VIVID_ADVANCED_15",
      user = "vantor_tester",
      startup = TRUE
    )
    expect_true(vantorr::vantor_has_auth(verbose = FALSE))
    expect_equal(vantorProduct(), "VIVID_ADVANCED_15")

    # 2. Upload boundary shapefile and select identification variable
    session$setInputs(new_shape = list(datapath = zip_path, name = "test_bounds_vantor.zip"))
    session$setInputs(area_id = "ea_code")

    # 3. Leaflet map must render with Vantor WMTS tiles, seamlines, bounds polygon, and drawing hook
    map_json <- output$detailSampMap
    expect_type(map_json, "character")
    expect_match(map_json, "api\\.maxar\\.com/basemaps/v1/ogc/gwc/service/wmts")
    expect_match(map_json, "VIVID_ADVANCED_15")
    expect_match(map_json, "addPolygons")
    expect_match(map_json, "add_suso_drawing")

    # 4. Simulate placing a building marker on EA_V1 and advancing to EA_V2
    session$setInputs(
      detailSampMap_markercomplete = list(
        position = list(lat = 50.05, lng = 10.05),
        randomValue = 0.7
      )
    )
    expect_equal(nrow(pointsCollectorGoogle()), 1L)
    expect_equal(pointsCollectorGoogle()$area_id, "EA_V1")

    session$setInputs(loadNextEA = 1L)
    expect_equal(counter(), 2L)
    expect_equal(lgaSel()$ea_code, "EA_V2")
    expect_true("EA_V1" %in% savedFiles())
    on.exit(unlink(fp(), recursive = TRUE), add = TRUE)
  })
})

