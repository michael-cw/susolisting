test_that("sanitize_string cleans punctuation, whitespace, and non-ASCII characters", {
  expect_equal(sanitize_string("John Doe!@# 123"), "John_Doe_123")
  expect_equal(sanitize_string("  multiple   spaces  "), "multiple_spaces")
  expect_equal(sanitize_string("user-name/test", repl_for_whitespace = "-"), "user-name-test")
  expect_equal(sanitize_string("already_clean_01"), "already_clean_01")
})

test_that("project_to_utm transforms to valid UTM EPSG codes including single-digit zones", {
  # Northern Hemisphere, Zone 32 (10 deg E, 50 deg N -> EPSG:32632)
  pts_nh32 <- sf::st_as_sf(
    data.frame(id = 1:2, lon = c(10.0, 10.1), lat = c(50.0, 50.1)),
    coords = c("lon", "lat"),
    crs = 4326
  )
  utm_nh32 <- project_to_utm(pts_nh32)
  expect_equal(sf::st_crs(utm_nh32)$epsg, 32632L)

  # Northern Hemisphere, single-digit Zone 5 (-153 deg W, 20 deg N -> EPSG:32605)
  pts_nh05 <- sf::st_as_sf(
    data.frame(id = 1:2, lon = c(-153.5, -153.4), lat = c(20.0, 20.1)),
    coords = c("lon", "lat"),
    crs = 4326
  )
  utm_nh05 <- project_to_utm(pts_nh05)
  expect_equal(sf::st_crs(utm_nh05)$epsg, 32605L)

  # Southern Hemisphere, single-digit Zone 5 (-153 deg W, -20 deg S -> EPSG:32705)
  pts_sh05 <- sf::st_as_sf(
    data.frame(id = 1:2, lon = c(-153.5, -153.4), lat = c(-20.1, -20.0)),
    coords = c("lon", "lat"),
    crs = 4326
  )
  utm_sh05 <- project_to_utm(pts_sh05)
  expect_equal(sf::st_crs(utm_sh05)$epsg, 32705L)
})

test_that("shapeLoad2 loads a shapefile and transforms to EPSG:4326", {
  tmp_dir <- file.path(tempdir(), "test_shp_load")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  poly <- sf::st_polygon(list(rbind(
    c(10, 50),
    c(10.1, 50),
    c(10.1, 50.1),
    c(10, 50.1),
    c(10, 50)
  )))
  sfc <- sf::st_sfc(poly, crs = 4326)
  shp_in <- sf::st_sf(area_id = "EA_01", geometry = sfc)
  # Project to Web Mercator (3857) before saving to test transformation back to 4326
  shp_3857 <- sf::st_transform(shp_in, 3857)
  shp_path <- file.path(tmp_dir, "test_bounds.shp")
  sf::st_write(shp_3857, shp_path, quiet = TRUE)

  loaded <- shapeLoad2(path = shp_path)
  expect_type(loaded, "list")
  expect_length(loaded, 2)
  expect_s3_class(loaded[[2]], "sf")
  expect_true(sf::st_is_longlat(loaded[[2]]))
  expect_equal(nrow(loaded[[2]]), 1L)
  expect_equal(loaded[[2]]$area_id, "EA_01")
})

test_that("resolve_map_provider normalizes google, vantor, and maxar providers", {
  old_opt <- getOption("susolisting.provider")
  on.exit(options(susolisting.provider = old_opt), add = TRUE)

  options(susolisting.provider = NULL)
  expect_equal(resolve_map_provider(), "google")
  expect_equal(resolve_map_provider("google"), "google")
  expect_equal(resolve_map_provider("vantor"), "vantor")
  expect_equal(resolve_map_provider("MAXAR"), "vantor")

  options(susolisting.provider = "vantor")
  expect_equal(resolve_map_provider(), "vantor")

  expect_error(resolve_map_provider("bing"), "`provider` must be one of")
})

test_that("add_marker_drawing and clear_marker_drawing work on google_map and leaflet objects", {
  m <- googleway::google_map(
    key = "dummy_key",
    libraries = c("visualization", "geometry", "places")
  )
  m_draw <- add_marker_drawing(m)
  expect_s3_class(m_draw, "google_map")
  fn_names <- vapply(m_draw$x$calls, function(c) c$functions, character(1))
  expect_true("add_suso_drawing" %in% fn_names)

  m_clear <- clear_marker_drawing(m_draw)
  fn_names2 <- vapply(m_clear$x$calls, function(c) c$functions, character(1))
  expect_true("clear_suso_drawing" %in% fn_names2)

  lf <- leaflet::leaflet()
  lf_draw <- add_marker_drawing(lf)
  expect_s3_class(lf_draw, "leaflet")
  lf_methods <- vapply(lf_draw$x$calls, function(c) c$method, character(1))
  expect_true("add_suso_drawing" %in% lf_methods)

  lf_clear <- clear_marker_drawing(lf_draw)
  lf_methods2 <- vapply(lf_clear$x$calls, function(c) c$method, character(1))
  expect_true("clear_suso_drawing" %in% lf_methods2)

  expect_error(add_marker_drawing(list()), "`map` must be a `google_map`")
  expect_error(clear_marker_drawing(list()), "`map` must be a `google_map`")

  js_tag <- suso_drawing_js()
  expect_s3_class(js_tag, "shiny.tag")
  expect_match(as.character(js_tag), "add_suso_drawing")
  expect_match(as.character(js_tag), "clear_suso_drawing")
  expect_match(as.character(js_tag), "ensureShinyOutputClasses")
  expect_match(as.character(js_tag), "installLeafletHooks")
})

test_that("calculate_polygon_coverage and summarize_cluster_coverage compute grid coverage and flag low coverage", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(10.0, 50.0),
      c(10.1, 50.0),
      c(10.1, 50.1),
      c(10.0, 50.1),
      c(10.0, 50.0)
    ))),
    crs = 4326
  )

  # Empty points -> 0% and flagged LOW (default n_grid = 10 -> 100 cells)
  cov_empty <- calculate_polygon_coverage(poly, data.frame(long = numeric(0), lat = numeric(0)))
  expect_equal(cov_empty$coverage_pct, 0)
  expect_equal(cov_empty$n_cells_total, 100L)
  expect_true(cov_empty$low_coverage)
  expect_equal(cov_empty$coverage_status, "LOW")

  # Sparse points in one corner -> < 20% and flagged LOW
  pts_sparse <- data.frame(long = c(10.005, 10.008), lat = c(50.005, 50.008))
  cov_sparse <- calculate_polygon_coverage(poly, pts_sparse, n_grid = 10L, low_threshold = 20)
  expect_gt(cov_sparse$coverage_pct, 0)
  expect_lt(cov_sparse$coverage_pct, 20)
  expect_true(cov_sparse$low_coverage)
  expect_equal(cov_sparse$coverage_status, "LOW")

  # Dense points across the 10x10 grid -> > 80% and status OK
  pts_dense <- expand.grid(
    long = seq(10.005, 10.095, length.out = 10),
    lat = seq(50.005, 50.095, length.out = 10)
  )
  cov_dense <- calculate_polygon_coverage(poly, pts_dense, n_grid = 10L, low_threshold = 20)
  expect_gte(cov_dense$coverage_pct, 80)
  expect_false(cov_dense$low_coverage)
  expect_equal(cov_dense$coverage_status, "OK")
  expect_error(resolve_n_grid(0L), "`n_grid` must be a positive integer")

  # Summarize across multiple clusters
  frame_df <- rbind(
    cbind(ea = "EA_SPARSE", pts_sparse, coverage_pct = cov_sparse$coverage_pct),
    cbind(ea = "EA_DENSE", pts_dense, coverage_pct = cov_dense$coverage_pct)
  )
  sum_df <- summarize_cluster_coverage(frame_df, cluster_var = "ea")
  expect_equal(nrow(sum_df), 2L)
  expect_equal(sum_df$coverage_status[sum_df$cluster_id == "EA_SPARSE"], "LOW")
  expect_equal(sum_df$coverage_status[sum_df$cluster_id == "EA_DENSE"], "OK")
})
