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

test_that("add_marker_drawing and clear_marker_drawing work on google_map and google_map_update", {
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

  expect_error(add_marker_drawing(list()), "`map` must be a `google_map`")
  expect_error(clear_marker_drawing(list()), "`map` must be a `google_map`")

  js_tag <- suso_drawing_js()
  expect_s3_class(js_tag, "shiny.tag")
  expect_match(as.character(js_tag), "add_suso_drawing")
  expect_match(as.character(js_tag), "clear_suso_drawing")
  expect_match(as.character(js_tag), "ensureShinyOutputClasses")
})
