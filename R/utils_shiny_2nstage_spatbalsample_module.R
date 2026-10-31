#' Project an sf object to its appropriate UTM zone
#'
#' @description Determines the UTM zone from the bounding box of a geographic
#' (WGS84) `sf` object and transforms it to the corresponding WGS84 UTM EPSG
#' projection (`326xx` for Northern Hemisphere, `327xx` for Southern Hemisphere).
#'
#' @param shp An `sf` object in geographic coordinates (longitude/latitude).
#'
#' @return The `sf` object transformed to its corresponding UTM CRS.
#' @noRd
#' @keywords internal
project_to_utm <- function(shp) {
  bbox <- sf::st_bbox(shp)
  maxLong <- unname(bbox["xmax"])
  maxLat <- unname(bbox["ymax"])

  long2UTM <- function(long = maxLong) {
    (floor((long + 180) / 6) %% 60) + 1
  }

  utmZone <- as.integer(long2UTM())
  epsg <- ifelse(
    maxLat <= 0,
    sprintf("327%02d", utmZone),
    sprintf("326%02d", utmZone)
  )

  shp <- sf::st_transform(shp, as.numeric(epsg))
  return(shp)
}

#' Calculate spatial grid coverage of a polygon by point locations
#'
#' @description Partitions an Enumeration Area (EA) polygon into a regular
#' planar spatial grid (in UTM coordinates), clips grid cells to the polygon
#' boundary, and computes the area-weighted percentage of the polygon covered
#' by grid cells that contain at least one listed building point. Areas below
#' `low_threshold` (default `20%`) are flagged as low coverage.
#'
#' @param polygon_sf Optional `sf` or `sfc` polygon object representing the
#'   enumeration area boundary. If `NULL`, the convex hull (buffered if
#'   degenerate) of the point locations is used as fallback.
#' @param points_df A `data.frame`, `data.table`, or `sf` object of point
#'   locations containing longitude and latitude coordinates.
#' @param n_grid Integer specifying the number of grid divisions along each axis
#'   (default `5L`, producing a `5 x 5` spatial grid clipped to the polygon).
#' @param low_threshold Numeric percentage threshold below which an area is
#'   flagged as having low coverage (default `20`).
#'
#' @return A named list with elements:
#'   - `coverage_pct`: Numeric percentage (`0` to `100`) of the polygon area
#'     covered by occupied grid cells.
#'   - `low_coverage`: Logical flag (`TRUE` when `coverage_pct < low_threshold`).
#'   - `coverage_status`: Character string (`"LOW"` or `"OK"`).
#'   - `n_cells_total`: Integer count of grid cells intersecting the polygon.
#'   - `n_cells_covered`: Integer count of grid cells containing at least one point.
#' @noRd
#' @keywords internal
calculate_polygon_coverage <- function(polygon_sf = NULL,
                                       points_df = NULL,
                                       n_grid = 5L,
                                       low_threshold = 20) {
  if (is.null(points_df) || nrow(points_df) == 0L) {
    return(list(
      coverage_pct = 0,
      low_coverage = TRUE,
      coverage_status = "LOW",
      n_cells_total = as.integer(n_grid * n_grid),
      n_cells_covered = 0L
    ))
  }

  if (inherits(points_df, "sf")) {
    pts_sf <- if (sf::st_is_longlat(points_df)) points_df else sf::st_transform(points_df, 4326)
  } else {
    df <- as.data.frame(points_df)
    lon_col <- names(df)[grepl("^(lon|long|lng|lon_wgs84)$", names(df), ignore.case = TRUE)][1]
    lat_col <- names(df)[grepl("^(lat|lat_wgs84)$", names(df), ignore.case = TRUE)][1]
    if (is.na(lon_col) || is.na(lat_col)) {
      return(list(
        coverage_pct = NA_real_,
        low_coverage = FALSE,
        coverage_status = "NA",
        n_cells_total = 0L,
        n_cells_covered = 0L
      ))
    }
    valid <- !is.na(df[[lon_col]]) & !is.na(df[[lat_col]])
    df <- df[valid, , drop = FALSE]
    if (nrow(df) == 0L) {
      return(list(
        coverage_pct = 0,
        low_coverage = TRUE,
        coverage_status = "LOW",
        n_cells_total = as.integer(n_grid * n_grid),
        n_cells_covered = 0L
      ))
    }
    pts_sf <- sf::st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)
  }

  pts_utm <- project_to_utm(pts_sf)
  target_crs <- sf::st_crs(pts_utm)

  if (!is.null(polygon_sf) && inherits(polygon_sf, c("sf", "sfc")) && length(sf::st_geometry(polygon_sf)) > 0L) {
    poly_geom <- sf::st_geometry(polygon_sf)
    if (sf::st_is_longlat(poly_geom)) {
      poly_utm <- sf::st_transform(poly_geom, target_crs)
    } else {
      poly_utm <- sf::st_transform(poly_geom, target_crs)
    }
    poly_utm <- suppressWarnings(sf::st_make_valid(sf::st_union(poly_utm)))
  } else {
    hull <- suppressWarnings(sf::st_convex_hull(sf::st_union(sf::st_geometry(pts_utm))))
    hull_area <- as.numeric(sf::st_area(hull))
    if (is.na(hull_area) || hull_area <= 0) {
      poly_utm <- sf::st_buffer(hull, dist = 50)
    } else {
      poly_utm <- hull
    }
  }

  grid_sfc <- sf::st_make_grid(
    poly_utm,
    n = c(as.integer(n_grid), as.integer(n_grid)),
    what = "polygons"
  )
  grid_clipped <- suppressWarnings(sf::st_intersection(grid_sfc, poly_utm))
  grid_clipped <- grid_clipped[!sf::st_is_empty(grid_clipped)]

  if (length(grid_clipped) == 0L) {
    return(list(
      coverage_pct = 0,
      low_coverage = TRUE,
      coverage_status = "LOW",
      n_cells_total = 0L,
      n_cells_covered = 0L
    ))
  }

  cell_areas <- as.numeric(sf::st_area(grid_clipped))
  total_area <- sum(cell_areas, na.rm = TRUE)
  hits <- lengths(sf::st_intersects(grid_clipped, pts_utm)) > 0L
  covered_area <- sum(cell_areas[hits], na.rm = TRUE)

  cov_pct <- if (total_area > 0) round((covered_area / total_area) * 100, 1) else 0
  is_low <- is.na(cov_pct) || (cov_pct < low_threshold)

  list(
    coverage_pct = cov_pct,
    low_coverage = is_low,
    coverage_status = if (is_low) "LOW" else "OK",
    n_cells_total = length(grid_clipped),
    n_cells_covered = sum(hits)
  )
}

#' Summarize spatial grid coverage across all clusters in a sampling frame
#'
#' @description Extracts persisted `coverage_pct` values saved at listing time
#' (when available in `frame_df`) or computes fallback spatial grid coverage per
#' cluster via [calculate_polygon_coverage()].
#'
#' @param frame_df A `data.frame` or `data.table` of listed building points.
#' @param cluster_var Character string naming the cluster / EA column.
#' @param n_grid Integer grid dimension passed to [calculate_polygon_coverage()].
#' @param low_threshold Numeric percentage threshold below which a cluster is
#'   flagged as `"LOW"` (default `20`).
#'
#' @return A `data.frame` with columns `cluster_id`, `coverage_pct`,
#'   `low_coverage`, and `coverage_status`.
#' @noRd
#' @keywords internal
summarize_cluster_coverage <- function(frame_df,
                                       cluster_var = "ea",
                                       n_grid = 5L,
                                       low_threshold = 20) {
  df <- if (inherits(frame_df, "sf")) {
    as.data.frame(sf::st_set_geometry(frame_df, NULL))
  } else {
    as.data.frame(frame_df)
  }

  if (!cluster_var %in% names(df)) {
    cand <- intersect(c("ea", "area_id"), names(df))
    cluster_var <- if (length(cand) > 0L) cand[1] else names(df)[1]
  }

  clusters <- unique(as.character(df[[cluster_var]]))
  cov_pct_vec <- numeric(length(clusters))

  for (i in seq_along(clusters)) {
    cl <- clusters[i]
    sub_df <- df[as.character(df[[cluster_var]]) == cl, , drop = FALSE]
    if ("coverage_pct" %in% names(sub_df) && any(!is.na(sub_df$coverage_pct))) {
      vals <- as.numeric(sub_df$coverage_pct[!is.na(sub_df$coverage_pct)])
      cov_pct_vec[i] <- round(vals[length(vals)], 1)
    } else {
      res <- calculate_polygon_coverage(
        polygon_sf = NULL,
        points_df = sub_df,
        n_grid = n_grid,
        low_threshold = low_threshold
      )
      cov_pct_vec[i] <- res$coverage_pct
    }
  }

  low_vec <- is.na(cov_pct_vec) | (cov_pct_vec < low_threshold)
  status_vec <- ifelse(low_vec, "LOW", "OK")

  data.frame(
    cluster_id = clusters,
    coverage_pct = cov_pct_vec,
    low_coverage = low_vec,
    coverage_status = status_vec,
    stringsAsFactors = FALSE
  )
}
