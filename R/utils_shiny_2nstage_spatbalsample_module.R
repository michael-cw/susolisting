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
