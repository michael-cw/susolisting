#' Load and validate an ESRI shapefile
#'
#' @description Reads an ESRI shapefile via [sf::st_read()], repairs invalid
#' geometries if necessary, and transforms coordinates to WGS84 (EPSG:4326).
#'
#' @param path Character string specifying the file path to the `.shp` file.
#' @param lay Optional layer name (retained for backwards compatibility).
#' @param sp.Library Character string specifying the spatial library (`"sf"`).
#'
#' @return A list of length 2: element 1 is the WKT CRS string, and element 2
#'   is the validated `sf` object in EPSG:4326.
#' @noRd
#' @keywords internal
shapeLoad2 <- function(path = NULL, lay = NULL, sp.Library = "sf") {
  outlist <- vector("list", 2)
  SHP <- sf::st_read(path, quiet = TRUE)
  crsOld <- sf::st_crs(SHP)

  if (sum(sf::st_is_valid(SHP), na.rm = TRUE) != nrow(SHP)) {
    suppressWarnings(
      SHP <- SHP %>%
        sf::st_transform(3857) %>%
        sf::st_make_valid() %>%
        sf::st_cast("MULTIPOLYGON") %>%
        sf::st_buffer(0.0) %>%
        sf::st_transform(crsOld)
    )
  }

  if (!sf::st_is_longlat(SHP)) {
    SHP <- sf::st_transform(SHP, 4326)
  }

  outlist[[1]] <- sf::st_crs(SHP)[[2]]
  outlist[[2]] <- SHP
  return(outlist)
}

#' Sanitize a string for safe file and directory naming
#'
#' @description Replaces non-alphanumeric characters, strips non-ASCII symbols,
#' squishes repeated whitespace, and substitutes spaces with a replacement string.
#'
#' @param input_string Character string to sanitize.
#' @param repl_for_whitespace Character string used to replace whitespace
#'   (default `"_"`).
#'
#' @return A sanitized character string.
#' @noRd
#' @keywords internal
sanitize_string <- function(input_string, repl_for_whitespace = "_") {
  sanitized_string <- stringr::str_replace_all(
    input_string,
    "[^a-zA-Z0-9_]",
    stringr::fixed(" ")
  ) %>%
    iconv("UTF-8", "ASCII", sub = "") %>%
    stringr::str_squish() %>%
    stringr::str_replace_all(stringr::fixed(" "), repl_for_whitespace)

  return(sanitized_string)
}

#' Resolve the application storage directory for enumerator listing files
#'
#' @description Returns the directory where enumerator listing `.fst` files are
#' stored. In interactive sessions, uses [tools::R_user_dir()]. In
#' non-interactive sessions (such as automated tests and `R CMD check`), falls
#' back to [tempdir()] so the user's home filespace is never modified.
#'
#' @return Character string specifying the path to the storage directory.
#' @noRd
#' @keywords internal
get_app_download_dir <- function() {
  appdir <- getOption("susolisting.storage_dir")
  if (is.null(appdir) || !nzchar(appdir)) {
    if (interactive()) {
      appdir <- file.path(tools::R_user_dir("susolisting", which = "data"), "download")
    } else {
      appdir <- file.path(tempdir(), "susolisting", "download")
    }
  }
  if (!dir.exists(appdir)) {
    dir.create(appdir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(appdir) || file.access(appdir, 2) != 0) {
    appdir <- file.path(tempdir(), "susolisting", "download")
    dir.create(appdir, recursive = TRUE, showWarnings = FALSE)
  }
  appdir
}
