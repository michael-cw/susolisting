#' Resolve and validate the satellite imagery map provider
#'
#' @param provider Character string specifying the map provider (`"google"`,
#'   `"vantor"`, or `"maxar"`). If `NULL`, reads
#'   `getOption("susolisting.provider", "google")`.
#'
#' @return Either `"google"` or `"vantor"`.
#' @noRd
#' @keywords internal
resolve_map_provider <- function(provider = NULL) {
  if (is.null(provider) || length(provider) == 0L || is.na(provider[1]) || !nzchar(provider[1])) {
    provider <- getOption("susolisting.provider", "google")
  }
  provider <- tolower(trimws(as.character(provider[1])))
  if (!provider %in% c("google", "vantor", "maxar")) {
    stop("`provider` must be one of \"google\", \"vantor\", or \"maxar\".", call. = FALSE)
  }
  if (identical(provider, "maxar")) {
    provider <- "vantor"
  }
  provider
}

#' Add custom marker drawing control to a googleway or leaflet map
#'
#' @description
#' Replaces `googleway::add_drawing(drawing_modes = "marker")`, which relied on
#' the now-decommissioned `google.maps.drawing.DrawingManager` library in the
#' Google Maps JavaScript API (removed in v3.65+), and provides an equivalent
#' Pan / Marker drawing control for `leaflet` maps used with the `vantorr`
#' provider. Attaches a lightweight custom drawing control (Pan / Add Marker)
#' and click listener that places markers and emits the
#' `<map_id>_markercomplete` Shiny input event.
#'
#' @param map A `google_map`, `google_map_update`, `leaflet`, or
#'   `leaflet_proxy` object.
#'
#' @return The modified map object.
#' @noRd
#' @keywords internal
add_marker_drawing <- function(map) {
  if (inherits(map, "google_map")) {
    x <- map$x$calls
    if (is.null(x)) {
      x <- list()
    }
    x[[length(x) + 1L]] <- list(functions = "add_suso_drawing", args = list())
    map$x$calls <- x
    return(map)
  } else if (inherits(map, "google_map_update")) {
    msg <- list(
      id = map$id,
      calls = list(
        list(
          dependencies = list(),
          method = "add_suso_drawing",
          args = list()
        )
      )
    )
    sess <- map$session
    if (!is.null(sess)) {
      if (isTRUE(map$deferUntilFlush)) {
        sess$onFlushed(function() {
          sess$sendCustomMessage("googlemap-calls", msg)
        }, once = TRUE)
      } else {
        sess$sendCustomMessage("googlemap-calls", msg)
      }
    }
    return(map)
  } else if (inherits(map, c("leaflet", "leaflet_proxy"))) {
    return(leaflet::invokeMethod(map, NULL, "add_suso_drawing"))
  }
  stop(
    "`map` must be a `google_map`, `google_map_update`, `leaflet`, or `leaflet_proxy` object.",
    call. = FALSE
  )
}

#' Clear drawn markers from a googleway or leaflet map
#'
#' @description
#' Clears all markers placed via `add_marker_drawing()` from the specified
#' `google_map`, `google_map_update`, `leaflet`, or `leaflet_proxy` object.
#'
#' @param map A `google_map`, `google_map_update`, `leaflet`, or
#'   `leaflet_proxy` object.
#'
#' @return The modified map object.
#' @noRd
#' @keywords internal
clear_marker_drawing <- function(map) {
  if (inherits(map, "google_map")) {
    x <- map$x$calls
    if (is.null(x)) {
      x <- list()
    }
    x[[length(x) + 1L]] <- list(functions = "clear_suso_drawing", args = list())
    map$x$calls <- x
    return(map)
  } else if (inherits(map, "google_map_update")) {
    msg <- list(
      id = map$id,
      calls = list(
        list(
          dependencies = list(),
          method = "clear_suso_drawing",
          args = list()
        )
      )
    )
    sess <- map$session
    if (!is.null(sess)) {
      if (isTRUE(map$deferUntilFlush)) {
        sess$onFlushed(function() {
          sess$sendCustomMessage("googlemap-calls", msg)
        }, once = TRUE)
      } else {
        sess$sendCustomMessage("googlemap-calls", msg)
      }
    }
    return(map)
  } else if (inherits(map, c("leaflet", "leaflet_proxy"))) {
    return(leaflet::invokeMethod(map, NULL, "clear_suso_drawing"))
  }
  stop(
    "`map` must be a `google_map`, `google_map_update`, `leaflet`, or `leaflet_proxy` object.",
    call. = FALSE
  )
}

#' JavaScript tags for custom Google Maps and Leaflet marker drawing controls
#'
#' @description
#' Returns the `shiny::tags$script` implementing `add_suso_drawing` and
#' `clear_suso_drawing` in the browser for both `googleway` and `leaflet`
#' maps, and patches `googleway`'s `clear_object` and `polygon_click` helpers
#' for null-safety and non-blocking boundary polygons.
#'
#' @return A `shiny.tag` object containing the script definition.
#' @noRd
#' @keywords internal
suso_drawing_js <- function() {
  shiny::tags$script(shiny::HTML("
      (function() {
        // Patch googleway's clear_object to prevent TypeError when clearing empty/uninitialized layers
        window.clear_object = function(map_id, objType, layer_id) {
          var arr = window[map_id + objType + layer_id];
          if (Array.isArray(arr)) {
            for (var i = 0; i < arr.length; i++) {
              if (arr[i]) {
                if (window.google && google.maps && google.maps.event) {
                  google.maps.event.clearInstanceListeners(arr[i]);
                }
                if (typeof arr[i].setMap === 'function') {
                  arr[i].setMap(null);
                }
                arr[i] = null;
              }
            }
            window[map_id + objType + layer_id] = null;
            if (typeof window.clear_legend === 'function') {
              window.clear_legend(map_id, layer_id);
            }
          }
        };

        function ensureShinyOutputClasses(map_id) {
          var mapDiv = document.getElementById(map_id);
          if (mapDiv && mapDiv.classList) {
            if (mapDiv.classList.contains('googlemap') || mapDiv.classList.contains('google_map')) {
              mapDiv.classList.add('google_map', 'html-widget', 'html-widget-output', 'shiny-bound-output');
              mapDiv.classList.remove('recalculating');
            } else if (mapDiv.classList.contains('leaflet')) {
              mapDiv.classList.add('leaflet', 'html-widget', 'html-widget-output', 'shiny-bound-output');
              mapDiv.classList.remove('recalculating');
            }
          }
        }

        if (window.jQuery) {
          window.jQuery(document).on('shiny:value shiny:visualchange', function(event) {
            if (event && event.target && event.target.id) {
              var targetId = event.target.id;
              window.setTimeout(function() { ensureShinyOutputClasses(targetId); }, 0);
              window.setTimeout(function() { ensureShinyOutputClasses(targetId); }, 50);
              window.setTimeout(function() { ensureShinyOutputClasses(targetId); }, 150);
            }
          });
        }

        function syncBoundsPolygonsNonClickable(map_id) {
          ensureShinyOutputClasses(map_id);
          var polys = window[map_id + 'googlePolygonbounds'];
          if (Array.isArray(polys)) {
            for (var i = 0; i < polys.length; i++) {
              if (polys[i] && typeof polys[i].setOptions === 'function') {
                polys[i].setOptions({ clickable: false });
              }
            }
          }
        }

        function schedulePolygonSync(map_id) {
          syncBoundsPolygonsNonClickable(map_id);
          window.setTimeout(function() { syncBoundsPolygonsNonClickable(map_id); }, 50);
          window.setTimeout(function() { syncBoundsPolygonsNonClickable(map_id); }, 250);
          window.setTimeout(function() { syncBoundsPolygonsNonClickable(map_id); }, 600);
        }

        function installGooglewayHooks() {
          if (typeof window.polygon_click === 'function' && !window.__susoPolygonClickWrapped) {
            var origPolygonClick = window.polygon_click;
            window.polygon_click = function(m_id, polygonObject, polygon_id, polygonInfo) {
              if (polygonObject && typeof polygonObject.setOptions === 'function') {
                polygonObject.setOptions({ clickable: false });
              }
              return origPolygonClick.apply(this, arguments);
            };
            window.__susoPolygonClickWrapped = true;
          }

          if (typeof window.add_polygons === 'function' && !window.__susoAddPolygonsWrapped) {
            var origAddPolygons = window.add_polygons;
            window.add_polygons = function(m_id) {
              if (!window[m_id + 'map']) return;
              var res = origAddPolygons.apply(this, arguments);
              schedulePolygonSync(m_id);
              return res;
            };
            window.__susoAddPolygonsWrapped = true;
          }
        }

        function installLeafletHooks() {
          if (window.LeafletWidget && window.LeafletWidget.methods) {
            window.LeafletWidget.methods.add_suso_drawing = function() {
              var map = this;
              if (!map || typeof map.getContainer !== 'function') return;
              var container = map.getContainer();
              var map_id = map.id || (container ? container.id : 'detailSampMap');
              ensureShinyOutputClasses(map_id);

              if (!Array.isArray(map.__susoLeafletMarkers)) {
                map.__susoLeafletMarkers = [];
              }
              if (!map.__susoDrawingMode) {
                map.__susoDrawingMode = 'marker';
              }

              function updateLeafletCursor() {
                var isMarker = map.__susoDrawingMode === 'marker';
                if (container) {
                  container.style.cursor = isMarker ? 'crosshair' : '';
                }
              }

              if (!map.__susoControlAdded && window.L && L.Control) {
                var SusoControl = L.Control.extend({
                  options: { position: 'topright' },
                  onAdd: function() {
                    var controlDiv = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
                    controlDiv.style.display = 'flex';
                    controlDiv.style.backgroundColor = '#fff';
                    controlDiv.style.borderRadius = '2px';
                    controlDiv.style.boxShadow = '0 1px 4px -1px rgba(0,0,0,0.3)';
                    controlDiv.style.overflow = 'hidden';
                    controlDiv.style.border = 'none';

                    var btnStyleBase = 'border:none;background:#fff;padding:6px 10px;cursor:pointer;font-size:13px;line-height:18px;display:flex;align-items:center;gap:4px;color:#333;';
                    var panBtn = document.createElement('button');
                    panBtn.type = 'button';
                    panBtn.title = 'Stop drawing (Pan map)';
                    panBtn.style.cssText = btnStyleBase + 'border-right:1px solid #e6e6e6;';
                    panBtn.innerHTML = '<span>&#9995;</span><span>Pan</span>';

                    var markerBtn = document.createElement('button');
                    markerBtn.type = 'button';
                    markerBtn.title = 'Add a building marker';
                    markerBtn.style.cssText = btnStyleBase;
                    markerBtn.innerHTML = '<span>&#128205;</span><span>Marker</span>';

                    function updateModeUI() {
                      var isMarker = map.__susoDrawingMode === 'marker';
                      markerBtn.style.backgroundColor = isMarker ? '#e8f0fe' : '#fff';
                      markerBtn.style.fontWeight = isMarker ? 'bold' : 'normal';
                      panBtn.style.backgroundColor = !isMarker ? '#e8f0fe' : '#fff';
                      panBtn.style.fontWeight = !isMarker ? 'bold' : 'normal';
                      updateLeafletCursor();
                    }

                    panBtn.addEventListener('click', function(e) {
                      e.stopPropagation();
                      e.preventDefault();
                      map.__susoDrawingMode = 'pan';
                      updateModeUI();
                    });

                    markerBtn.addEventListener('click', function(e) {
                      e.stopPropagation();
                      e.preventDefault();
                      map.__susoDrawingMode = 'marker';
                      updateModeUI();
                    });

                    controlDiv.appendChild(panBtn);
                    controlDiv.appendChild(markerBtn);

                    if (L.DomEvent) {
                      L.DomEvent.disableClickPropagation(controlDiv);
                      L.DomEvent.disableScrollPropagation(controlDiv);
                    }

                    map.__susoUpdateModeUI = updateModeUI;
                    updateModeUI();
                    return controlDiv;
                  }
                });
                map.__susoLeafletControl = new SusoControl();
                map.addControl(map.__susoLeafletControl);
                map.__susoControlAdded = true;
              }

              if (!map.__susoClickAdded) {
                map.on('click', function(e) {
                  if (map.__susoDrawingMode === 'marker' && e && e.latlng) {
                    if (window.L && typeof L.marker === 'function') {
                      var m = L.marker(e.latlng).addTo(map);
                      if (!Array.isArray(map.__susoLeafletMarkers)) {
                        map.__susoLeafletMarkers = [];
                      }
                      map.__susoLeafletMarkers.push(m);
                    }
                    var eventInfo = {
                      position: {
                        lat: e.latlng.lat,
                        lng: e.latlng.lng
                      },
                      randomValue: Math.random()
                    };
                    if (window.Shiny && typeof Shiny.setInputValue === 'function') {
                      Shiny.setInputValue(map_id + '_markercomplete', eventInfo, {priority: 'event'});
                    } else if (window.Shiny && typeof Shiny.onInputChange === 'function') {
                      Shiny.onInputChange(map_id + '_markercomplete', eventInfo);
                    }
                  }
                });
                map.__susoClickAdded = true;
              }

              if (typeof map.__susoUpdateModeUI === 'function') {
                map.__susoUpdateModeUI();
              } else {
                updateLeafletCursor();
              }
            };

            window.LeafletWidget.methods.clear_suso_drawing = function() {
              var map = this;
              if (!map) return;
              if (Array.isArray(map.__susoLeafletMarkers)) {
                for (var i = 0; i < map.__susoLeafletMarkers.length; i++) {
                  if (map.__susoLeafletMarkers[i] && typeof map.removeLayer === 'function') {
                    map.removeLayer(map.__susoLeafletMarkers[i]);
                  }
                }
              }
              map.__susoLeafletMarkers = [];
            };
          }
        }

        installGooglewayHooks();
        installLeafletHooks();
        if (window.jQuery) {
          window.jQuery(function() {
            installGooglewayHooks();
            installLeafletHooks();
          });
        }

        function placeSusoMarker(map_id, latLng) {
          var map = window[map_id + 'map'];
          if (!map || !latLng) return;
          if (!Array.isArray(window[map_id + 'googleDrawingOverlays'])) {
            window[map_id + 'googleDrawingOverlays'] = [];
          }
          var marker = new google.maps.Marker({
            position: latLng,
            map: map
          });
          window[map_id + 'googleDrawingOverlays'].push(marker);

          var eventInfo = {
            position: {
              lat: typeof latLng.lat === 'function' ? latLng.lat() : latLng.lat,
              lng: typeof latLng.lng === 'function' ? latLng.lng() : latLng.lng
            },
            randomValue: Math.random()
          };
          if (window.Shiny && typeof Shiny.setInputValue === 'function') {
            Shiny.setInputValue(map_id + '_markercomplete', eventInfo, {priority: 'event'});
          } else if (window.Shiny && typeof Shiny.onInputChange === 'function') {
            Shiny.onInputChange(map_id + '_markercomplete', eventInfo);
          }
        }

        window.add_suso_drawing = function(map_id) {
          installGooglewayHooks();
          var map = window[map_id + 'map'];
          if (!map) return;

          if (!Array.isArray(window[map_id + 'googleDrawingOverlays'])) {
            window[map_id + 'googleDrawingOverlays'] = [];
          }
          if (!window[map_id + 'drawingMode']) {
            window[map_id + 'drawingMode'] = 'marker';
          }

          schedulePolygonSync(map_id);

          if (map.__susoDrawingInitialized) {
            map.setOptions({
              draggableCursor: window[map_id + 'drawingMode'] === 'marker' ? 'crosshair' : null
            });
            return;
          }
          map.__susoDrawingInitialized = true;
          window[map_id + 'googleDrawingOverlays'] = [];

          // Build TOP_RIGHT drawing mode control (Pan / Add Marker)
          var controlDiv = document.createElement('div');
          controlDiv.style.display = 'flex';
          controlDiv.style.margin = '10px';
          controlDiv.style.backgroundColor = '#fff';
          controlDiv.style.borderRadius = '2px';
          controlDiv.style.boxShadow = '0 1px 4px -1px rgba(0,0,0,0.3)';
          controlDiv.style.overflow = 'hidden';

          var btnStyleBase = 'border:none;background:#fff;padding:6px 10px;cursor:pointer;font-size:13px;line-height:18px;display:flex;align-items:center;gap:4px;color:#333;';
          var panBtn = document.createElement('button');
          panBtn.type = 'button';
          panBtn.title = 'Stop drawing (Pan map)';
          panBtn.style.cssText = btnStyleBase + 'border-right:1px solid #e6e6e6;';
          panBtn.innerHTML = '<span>&#9995;</span><span>Pan</span>';

          var markerBtn = document.createElement('button');
          markerBtn.type = 'button';
          markerBtn.title = 'Add a building marker';
          markerBtn.style.cssText = btnStyleBase;
          markerBtn.innerHTML = '<span>&#128205;</span><span>Marker</span>';

          function updateModeUI() {
            var isMarker = window[map_id + 'drawingMode'] === 'marker';
            markerBtn.style.backgroundColor = isMarker ? '#e8f0fe' : '#fff';
            markerBtn.style.fontWeight = isMarker ? 'bold' : 'normal';
            panBtn.style.backgroundColor = !isMarker ? '#e8f0fe' : '#fff';
            panBtn.style.fontWeight = !isMarker ? 'bold' : 'normal';
            map.setOptions({
              draggableCursor: isMarker ? 'crosshair' : null
            });
            schedulePolygonSync(map_id);
          }

          panBtn.addEventListener('click', function(e) {
            e.stopPropagation();
            window[map_id + 'drawingMode'] = 'pan';
            updateModeUI();
          });

          markerBtn.addEventListener('click', function(e) {
            e.stopPropagation();
            window[map_id + 'drawingMode'] = 'marker';
            updateModeUI();
          });

          controlDiv.appendChild(panBtn);
          controlDiv.appendChild(markerBtn);
          if (map.controls && google.maps.ControlPosition) {
            map.controls[google.maps.ControlPosition.TOP_RIGHT].push(controlDiv);
          }

          updateModeUI();

          google.maps.event.addListener(map, 'click', function(event) {
            if (window[map_id + 'drawingMode'] === 'marker' && event && event.latLng) {
              placeSusoMarker(map_id, event.latLng);
            }
          });
        };

        window.clear_suso_drawing = function(map_id) {
          var overlays = window[map_id + 'googleDrawingOverlays'];
          if (Array.isArray(overlays)) {
            for (var i = 0; i < overlays.length; i++) {
              if (overlays[i] && typeof overlays[i].setMap === 'function') {
                overlays[i].setMap(null);
              }
            }
          }
          window[map_id + 'googleDrawingOverlays'] = [];
        };

        // Backwards compatibility aliases for googleway drawing functions
        window.add_drawing = window.add_suso_drawing;
        window.clear_drawing = window.clear_suso_drawing;
      })();
    "))
}

#' Resolve and validate the spatial grid resolution (`n_grid`)
#'
#' @param n_grid Integer specifying the number of grid divisions along each axis.
#'   If `NULL`, reads `getOption("susolisting.n_grid", 10L)`.
#'
#' @return Positive integer giving the grid dimension along each axis.
#' @noRd
#' @keywords internal
resolve_n_grid <- function(n_grid = NULL) {
  if (is.null(n_grid) || length(n_grid) == 0L || is.na(n_grid[1])) {
    n_grid <- getOption("susolisting.n_grid", 10L)
  }
  n_grid <- suppressWarnings(as.integer(n_grid[1]))
  if (is.na(n_grid) || n_grid < 1L) {
    stop("`n_grid` must be a positive integer.", call. = FALSE)
  }
  n_grid
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
#'   (default `getOption("susolisting.n_grid", 10L)`, producing a `10 x 10`
#'   spatial grid clipped to the polygon).
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
                                       n_grid = getOption("susolisting.n_grid", 10L),
                                       low_threshold = 20) {
  n_grid <- resolve_n_grid(n_grid)
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
    poly_utm <- sf::st_transform(poly_geom, target_crs)
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
#' @param n_grid Integer grid dimension passed to [calculate_polygon_coverage()]
#'   (default `getOption("susolisting.n_grid", 10L)`).
#' @param low_threshold Numeric percentage threshold below which a cluster is
#'   flagged as `"LOW"` (default `20`).
#'
#' @return A `data.frame` with columns `cluster_id`, `coverage_pct`,
#'   `low_coverage`, and `coverage_status`.
#' @noRd
#' @keywords internal
summarize_cluster_coverage <- function(frame_df,
                                       cluster_var = "ea",
                                       n_grid = getOption("susolisting.n_grid", 10L),
                                       low_threshold = 20) {
  n_grid <- resolve_n_grid(n_grid)
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
