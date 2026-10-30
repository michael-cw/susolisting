#' Add custom marker drawing control to a googleway map
#'
#' @description
#' Replaces `googleway::add_drawing(drawing_modes = "marker")`, which relied on
#' the now-decommissioned `google.maps.drawing.DrawingManager` library in the
#' Google Maps JavaScript API (removed in v3.65+). Attaches a lightweight
#' custom drawing control (Pan / Add Marker) and click listener that places
#' markers and emits the `<map_id>_markercomplete` Shiny input event.
#'
#' @param map A `google_map` or `google_map_update` object created by `googleway`.
#'
#' @return The modified `google_map` or `google_map_update` object.
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
  }
  stop("`map` must be a `google_map` or `google_map_update` object.", call. = FALSE)
}

#' Clear drawn markers from a googleway map
#'
#' @description
#' Clears all markers placed via `add_marker_drawing()` from the specified
#' `google_map` or `google_map_update` object.
#'
#' @param map A `google_map` or `google_map_update` object created by `googleway`.
#'
#' @return The modified `google_map` or `google_map_update` object.
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
  }
  stop("`map` must be a `google_map` or `google_map_update` object.", call. = FALSE)
}

#' JavaScript tags for custom Google Maps marker drawing control
#'
#' @description
#' Returns the `shiny::tags$script` implementing `add_suso_drawing` and
#' `clear_suso_drawing` in the browser to replace the deprecated and removed
#' `google.maps.drawing.DrawingManager` API, and patches `googleway`'s
#' `clear_object` and `polygon_click` helpers for null-safety and non-blocking
#' boundary polygons.
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

        function syncBoundsPolygonsNonClickable(map_id) {
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

        installGooglewayHooks();

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
