#' Shiny module UI for downloading the Word sampling report
#'
#' @description Renders an action button and hidden download handler to generate
#' and download a Microsoft Word (`.docx`) sampling report styled according to
#' the World Bank Group Branding and Visual Identity Guidelines.
#'
#' @param id Character string specifying the module namespace ID.
#' @param label Character string for the report download button label.
#' @param style CSS style string for the action button.
#'
#' @return A `shiny.tag.list` object containing the report download controls.
#' @noRd
#' @keywords internal
sampling_report_ui <- function(id,
                               label = "Download Sampling Report",
                               style = "color: #FFFFFF; background-color: #002244; border-color: #009FDA") {
  invisibleButton <- "color: #FFFFFF; background-color: #FFFFFF; visibility: hidden; border-color: #FFFFFF; margin: 0; height: 1px;"

  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::column(
        10,
        shiny::actionButton(
          ns("generateReport"),
          label = label,
          icon = shiny::icon("file-word"),
          width = "100%",
          style = style
        )
      ),
      shiny::column(1)
    ),
    shiny::fluidRow(
      shiny::downloadButton(ns("dwl_report"), "Not visible", style = invisibleButton)
    )
  )
}

#' Shiny module server for generating and downloading the Word sampling report
#'
#' @description Compiles a Microsoft Word (`.docx`) sampling report using the
#' `officer` package documenting the second-stage sampling frame, the sampling
#' methodology and parameters, and the resulting sample.
#'
#' @param id Character string specifying the module namespace ID.
#' @param frame_data Reactive expression returning the second-stage sampling
#'   frame (`data.table` or `data.frame`).
#' @param sample_data Reactive expression returning the drawn second-stage
#'   sample (`data.table` or `data.frame`).
#' @param sampling_params Reactive expression returning a named list of sampling
#'   parameters (`clustervar`, `seed`, `samplemode`, `n_sample`, `mindist`,
#'   `utm_epsg`, `timestamp`).
#' @param file_name Reactive expression returning the base filename (without
#'   `.docx` extension).
#'
#' @return Invisible `NULL` (used for its Shiny download handler side-effects).
#' @noRd
#' @keywords internal
sampling_report_server <- function(id,
                                   frame_data = shiny::reactive(NULL),
                                   sample_data = shiny::reactive(NULL),
                                   sampling_params = shiny::reactive(NULL),
                                   file_name = shiny::reactive("SamplingReport_2stage")) {
  shiny::moduleServer(id, function(input, output, session) {
    report_path <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$generateReport, {
      if (is.null(sample_data()) || nrow(sample_data()) == 0) {
        shiny::showNotification(
          "Please generate a sample first before downloading the sampling report.",
          type = "warning",
          id = "noreportdata"
        )
        shiny::req(FALSE)
      }

      fn_docx <- file.path(tempdir(), paste0(file_name(), ".docx"))
      params <- if (shiny::is.reactive(sampling_params)) sampling_params() else sampling_params

      build_sampling_report_docx(
        frame_data = frame_data(),
        sample_data = sample_data(),
        sampling_params = params,
        file = fn_docx
      )
      report_path(fn_docx)
      shinyjs::click("dwl_report")
    })

    output$dwl_report <- shiny::downloadHandler(
      filename = function() {
        paste0(file_name(), ".docx")
      },
      content = function(file) {
        fn_docx <- report_path()
        if (is.null(fn_docx) || !file.exists(fn_docx)) {
          fn_docx <- file.path(tempdir(), paste0(file_name(), ".docx"))
          params <- if (shiny::is.reactive(sampling_params)) sampling_params() else sampling_params
          build_sampling_report_docx(
            frame_data = frame_data(),
            sample_data = sample_data(),
            sampling_params = params,
            file = fn_docx
          )
        }
        on.exit({
          if (file.exists(fn_docx)) file.remove(fn_docx)
        }, add = TRUE)
        file.copy(fn_docx, file, overwrite = TRUE)
      },
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )
  })
}

#' Resolve the path to the World Bank logo image (`logoWBDG.png`)
#'
#' @param logo_path Optional explicit file path to the logo PNG.
#'
#' @return Character string giving an existing path to `logoWBDG.png`, or `""`
#'   if not found.
#' @noRd
#' @keywords internal
resolve_wbg_logo_path <- function(logo_path = NULL) {
  if (!is.null(logo_path) && nzchar(logo_path) && file.exists(logo_path)) {
    return(logo_path)
  }
  pkg_logo <- system.file("www", "logoWBDG.png", package = "susolisting")
  if (nzchar(pkg_logo) && file.exists(pkg_logo)) {
    return(pkg_logo)
  }
  local_logo <- file.path("inst", "www", "logoWBDG.png")
  if (file.exists(local_logo)) {
    return(local_logo)
  }
  ""
}

#' Build a World Bank-styled Word (`.docx`) sampling report using `officer`
#'
#' @description Creates a comprehensive Microsoft Word report documenting the
#' second-stage building listing frame, the spatial sampling methodology and
#' parameters, and the resulting sample. Styled in accordance with the World
#' Bank Group Branding and Visual Identity Guidelines (Primary Solid Blue
#' `#002244`, Accent Cyan `#009FDA`, Arial typography, and `logoWBDG.png` on the
#' front cover page).
#'
#' @param frame_data A `data.frame` or `data.table` containing the second-stage
#'   sampling frame of listed buildings.
#' @param sample_data A `data.frame`, `data.table`, or `sf` object containing
#'   the selected second-stage sample.
#' @param sampling_params Optional named list of sampling parameters:
#'   `clustervar`, `seed`, `samplemode`, `n_sample`, `mindist`, `utm_epsg`, and
#'   `timestamp`. Missing fields are inferred automatically from the data.
#' @param file Character string specifying the output `.docx` file path.
#' @param logo_path Optional explicit file path to `logoWBDG.png`.
#'
#' @return Invisibly returns the path `file` to the generated `.docx` document.
#' @noRd
#' @keywords internal
build_sampling_report_docx <- function(frame_data,
                                       sample_data,
                                       sampling_params = NULL,
                                       file = tempfile(fileext = ".docx"),
                                       logo_path = NULL) {
  if (inherits(sample_data, "sf")) {
    sample_df <- as.data.frame(sf::st_set_geometry(sample_data, NULL))
  } else {
    sample_df <- as.data.frame(sample_data)
  }

  if (is.null(frame_data)) {
    frame_df <- sample_df
  } else if (inherits(frame_data, "sf")) {
    frame_df <- as.data.frame(sf::st_set_geometry(frame_data, NULL))
  } else {
    frame_df <- as.data.frame(frame_data)
  }

  if (is.null(sampling_params)) {
    sampling_params <- list()
  }

  # Resolve cluster variable
  cluster_var <- sampling_params$clustervar
  if (is.null(cluster_var) || !cluster_var %in% names(sample_df)) {
    candidate_vars <- intersect(c("ea", "area_id", "stratum"), names(sample_df))
    cluster_var <- if (length(candidate_vars) > 0) candidate_vars[1] else names(sample_df)[1]
  }
  frame_cluster_var <- if (cluster_var %in% names(frame_df)) {
    cluster_var
  } else {
    candidate_f <- intersect(c("ea", "area_id"), names(frame_df))
    if (length(candidate_f) > 0) candidate_f[1] else names(frame_df)[1]
  }

  # Resolve coordinate columns
  frame_lon_col <- names(frame_df)[grepl("lon|long", names(frame_df), ignore.case = TRUE)][1]
  frame_lat_col <- names(frame_df)[grepl("lat", names(frame_df), ignore.case = TRUE)][1]
  samp_lon_col <- if ("lon_WGS84" %in% names(sample_df)) {
    "lon_WGS84"
  } else {
    names(sample_df)[grepl("lon|long", names(sample_df), ignore.case = TRUE)][1]
  }
  samp_lat_col <- if ("lat_WGS84" %in% names(sample_df)) {
    "lat_WGS84"
  } else {
    names(sample_df)[grepl("lat", names(sample_df), ignore.case = TRUE)][1]
  }

  # Compute Frame & Sample summaries
  frame_clusters <- as.character(frame_df[[frame_cluster_var]])
  sample_clusters <- as.character(sample_df[[cluster_var]])

  frame_counts_tbl <- table(frame_clusters)
  sample_counts_tbl <- table(sample_clusters)

  n_frame_clusters <- length(frame_counts_tbl)
  n_total_frame <- nrow(frame_df)
  frame_counts_num <- as.numeric(frame_counts_tbl)

  cov_summary <- summarize_cluster_coverage(frame_df, cluster_var = frame_cluster_var)
  cov_lookup <- stats::setNames(cov_summary$coverage_pct, cov_summary$cluster_id)
  flag_lookup <- stats::setNames(cov_summary$coverage_status, cov_summary$cluster_id)
  mean_cov_pct <- mean(cov_summary$coverage_pct, na.rm = TRUE)
  min_cov_pct <- min(cov_summary$coverage_pct, na.rm = TRUE)
  n_low_cov <- sum(cov_summary$low_coverage, na.rm = TRUE)
  low_cov_eas <- cov_summary$cluster_id[cov_summary$low_coverage]

  n_sample_clusters <- length(sample_counts_tbl)
  n_total_sample <- nrow(sample_df)

  wgt_vec <- if ("wgt" %in% names(sample_df) && is.numeric(sample_df$wgt)) {
    sample_df$wgt
  } else {
    rep(n_total_frame / max(1L, n_total_sample), n_total_sample)
  }

  mean_wgt <- mean(wgt_vec, na.rm = TRUE)
  min_wgt <- min(wgt_vec, na.rm = TRUE)
  max_wgt <- max(wgt_vec, na.rm = TRUE)
  sd_wgt <- if (length(wgt_vec) > 1) stats::sd(wgt_vec, na.rm = TRUE) else 0
  cv_wgt <- if (mean_wgt > 0) sd_wgt / mean_wgt else 0
  deff_wgt <- 1 + cv_wgt^2
  n_eff <- n_total_sample / deff_wgt
  overall_rate <- if (n_total_frame > 0) (n_total_sample / n_total_frame) * 100 else 0

  # Resolve sampling parameters
  sample_mode <- if (!is.null(sampling_params$samplemode)) {
    as.character(sampling_params$samplemode)
  } else {
    "Balanced"
  }
  method_label <- switch(
    sample_mode,
    "Balanced" = "Spatially Balanced - Generalized Random Tessellation Stratified (GRTS)",
    "Balanced w. minimum distance" = "Spatially Balanced with Minimum Distance Constraint (GRTS)",
    "Random" = "Simple Random Sample - Independent Random Sampling (IRS)",
    sample_mode
  )
  seed_val <- if (!is.null(sampling_params$seed)) as.character(sampling_params$seed) else "Not specified"
  target_n <- if (!is.null(sampling_params$n_sample)) {
    as.integer(sampling_params$n_sample)
  } else {
    max(as.integer(sample_counts_tbl), na.rm = TRUE)
  }
  mindist_val <- if (
    identical(sample_mode, "Balanced w. minimum distance") &&
      !is.null(sampling_params$mindist) &&
      !is.na(sampling_params$mindist)
  ) {
    paste0(sampling_params$mindist, " meters")
  } else {
    "Not applicable"
  }
  utm_epsg_val <- if (!is.null(sampling_params$utm_epsg) && !is.na(sampling_params$utm_epsg)) {
    paste0("EPSG:", sampling_params$utm_epsg)
  } else {
    "Dynamic UTM Zone (EPSG:326xx / EPSG:327xx)"
  }
  report_time <- if (!is.null(sampling_params$timestamp)) {
    format(sampling_params$timestamp, "%Y-%m-%d %H:%M:%S %Z")
  } else {
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  }

  # World Bank Group Branding & Visual Identity Guidelines Palette (Feb 2016)
  # Primary Solid Blue (PMS 289): #002244 | Accent Bright Cyan: #009FDA
  wbg_navy <- "#002244"
  wbg_cyan <- "#009FDA"
  wbg_text <- "#333333"
  wbg_muted <- "#666666"
  wbg_tint <- "#EBF4FA"

  doc <- officer::read_docx()
  doc <- officer::set_doc_properties(
    doc,
    title = "Second-Stage Spatial Sampling Report",
    subject = "Enumeration Area Building Listing and Second-Stage Sample Documentation",
    creator = "World Bank Group - Development Data Group / JDC (susolisting)"
  )
  doc <- officer::body_set_default_section(
    doc,
    value = officer::prop_section(
      page_margins = officer::page_mar(bottom = 0.9, top = 0.9, right = 0.9, left = 0.9)
    )
  )

  # Register custom World Bank Group paragraph styles
  doc <- officer::docx_set_paragraph_style(
    doc,
    style_id = "WBGCoverCategory",
    style_name = "WBG Cover Category",
    fp_p = officer::fp_par(padding.top = 18, padding.bottom = 4),
    fp_t = officer::fp_text_lite(
      color = wbg_cyan,
      font.size = 10,
      bold = TRUE,
      font.family = "Arial"
    )
  )
  doc <- officer::docx_set_paragraph_style(
    doc,
    style_id = "WBGCoverTitle",
    style_name = "WBG Cover Title",
    fp_p = officer::fp_par(padding.top = 4, padding.bottom = 6),
    fp_t = officer::fp_text_lite(
      color = wbg_navy,
      font.size = 24,
      bold = TRUE,
      font.family = "Arial"
    )
  )
  doc <- officer::docx_set_paragraph_style(
    doc,
    style_id = "WBGCoverSubtitle",
    style_name = "WBG Cover Subtitle",
    fp_p = officer::fp_par(
      padding.top = 2,
      padding.bottom = 14,
      border.bottom = officer::fp_border(color = wbg_cyan, width = 2.5)
    ),
    fp_t = officer::fp_text_lite(
      color = wbg_cyan,
      font.size = 13,
      bold = FALSE,
      font.family = "Arial"
    )
  )
  doc <- officer::docx_set_paragraph_style(
    doc,
    style_id = "WBGHeading1",
    style_name = "WBG Heading 1",
    fp_p = officer::fp_par(
      padding.top = 14,
      padding.bottom = 4,
      border.bottom = officer::fp_border(color = wbg_cyan, width = 1.5),
      keep_with_next = TRUE
    ),
    fp_t = officer::fp_text_lite(
      color = wbg_navy,
      font.size = 14,
      bold = TRUE,
      font.family = "Arial"
    )
  )
  doc <- officer::docx_set_paragraph_style(
    doc,
    style_id = "WBGHeading2",
    style_name = "WBG Heading 2",
    fp_p = officer::fp_par(
      padding.top = 10,
      padding.bottom = 3,
      keep_with_next = TRUE
    ),
    fp_t = officer::fp_text_lite(
      color = wbg_cyan,
      font.size = 11.5,
      bold = TRUE,
      font.family = "Arial"
    )
  )
  doc <- officer::docx_set_paragraph_style(
    doc,
    style_id = "WBGBody",
    style_name = "WBG Body",
    fp_p = officer::fp_par(
      padding.top = 2,
      padding.bottom = 6,
      line_spacing = 1.15
    ),
    fp_t = officer::fp_text_lite(
      color = wbg_text,
      font.size = 10,
      font.family = "Arial"
    )
  )
  doc <- officer::docx_set_paragraph_style(
    doc,
    style_id = "WBGCallout",
    style_name = "WBG Callout",
    fp_p = officer::fp_par(
      padding.top = 5,
      padding.bottom = 5,
      padding.left = 10,
      padding.right = 10,
      shading.color = wbg_tint,
      border.left = officer::fp_border(color = wbg_cyan, width = 3)
    ),
    fp_t = officer::fp_text_lite(
      color = wbg_navy,
      font.size = 10,
      font.family = "Arial"
    )
  )
  doc <- officer::docx_set_paragraph_style(
    doc,
    style_id = "WBGTableTitle",
    style_name = "WBG Table Title",
    fp_p = officer::fp_par(
      padding.top = 8,
      padding.bottom = 3,
      keep_with_next = TRUE
    ),
    fp_t = officer::fp_text_lite(
      color = wbg_navy,
      font.size = 10,
      bold = TRUE,
      font.family = "Arial"
    )
  )
  doc <- officer::docx_set_paragraph_style(
    doc,
    style_id = "WBGCaption",
    style_name = "WBG Caption",
    fp_p = officer::fp_par(padding.top = 3, padding.bottom = 8),
    fp_t = officer::fp_text_lite(
      color = wbg_muted,
      font.size = 8.5,
      italic = TRUE,
      font.family = "Arial"
    )
  )

  fp_label <- officer::fp_text_lite(
    color = wbg_navy,
    font.size = 10,
    bold = TRUE,
    font.family = "Arial"
  )
  fp_value <- officer::fp_text_lite(
    color = wbg_text,
    font.size = 10,
    font.family = "Arial"
  )

  xml_escape <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&apos;", x, fixed = TRUE)
    x
  }

  add_wbg_table <- function(doc_obj, df_val, col_widths = NULL, col_align = NULL) {
    nc <- ncol(df_val)
    nr <- nrow(df_val)
    total_dxa <- 9648L # 6.7 inches in dxa (8.5in page width minus 2 * 0.9in margins)
    if (is.null(col_widths)) {
      hdr_nchar <- nchar(colnames(df_val))
      body_nchar <- vapply(
        df_val,
        function(col) max(c(1L, nchar(as.character(col))), na.rm = TRUE),
        integer(1)
      )
      weights <- pmin(45L, pmax(10L, pmax(hdr_nchar, body_nchar)))
      col_dxa <- as.integer(round(total_dxa * (weights / sum(weights))))
      col_dxa[nc] <- total_dxa - sum(col_dxa[-nc])
    } else {
      col_dxa <- as.integer(round(total_dxa * (col_widths / sum(col_widths))))
      col_dxa[nc] <- total_dxa - sum(col_dxa[-nc])
    }

    if (is.null(col_align)) {
      col_align <- ifelse(vapply(df_val, is.numeric, logical(1)), "right", "left")
      if (nc == 2L) {
        col_align <- c("left", "right")
      } else if (nc > 2L) {
        col_align <- c("left", rep("right", nc - 1L))
      }
    }

    grid_cols <- paste(sprintf("<w:gridCol w:w=\"%d\"/>", col_dxa), collapse = "")

    hdr_cells <- character(nc)
    col_names <- xml_escape(colnames(df_val))
    for (j in seq_len(nc)) {
      hdr_cells[j] <- sprintf(
        paste0(
          "<w:tc>",
          "<w:tcPr>",
          "<w:tcW w:w=\"%d\" w:type=\"dxa\"/>",
          "<w:tcBorders>",
          "<w:top w:val=\"single\" w:sz=\"12\" w:space=\"0\" w:color=\"002244\"/>",
          "<w:bottom w:val=\"single\" w:sz=\"12\" w:space=\"0\" w:color=\"009FDA\"/>",
          "<w:left w:val=\"none\"/>",
          "<w:right w:val=\"none\"/>",
          "</w:tcBorders>",
          "<w:shd w:val=\"clear\" w:color=\"auto\" w:fill=\"002244\"/>",
          "<w:vAlign w:val=\"center\"/>",
          "</w:tcPr>",
          "<w:p>",
          "<w:pPr>",
          "<w:spacing w:before=\"60\" w:after=\"60\" w:line=\"240\" w:lineRule=\"auto\"/>",
          "<w:jc w:val=\"%s\"/>",
          "</w:pPr>",
          "<w:r>",
          "<w:rPr>",
          "<w:rFonts w:ascii=\"Arial\" w:hAnsi=\"Arial\" w:cs=\"Arial\"/>",
          "<w:b/>",
          "<w:bCs/>",
          "<w:color w:val=\"FFFFFF\"/>",
          "<w:sz w:val=\"19\"/>",
          "<w:szCs w:val=\"19\"/>",
          "</w:rPr>",
          "<w:t xml:space=\"preserve\">%s</w:t>",
          "</w:r>",
          "</w:p>",
          "</w:tc>"
        ),
        col_dxa[j],
        col_align[j],
        col_names[j]
      )
    }
    hdr_row <- paste0(
      "<w:tr><w:trPr><w:cantSplit/><w:tblHeader/></w:trPr>",
      paste(hdr_cells, collapse = ""),
      "</w:tr>"
    )

    body_rows <- character(nr)
    for (i in seq_len(nr)) {
      bg_fill <- if (i %% 2L == 0L) "F2F7FB" else "FFFFFF"
      btm_color <- if (i == nr) "002244" else "D9E2EC"
      btm_sz <- if (i == nr) "12" else "4"
      row_cells <- character(nc)
      for (j in seq_len(nc)) {
        raw_val <- as.character(df_val[[j]][i])
        val_str <- xml_escape(raw_val)
        cell_text_color <- if (identical(raw_val, "LOW")) {
          "C62828"
        } else if (identical(raw_val, "OK")) {
          "2E7D32"
        } else {
          "333333"
        }
        cell_bold_tag <- if (raw_val %in% c("LOW", "OK")) "<w:b/><w:bCs/>" else ""
        row_cells[j] <- sprintf(
          paste0(
            "<w:tc>",
            "<w:tcPr>",
            "<w:tcW w:w=\"%d\" w:type=\"dxa\"/>",
            "<w:tcBorders>",
            "<w:top w:val=\"none\"/>",
            "<w:bottom w:val=\"single\" w:sz=\"%s\" w:space=\"0\" w:color=\"%s\"/>",
            "<w:left w:val=\"none\"/>",
            "<w:right w:val=\"none\"/>",
            "</w:tcBorders>",
            "<w:shd w:val=\"clear\" w:color=\"auto\" w:fill=\"%s\"/>",
            "<w:vAlign w:val=\"center\"/>",
            "</w:tcPr>",
            "<w:p>",
            "<w:pPr>",
            "<w:spacing w:before=\"40\" w:after=\"40\" w:line=\"240\" w:lineRule=\"auto\"/>",
            "<w:jc w:val=\"%s\"/>",
            "</w:pPr>",
            "<w:r>",
            "<w:rPr>",
            "<w:rFonts w:ascii=\"Arial\" w:hAnsi=\"Arial\" w:cs=\"Arial\"/>",
            "%s",
            "<w:color w:val=\"%s\"/>",
            "<w:sz w:val=\"19\"/>",
            "<w:szCs w:val=\"19\"/>",
            "</w:rPr>",
            "<w:t xml:space=\"preserve\">%s</w:t>",
            "</w:r>",
            "</w:p>",
            "</w:tc>"
          ),
          col_dxa[j],
          btm_sz,
          btm_color,
          bg_fill,
          col_align[j],
          cell_bold_tag,
          cell_text_color,
          val_str
        )
      }
      body_rows[i] <- paste0(
        "<w:tr><w:trPr><w:cantSplit/></w:trPr>",
        paste(row_cells, collapse = ""),
        "</w:tr>"
      )
    }

    tbl_xml <- paste0(
      "<w:tbl xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
      "<w:tblPr>",
      "<w:tblW w:w=\"", total_dxa, "\" w:type=\"dxa\"/>",
      "<w:jc w:val=\"center\"/>",
      "<w:tblLayout w:type=\"fixed\"/>",
      "<w:tblCellMar>",
      "<w:top w:w=\"80\" w:type=\"dxa\"/>",
      "<w:left w:w=\"120\" w:type=\"dxa\"/>",
      "<w:bottom w:w=\"80\" w:type=\"dxa\"/>",
      "<w:right w:w=\"120\" w:type=\"dxa\"/>",
      "</w:tblCellMar>",
      "<w:tblLook w:val=\"04A0\" w:firstRow=\"1\" w:lastRow=\"0\" w:firstColumn=\"0\" w:lastColumn=\"0\" w:noHBand=\"0\" w:noVBand=\"1\"/>",
      "</w:tblPr>",
      "<w:tblGrid>", grid_cols, "</w:tblGrid>",
      hdr_row,
      paste(body_rows, collapse = ""),
      "</w:tbl>"
    )

    officer::body_add_xml(doc_obj, str = tbl_xml)
  }

  # ============================================================================
  # FRONT COVER PAGE
  # ============================================================================
  resolved_logo <- resolve_wbg_logo_path(logo_path)
  if (nzchar(resolved_logo)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::external_img(src = resolved_logo, width = 2.8, height = 0.71),
        fp_p = officer::fp_par(
          padding.top = 6,
          padding.bottom = 12,
          border.bottom = officer::fp_border(color = wbg_navy, width = 1.5)
        )
      )
    )
  }

  doc <- officer::body_add_par(
    doc,
    "SURVEY SOLUTIONS SPATIAL SAMPLING & LISTING SUITE",
    style = "WBG Cover Category"
  )
  doc <- officer::body_add_par(
    doc,
    "Second-Stage Spatial Sampling Report",
    style = "WBG Cover Title"
  )
  doc <- officer::body_add_par(
    doc,
    "Enumeration Area Building Listing, Sampling Process, and Selected Sample Documentation",
    style = "WBG Cover Subtitle"
  )

  doc <- officer::body_add_par(
    doc,
    paste0(
      "This report documents the compilation of the second-stage building sampling frame, ",
      "the spatial sampling methodology and parameters applied, and the resulting sample ",
      "of survey units drawn across enumeration areas."
    ),
    style = "WBG Body"
  )

  cover_meta <- list(
    c("Report Generated: ", report_time),
    c("Sampling Design: ", method_label),
    c("Cluster / Stratum Variable: ", cluster_var),
    c("Target Sample Size per Cluster: ", paste0(target_n, " units")),
    c("Minimum Distance Constraint: ", mindist_val),
    c("Random Seed: ", seed_val),
    c("Enumeration Areas in Frame: ", format(n_frame_clusters, big.mark = ",")),
    c("Total Listed Buildings in Frame: ", format(n_total_frame, big.mark = ",")),
    c("Average Polygon Grid Coverage: ", sprintf("%.1f%% (%d flagged low < 20%%)", mean_cov_pct, n_low_cov)),
    c("Total Selected Sample Units: ", format(n_total_sample, big.mark = ","))
  )
  for (item in cover_meta) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(item[1], prop = fp_label),
        officer::ftext(item[2], prop = fp_value)
      ),
      style = "WBG Callout"
    )
  }

  doc <- officer::body_add_par(
    doc,
    paste0(
      "Prepared with the World Bank Group / JDC Survey Solutions Spatial Listing Application ",
      "(susolisting v", as.character(utils::packageVersion("susolisting")), ")."
    ),
    style = "WBG Caption"
  )

  doc <- officer::body_add_break(doc)

  # ============================================================================
  # 1. EXECUTIVE SUMMARY
  # ============================================================================
  doc <- officer::body_add_par(doc, "1. Executive Summary", style = "WBG Heading 1")
  doc <- officer::body_add_par(
    doc,
    sprintf(
      paste0(
        "A second-stage sample of %s building units was drawn from a compiled listing frame ",
        "of %s buildings across %s enumeration areas (clusters), corresponding to an overall ",
        "sampling fraction of %.2f%%. The sample was selected using %s with random seed %s ",
        "and a target cluster sample size of %d units per enumeration area."
      ),
      format(n_total_sample, big.mark = ","),
      format(n_total_frame, big.mark = ","),
      format(n_frame_clusters, big.mark = ","),
      overall_rate,
      method_label,
      seed_val,
      target_n
    ),
    style = "WBG Body"
  )
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext("Key Design Diagnostics: ", prop = fp_label),
      officer::ftext(
        sprintf(
          "Mean design weight = %.3f (range: %.3f to %.3f); Weight CV = %.3f; Unequal weighting design effect (DEFF_w) = %.3f; Effective sample size (n_eff) = %.1f.",
          mean_wgt, min_wgt, max_wgt, cv_wgt, deff_wgt, n_eff
        ),
        prop = fp_value
      )
    ),
    style = "WBG Callout"
  )

  # ============================================================================
  # 2. SECOND-STAGE SAMPLING FRAME
  # ============================================================================
  doc <- officer::body_add_par(doc, "2. Second-Stage Sampling Frame", style = "WBG Heading 1")
  doc <- officer::body_add_par(
    doc,
    paste0(
      "The second-stage sampling frame was compiled from building locations digitized on ",
      "high-resolution satellite imagery within enumeration area (EA) boundaries. Each listed ",
      "structure is georeferenced in WGS84 (EPSG:4326) and linked to its parent enumeration ",
      "area identifier (`", frame_cluster_var, "`). Records with missing geographic coordinates ",
      "are excluded prior to spatial projection and sample selection."
    ),
    style = "WBG Body"
  )

  doc <- officer::body_add_par(
    doc,
    "Table 1. Second-Stage Sampling Frame Summary Statistics",
    style = "WBG Table Title"
  )
  frame_summary_df <- data.frame(
    Indicator = c(
      "Number of Enumeration Areas (Segments)",
      "Total Listed Buildings in Frame (N)",
      "Average Buildings per Enumeration Area",
      "Median Buildings per Enumeration Area",
      "Standard Deviation of Buildings per EA",
      "Minimum Buildings per Enumeration Area",
      "Maximum Buildings per Enumeration Area",
      "Average Polygon Spatial Grid Coverage (%)",
      "Minimum Polygon Spatial Grid Coverage (%)",
      "Flagged Low-Coverage Enumeration Areas (< 20%)"
    ),
    Value = c(
      format(n_frame_clusters, big.mark = ","),
      format(n_total_frame, big.mark = ","),
      sprintf("%.2f", mean(frame_counts_num)),
      sprintf("%.2f", stats::median(frame_counts_num)),
      sprintf("%.2f", if (length(frame_counts_num) > 1) stats::sd(frame_counts_num) else 0),
      format(min(frame_counts_num), big.mark = ","),
      format(max(frame_counts_num), big.mark = ","),
      sprintf("%.1f%%", mean_cov_pct),
      sprintf("%.1f%%", min_cov_pct),
      format(n_low_cov, big.mark = ",")
    ),
    stringsAsFactors = FALSE
  )
  doc <- add_wbg_table(doc, frame_summary_df)
  doc <- officer::body_add_par(
    doc,
    paste0(
      "Source: Compiled enumerator building listing records (cluster variable: `",
      frame_cluster_var,
      "`). Spatial coverage is computed on a 20x20 planar UTM grid clipped to each polygon boundary."
    ),
    style = "WBG Caption"
  )

  if (n_low_cov > 0L) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext("Low Spatial Coverage Flag (< 20%): ", prop = fp_label),
        officer::ftext(
          sprintf(
            "%d enumeration area(s) exhibited spatial grid coverage below the 20%% threshold (%s). Please verify whether sparse building coverage reflects true settlement geometry or incomplete listing.",
            n_low_cov,
            paste(low_cov_eas, collapse = ", ")
          ),
          prop = fp_value
        )
      ),
      style = "WBG Callout"
    )
  }

  # Table 2: Frame distribution by Enumeration Area
  doc <- officer::body_add_par(
    doc,
    "Table 2. Listed Building Frame Distribution and Spatial Grid Coverage by Enumeration Area",
    style = "WBG Table Title"
  )
  ea_names <- names(frame_counts_tbl)
  lon_ranges <- vapply(ea_names, function(cl) {
    if (!is.na(frame_lon_col) && frame_lon_col %in% names(frame_df)) {
      vals <- frame_df[[frame_lon_col]][frame_clusters == cl]
      sprintf("[%.4f, %.4f]", min(vals, na.rm = TRUE), max(vals, na.rm = TRUE))
    } else {
      "-"
    }
  }, character(1))
  lat_ranges <- vapply(ea_names, function(cl) {
    if (!is.na(frame_lat_col) && frame_lat_col %in% names(frame_df)) {
      vals <- frame_df[[frame_lat_col]][frame_clusters == cl]
      sprintf("[%.4f, %.4f]", min(vals, na.rm = TRUE), max(vals, na.rm = TRUE))
    } else {
      "-"
    }
  }, character(1))
  ea_cov_str <- vapply(ea_names, function(cl) {
    val <- cov_lookup[cl]
    if (!is.na(val)) sprintf("%.1f%%", val) else "-"
  }, character(1))
  ea_flag_str <- vapply(ea_names, function(cl) {
    val <- flag_lookup[cl]
    if (!is.na(val)) unname(val) else "LOW"
  }, character(1))

  frame_ea_df <- data.frame(
    `Enumeration Area` = ea_names,
    `Listed Buildings (N_h)` = as.integer(frame_counts_num),
    `Share of Frame (%)` = sprintf("%.2f%%", (frame_counts_num / max(1L, n_total_frame)) * 100),
    `Grid Coverage (%)` = ea_cov_str,
    `Coverage Status` = ea_flag_str,
    `Longitude Range` = lon_ranges,
    `Latitude Range` = lat_ranges,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  doc <- add_wbg_table(doc, frame_ea_df)
  doc <- officer::body_add_par(
    doc,
    "Note: Coordinates are reported in decimal degrees (WGS84 / EPSG:4326).",
    style = "WBG Caption"
  )

  # ============================================================================
  # 3. SAMPLING METHODOLOGY & PROCESS
  # ============================================================================
  doc <- officer::body_add_par(doc, "3. Sampling Methodology and Process", style = "WBG Heading 1")

  doc <- officer::body_add_par(doc, "3.1 Coordinate Projection and Stratification", style = "WBG Heading 2")
  doc <- officer::body_add_par(
    doc,
    sprintf(
      paste0(
        "Prior to sample selection, the WGS84 (EPSG:4326) building point coordinates were ",
        "dynamically projected into a planar Universal Transverse Mercator (UTM) coordinate ",
        "reference system (%s) computed from the centroid of the listing frame. ",
        "Stratification was defined by the enumeration area variable (`%s`), treating each ",
        "enumeration area as an independent second-stage sampling stratum."
      ),
      utm_epsg_val,
      cluster_var
    ),
    style = "WBG Body"
  )

  doc <- officer::body_add_par(doc, "3.2 Selection Algorithm and Inclusion Probabilities", style = "WBG Heading 2")
  algo_desc <- if (identical(sample_mode, "Balanced")) {
    paste0(
      "Within each enumeration area, units were selected via the Generalized Random ",
      "Tessellation Stratified (GRTS) algorithm implemented in `spsurvey::grts()`. ",
      "GRTS maps two-dimensional UTM coordinates into a one-dimensional hierarchical ",
      "address space via quadrant-recursive tessellation and selects a systematic ",
      "random sample, guaranteeing two-dimensional spatial balance across the enumeration area."
    )
  } else if (identical(sample_mode, "Balanced w. minimum distance")) {
     sprintf(
      paste0(
        "Within each enumeration area, units were selected via the Generalized Random ",
        "Tessellation Stratified (GRTS) algorithm (`spsurvey::grts()`) with an enforced ",
        "minimum planar separation distance of %s between sampled buildings. This prevents ",
        "immediate spatial clustering of selected buildings in high-density segments while ",
        "preserving overall spatial balance."
      ),
      mindist_val
    )
  } else {
    paste0(
      "Within each enumeration area, units were selected via Independent Random Sampling ",
      "(IRS) implemented in `spsurvey::irs()`, giving every listed building within an ",
      "enumeration area an equal and independent probability of selection without spatial ",
      "tessellation constraints."
    )
  }
  doc <- officer::body_add_par(doc, algo_desc, style = "WBG Body")

  doc <- officer::body_add_par(
    doc,
    paste0(
      "For each enumeration area h with N_h listed buildings and target sample size n_target, ",
      "the realized sample size is n_h = min(n_target, N_h). When N_h <= n_target, a take-all ",
      "census of listed buildings in that enumeration area is conducted (inclusion probability ",
      "pi_hi = 1.0 and base weight w_hi = 1.0). Otherwise, each unit i in enumeration area h ",
      "receives inclusion probability pi_hi = n_h / N_h and design weight w_hi = N_h / n_h."
    ),
    style = "WBG Body"
  )

  doc <- officer::body_add_par(
    doc,
    "Table 3. Sampling Process Parameters and Reproducibility Specification",
    style = "WBG Table Title"
  )
  spsurvey_ver <- if (requireNamespace("spsurvey", quietly = TRUE)) {
    as.character(utils::packageVersion("spsurvey"))
  } else {
    "Installed"
  }
  params_df <- data.frame(
    Parameter = c(
      "Sampling Mode",
      "Selection Function",
      "Cluster / Stratum Variable",
      "Target Sample Size per Cluster (n_target)",
      "Minimum Distance Constraint",
      "Random Number Generator Seed",
      "Planar Sampling CRS",
      "Output Coordinate System",
      "Spatial Sampling Engine"
    ),
    Specification = c(
      method_label,
      if (identical(sample_mode, "Random")) "spsurvey::irs()" else "spsurvey::grts()",
      cluster_var,
      paste0(target_n, " buildings per enumeration area"),
      mindist_val,
      seed_val,
      utm_epsg_val,
      "WGS84Geographic (EPSG:4326)",
      paste0("spsurvey v", spsurvey_ver)
    ),
    stringsAsFactors = FALSE
  )
  doc <- add_wbg_table(doc, params_df)
  doc <- officer::body_add_par(
    doc,
    "Note: Providing the same frame dataset, cluster variable, sampling mode, and random seed reproduces the exact sample.",
    style = "WBG Caption"
  )

  # ============================================================================
  # 4. RESULTING SECOND-STAGE SAMPLE
  # ============================================================================
  doc <- officer::body_add_par(doc, "4. Resulting Second-Stage Sample", style = "WBG Heading 1")
  doc <- officer::body_add_par(
    doc,
    sprintf(
      paste0(
        "The sampling procedure selected %s buildings across %s enumeration areas. ",
        "Design weights (`wgt`) sum to %.1f across the sample (matching the total frame size ",
        "of %s buildings). The coefficient of variation of the second-stage design weights is ",
        "%.3f, yielding a Kish unequal weighting design effect of DEFF_w = %.3f and an ",
        "effective sample size of %.1f units."
      ),
      format(n_total_sample, big.mark = ","),
      format(n_sample_clusters, big.mark = ","),
      sum(wgt_vec, na.rm = TRUE),
      format(n_total_frame, big.mark = ","),
      cv_wgt,
      deff_wgt,
      n_eff
    ),
    style = "WBG Body"
  )

  doc <- officer::body_add_par(
    doc,
    "Table 4. Resulting Sample Summary and Design Weight Diagnostics",
    style = "WBG Table Title"
  )
  sample_summary_df <- data.frame(
    Indicator = c(
      "Sampled Enumeration Areas (Segments)",
      "Total Selected Sample Units (n)",
      "Overall Second-Stage Sampling Rate (%)",
      "Sum of Second-Stage Design Weights",
      "Average Sampling Weight (mean wgt)",
      "Minimum Sampling Weight (min wgt)",
      "Maximum Sampling Weight (max wgt)",
      "Sampling Weight Coefficient of Variation (CV)",
      "Unequal Weighting Design Effect (DEFF_w)",
      "Effective Sample Size (n_eff)"
    ),
    Value = c(
      format(n_sample_clusters, big.mark = ","),
      format(n_total_sample, big.mark = ","),
      sprintf("%.2f%%", overall_rate),
      sprintf("%.2f", sum(wgt_vec, na.rm = TRUE)),
      sprintf("%.3f", mean_wgt),
      sprintf("%.3f", min_wgt),
      sprintf("%.3f", max_wgt),
      sprintf("%.3f", cv_wgt),
      sprintf("%.3f", deff_wgt),
      sprintf("%.1f", n_eff)
    ),
    stringsAsFactors = FALSE
  )
  doc <- add_wbg_table(doc, sample_summary_df)
  doc <- officer::body_add_par(
    doc,
    "Note: DEFF_w = 1 + CV(wgt)^2; Effective sample size n_eff = n / DEFF_w.",
    style = "WBG Caption"
  )

  # Visual Figure: Frame vs. Sampled Buildings per Enumeration Area
  doc <- officer::body_add_par(
    doc,
    "Figure 1. Comparison of Listed Frame Buildings and Selected Sample Units by Enumeration Area",
    style = "WBG Table Title"
  )
  all_clusters <- union(names(frame_counts_tbl), names(sample_counts_tbl))
  frame_by_cl <- as.numeric(frame_counts_tbl[all_clusters])
  frame_by_cl[is.na(frame_by_cl)] <- 0
  samp_by_cl <- as.numeric(sample_counts_tbl[all_clusters])
  samp_by_cl[is.na(samp_by_cl)] <- 0

  # Show up to 25 clusters in the chart for visual legibility
  max_plot_clusters <- min(length(all_clusters), 25L)
  plot_clusters <- all_clusters[seq_len(max_plot_clusters)]
  plot_mat <- rbind(
    Frame = frame_by_cl[seq_len(max_plot_clusters)],
    Sample = samp_by_cl[seq_len(max_plot_clusters)]
  )
  colnames(plot_mat) <- plot_clusters

  tmp_plot <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_plot), add = TRUE)
  grDevices::png(filename = tmp_plot, width = 6.2, height = 3.4, units = "in", res = 300)
  graphics::par(mar = c(6, 4.5, 2.5, 1.5), family = "sans", cex.axis = 0.8)
  y_max <- max(plot_mat, na.rm = TRUE) * 1.22
  graphics::barplot(
    plot_mat,
    beside = TRUE,
    col = c(wbg_navy, wbg_cyan),
    border = NA,
    ylim = c(0, max(1, y_max)),
    las = 2,
    cex.names = 0.8,
    ylab = "Number of Buildings",
    main = "Frame vs. Sampled Buildings by Enumeration Area",
    col.main = wbg_navy,
    col.lab = wbg_text
  )
  graphics::grid(nx = NA, ny = NULL, col = "#D9E2EC", lty = "dotted")
  graphics::legend(
    "topright",
    legend = c("Listed Frame (N_h)", "Selected Sample (n_h)"),
    fill = c(wbg_navy, wbg_cyan),
    border = NA,
    bty = "n",
    cex = 0.85
  )
  grDevices::dev.off()

  doc <- officer::body_add_img(
    doc,
    src = tmp_plot,
    width = 6.2,
    height = 3.4
  )
  doc <- officer::body_add_par(
    doc,
    "Note: Bars compare total listed buildings (WBG Solid Blue #002244) against drawn sample units (WBG Cyan #009FDA) per enumeration area.",
    style = "WBG Caption"
  )

  # Table 5: Cluster-Level Allocation & Weights
  doc <- officer::body_add_par(
    doc,
    "Table 5. Cluster-Level Frame Size, Sample Allocation, and Design Weights",
    style = "WBG Table Title"
  )
  wgt_by_cl <- vapply(all_clusters, function(cl) {
    vals <- wgt_vec[sample_clusters == cl]
    if (length(vals) > 0) sprintf("%.3f", mean(vals, na.rm = TRUE)) else "-"
  }, character(1))
  centroid_by_cl <- vapply(all_clusters, function(cl) {
    idx <- which(sample_clusters == cl)
    if (length(idx) > 0 && !is.na(samp_lon_col) && !is.na(samp_lat_col)) {
      sprintf(
        "(%.4f, %.4f)",
        mean(sample_df[[samp_lon_col]][idx], na.rm = TRUE),
        mean(sample_df[[samp_lat_col]][idx], na.rm = TRUE)
      )
    } else {
      "-"
    }
  }, character(1))

  alloc_cov_str <- vapply(all_clusters, function(cl) {
    val <- cov_lookup[cl]
    if (!is.na(val)) sprintf("%.1f%%", val) else "-"
  }, character(1))
  alloc_flag_str <- vapply(all_clusters, function(cl) {
    val <- flag_lookup[cl]
    if (!is.na(val)) unname(val) else "LOW"
  }, character(1))

  cluster_alloc_df <- data.frame(
    `Enumeration Area` = all_clusters,
    `Frame Size (N_h)` = as.integer(frame_by_cl),
    `Grid Coverage (%)` = alloc_cov_str,
    `Coverage Status` = alloc_flag_str,
    `Sample Size (n_h)` = as.integer(samp_by_cl),
    `Sampling Rate (%)` = sprintf("%.2f%%", ifelse(frame_by_cl > 0, (samp_by_cl / frame_by_cl) * 100, 0)),
    `Design Weight (w_h)` = wgt_by_cl,
    `Sample Centroid (Lon, Lat)` = centroid_by_cl,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  doc <- add_wbg_table(doc, cluster_alloc_df)
  doc <- officer::body_add_par(
    doc,
    "Source: Second-stage sample generated by susolisting via spsurvey.",
    style = "WBG Caption"
  )

  # Save the Word document to disk (`print.rdocx` writes to `target` when specified)
  print(doc, target = file)
  invisible(file)
}
