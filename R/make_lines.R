#' Convert point or tracklog to line segments
#'
#' Converts standardized points to line segments.
#' Currently, this function generates points to transect by converting the _POINT_
#' feature data to LINES sf features.
#' A placeholder has been made to generate transect lines via the transect method created by M. Coghill.
#'
#' @param points A `sf`object with standardised attribute fields
#' @param transect_layout A `sf` object with simplified transect layout.
#' @param method Multiple methods to be made available:
#' - _pts2lines_ takes a sf POINT object and converts it to lines.
#' _This is currently the only function available
#' - _tracklog_ uses the tracklog and the sample points to generate the lines. _Not implemented yet_.
# #' @param sortby Field in the points data to sort the points by.  Defaults to  _"none"_ (i.e. assumes no sorting of data)
#' @param buffer A numeric value for buffer distance around transect layout a default of 20m.
#' This is the buffer distance to place around the transect.
#' Waypoints outside this distance will not be considered.
#' @param write_output should the sf object be written to disk?
#'     If `TRUE` (default), will write to `out_dir` under the appropriate resolution subfolder.
#' @param out_dir A character string of path which points to output location. A default
#'    location and name are applied in line with standard workflow.
#' @param out_name A character string of the output file name. Default is `proc_s1_transects.gpkg`
#' @export
#' @examples
#' \dontrun{
#' processed_lines <- make_lines(points, transect_layout, buffer = 20, method = "pts2lines")
#' }
make_lines <- function(points = fs::path(PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel, "s1_points_raw.gpkg"),
                       # tracks = fs::path(PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel, "s1_tracks_raw.gpkg"),
                       transect_layout = fs::path(PEMprepr::read_fid()$dir_20104020_transect$path_rel,"transect_layout.gpkg"),
                       method = "pts2lines",
                       buffer = 20,
                       write_output = TRUE,
                       out_dir = fs::path(PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel),
                       out_name = "proc_s1_transects.gpkg") {


  # check the input files
  points <- PEMprepr:::read_sf_if_necessary(points)

  # check method is valid
  if (!method %in% c("pts2lines", "tracklog")) {
    cli::cli_abort("{.var method} must be one of 'pts2lines' or 'tracklog'")
  }

  # check transect_inputs
  transect_layout <- PEMprepr:::read_sf_if_necessary(transect_layout)

  # add check for buffer
  if (!is.numeric(buffer) || buffer < 0) {
    cli::cli_abort("{.var buffer} must be numeric")
  }

  # add check for out_dir
  if (!inherits(out_dir, "character") || !fs::dir_exists(out_dir)) {
    cli::cli_abort("{.var out_dir} must be a directory path")
  }

  PROJ <- sf::st_crs(points)


  if (method == "pts2lines") {
    ## Transects

    planT <- transect_layout |>
      dplyr::mutate(TID = dplyr::row_number()) |>
      sf::st_buffer(buffer) |>
      dplyr::select("TID")

    ## Spatial join attributes
    GPSPoints <- sf::st_join(points, planT)

    GPSPoints <- GPSPoints |> tibble::rowid_to_column("ID")

    ## convert GPSPoints to a table for manipulation
    GPSPoints <- cbind(GPSPoints, sf::st_coordinates(GPSPoints))
    GPSPoints <- GPSPoints |> sf::st_drop_geometry()

    # iterate through transect id
    transects_id <- unique(GPSPoints$TID)

    all_lines <- purrr::map(transects_id, function(x) {
      # x <- transects_id[1] # testing line

      GPSPoints_transect <- GPSPoints |>
        dplyr::filter(.data$TID == x)

      ## Define the Line Start and End Coordinates and Add XY coordinates as

      GPSPoints_transect |>
        dplyr::mutate(
          Xend = dplyr::lead(.data$X),
          Yend = dplyr::lead(.data$Y)
        ) |>
        dplyr::filter(!is.na(.data$Yend)) |>
        dplyr::rowwise(.data$ID) |>
        dplyr::mutate(geometry = sf::st_sfc(
          sf::st_linestring(
            x = matrix(c(.data$X, .data$Xend, .data$Y, .data$Yend), ncol = 2)
          )
        )) |>
        sf::st_sf(crs = PROJ)
    }) |> dplyr::bind_rows()


    ## Need to remove excess lines -- currently there are lines that run between the plots
    within <- lengths(sf::st_within(all_lines, planT)) > 0
    all_lines <- all_lines[within, ]

    all_lines <- sf::st_make_valid(all_lines)

    all_lines <- all_lines |> dplyr::select(-c("X", "Y", "TID", "ID", "Xend", "Yend"))
  } else if (method == "tracklog") {
    cli::cli_alert_info("Tracklog method not implemented yet")
    # see file: placeholder_make_lines_tracklog_method.R for details. As of Jan 20205 this has not been updated
    # to match the current workflow.
  }

  if (write_output) {
    out_loc <- fs::path(out_dir, out_name)

    if (fs::file_exists(out_loc)) {
      cli::cli_alert_warning("file already exists at {.var {out_dir}}, this file will be overwriten")
    }

    sf::st_write(all_lines, out_loc, driver = "GPKG", append = FALSE)
    cli::cli_alert_success("processed transects written to {.var {out_dir}}")
  }

  return(all_lines)
}
