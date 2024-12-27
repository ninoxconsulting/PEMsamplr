#' Convert Line segments or points to rasterized training points
#'
#' @param processed_lines A `sf` object of all attributed line segments or points
#' @param trast A `SpatRast` or path to file with resolution matching the modelling resolution.
#' @param neighbours A `logical` if adjacent neighbouring cells should be added. Default is FALSE.
#' @param buffer A `numeric`to determine the extend in meters the line segments will be expanded to.
#' Default is half of the raster template resolution 2.5m for a 5m resolution raster template
#' @param write_output A `logical`if the sf object be written to disk?
#'     If `TRUE` (default), will write to `out_dir` under the appropriate resolution subfolder.
#' @param out_dir A character string of path which points to output location. A default
#'    location and name are applied in line with standard workflow.
#' @param out_name A character string of the output file name. Default is `allpoints.gpkg`
#' @return sf object points based on raster reolution
#' @export
#' @examples
#' \dontrun{
#' clean_tracks <- convert_to_pts(
#'   processed_lines = fs::path(PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel,
#'   "proc_s1_transects5.gpkg"),
#'   trast = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "5m", "template.tif"),
#'   buffer = 2.5,
#'   neighbours = FALSE,
#'   write_output = TRUE,
#'   out_dir = PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel,
#'   out_name = "allpoints.gpkg")
#' }
convert_lines_pts <- function(processed_lines = fs::path(PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel, "proc_s1_transects5.gpkg"),
                              trast,
                              buffer = 2.5,
                              neighbours = FALSE,
                              write_output = TRUE,
                              out_dir = PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel,
                              out_name = "allpoints.gpkg") {
  # check processed_transects is path or spatvect
  processed_lines <- PEMprepr:::read_sf_if_necessary(processed_lines)

  # check trast is path or spatrast
  trast <- PEMprepr:::read_spatrast_if_necessary(trast)
  tname <- names(trast)

  # check if ID column exists
  if ("ID" %in% colnames(processed_lines) == FALSE) {
    processed_lines <- processed_lines |>
      dplyr::mutate(ID = seq(1, length(processed_lines$order), 1))
  }

  processed_transects_id <- sf::st_drop_geometry(processed_lines)

  geom_type <- as.character(unique(sf::st_geometry_type(processed_lines, by_geometry = TRUE)))

  if (geom_type %in% c("LINESTRING")) {
    # for lines
    lBuff <- sf::st_buffer(processed_lines, dist = buffer, endCapStyle = "FLAT", joinStyle = "MITRE")
    lBuff <- sf::st_cast(lBuff, "MULTIPOLYGON")
  } else {
    # for points
    lBuff <- sf::st_buffer(processed_lines, dist = buffer)
    lBuff <- sf::st_cast(lBuff, "MULTIPOLYGON")
  }

  # extract the XY values of raster cells where crossed lines
  xys <- terra::extract(trast, terra::vect(lBuff), xy = TRUE)

  raster_points_xy <- xys |>
    sf::st_as_sf(coords = c("x", "y"), crs = 3005) |>
    terra::merge(processed_transects_id) |>
    dplyr::select(c(-"ID", -tname))

  # add slice and tid (transect id)
  allpts <- raster_points_xy |>
    dplyr::mutate(tid = tolower(gsub("_[[:alpha:]].*", "", .data$transect_id))) |>
    dplyr::mutate(slice = sub(".*(?=.$)", "", gsub("\\..*", "", .data$tid), perl = T))

  # add neighbours if selected
  if (neighbours) {
    sf::st_geometry(allpts) <- "geom"
    cli::cat_line()
    cli::cli_alert_warning("generating neighbouring points")

    dat_pts <- allpts |> dplyr::mutate(ptsID = dplyr::row_number())

    dat_atts <- sf::st_drop_geometry(dat_pts)

    pts <- terra::vect(dat_pts)
    cellNums <- terra::cells(trast, pts)
    cell_lookup <- tibble::tibble(ID = pts$ptsID, cell = cellNums)

    adjCells <- terra::adjacent(trast, cells = cellNums[, 2], directions = "queen", include = TRUE) |>
      tibble::as_tibble(.name_repair = "minimal")  |>
      dplyr::rename_with(~ c("Orig", paste("Adj", 1:8, sep = "")))  |>
      dplyr::mutate(ID = dplyr::row_number())

    adjLong <- adjCells  |>
      tidyr::pivot_longer(cols = !("ID") , names_to = "Position", values_to = "CellNum") |>
      dplyr::arrange("ID", "Position")

    terra::values(trast) <- 1:terra::ncell(trast)
    cellnums <- 1:terra::ncell(trast)
    trast[!cellnums %in% adjLong$CellNum] <- NA

    pts <- terra::as.points(trast, values = TRUE, na.rm = TRUE)  |>
      sf::st_as_sf()  |>
      tibble::as_tibble(.name_repair = "minimal")   |>
      dplyr::rename(CellNum = 1)

    allPts <- pts  |>
      dplyr::left_join(adjLong, by = "CellNum")  |>
      dplyr::left_join(dat_atts, by = c("ID" = "ptsID"))  |>
      sf::st_as_sf()  |>
      dplyr::select(-"CellNum",-"ID")

  } else {

    allpts$Postition <- "Orig"
  }

  if (write_output) {
    out_loc <- fs::path(out_dir, out_name)

    # if file exists
    if (fs::file_exists(out_loc)) {
      cli::cat_line()
      cli::cli_alert_warning("file already exists at {.var {out_dir}}, this file will be overwriten")
    }

    sf::st_write(allpts, out_loc, driver = "GPKG", append = FALSE)
    cli::cat_line()
    cli::cli_alert_success("field data formatted and written to {.var {out_dir}}")
  }

  return(allpts)
}
