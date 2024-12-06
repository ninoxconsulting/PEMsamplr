#' Add exclusion areas to the cost layer for sampling
#'
#' Creates and applies a mask to permanently changed areas where sample is not be conducted.
#' This includes (lakes, and permanently changed landscape i.e roads)
#'
#' @param vec_dir A `character` string or path which points to base vector
#'    layers created using create_base_vectors(). A default location and name
#'    are applied in line with standard workflow.
#' @param cost A `SpatRast` or path to cost layer with high cost applied.
#'    This is created using the create_cost_penality() function. A default
#'    location and name is applied in line with standard workflow.
#' @param buffer A `numeric` values in meters representing the buffer distance to
#'    be excluded surrounding lakes
#' @param out_dir A `character` or path which points to input location
#'      of . A default location and name are applied in line with standard workflow.
#' @param write_output A `logical` should the cost_penalty spatRaster be
#'     written to disk? If `TRUE` (default), will write to `out_dir`.
#' @return A `SpatRast`**` A masked cost raster layer
#' @export
#' @examples
#' \dontrun{
#' cost_masked <- create_cost_nosample(vec_dir = vec_dir,
#'                                    cost = cost_penalty,
#'                                  buffer = 150,
#'                                  write_output = FALSE)
#'
#' }
#'
create_cost_exclusion <- function(vec_dir = fs::path(PEMprepr::read_fid()$dir_1010_vector$path_abs),
                                  cost,
                                  buffer = 150,
                                  out_dir = fs::path(PEMprepr::read_fid()$dir_201010_inputs$path_abs),
                                  write_output = TRUE) {
  if (!inherits(vec_dir, "character") || !fs::dir_exists(vec_dir)) {
    cli::cli_abort("{.var vec_dir} must be a directory path")
  }

  if (inherits(cost, c("character"))) {
    cost <- terra::rast(cost)
  } else if (!inherits(cost, c("SpatRaster"))) {
    cli::cli_abort("{.var cost} must be a SpatRaster or a path to a file")
  }

  if (fs::file_exists(fs::path(vec_dir, "water.gpkg"))) {
    water <- sf::st_read(file.path(vec_dir, "water.gpkg")) |>
      dplyr::filter("WATERBODY_TYPE" != "W")
    water_buff <- sf::st_buffer(water, dist = buffer)
    cli::cat_line()
    cli::cli_alert_success(
      "water bodies excluded and buffered added to cost penalty layer"
    )
  } else {
    cli::cli_alert_warning(
      "water.gpkg does not exist in {.var vec_dir}. Please check and add this
      file if water is to be excluded"
    )
  }

  if (fs::file_exists(fs::path(vec_dir, "road_network.gpkg"))) {
    roads <- sf::st_read(file.path(vec_dir, "road_network.gpkg")) |>
      sf::st_transform(3005)
    roads_buff <- sf::st_buffer(roads, dist = 175)
    cli::cat_line()
    cli::cli_alert_success(
      "roads excluded and buffered added to cost penalty layer"
    )
  } else {
    cli::cli_alert_warning(
      "road_network.gpkg does not exist in {.var vec_dir}. Please check and add
    this file if roads is to be excluded"
    )
  }

  # create accumulated sample cost mask
  sample_cost_masked <- terra::mask(cost, roads_buff, inverse = TRUE) |>
    terra::mask(water_buff, inverse = TRUE)

  if (write_output) {
    if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir, recurse = TRUE)
      cli::cli_alert_warning(
        "write out folder does not exist, creating at location {.path {out_dir}}"
      )
    }

    output_file <- fs::path(fs::path_abs(out_dir), "cost_exclude.tif")

    if (fs::file_exists(output_file)) {
      cli::cli_alert_warning(
        "Cost penalty raster already exists in {.path {output_file}}"
      )
    }

    terra::writeRaster(sample_cost_masked, fs::path(output_file), overwrite = TRUE)
    cli::cat_line()
    cli::cli_alert_success(
      "Cost penalty Raster written to {.path {output_file}}"
    )
  }

  return(sample_cost_masked)
}
