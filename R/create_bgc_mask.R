#' Create cost no sample areas per bec zone
#'
#' Creates and applies a mask of the bec zone to the final cost layer
#'
#' @param vec_dir text string with folder location of base vector layers
#' @param cost_masked A `SpatRast` of cost layer with high cost penalty and
#'      exclusions applied.
#' @param out_dir A `character` or path which points to input location
#'      of . A default location and name are applied in line with standard workflow.
#' @return path to the output directory where files are written (invisibly).
#' @param ... Additional options passed on to `...` in [terra::writeRaster()]
#' @inheritParams terra::writeRaster
#' @export
#'
#' @examples
#' \dontrun{
#' create_bgc_mask <- function(vec_dir, cost_final, out_dir)
#'}
#'
create_bgc_mask <- function(
   vec_dir = fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel),
   cost_masked,
   out_dir = fs::path(PEMprepr::read_fid()$dir_201010_inputs$path_rel),
   overwrite = TRUE,
   ...){

  if (!inherits(vec_dir, "character") || !fs::dir_exists(vec_dir)) {
    cli::cli_abort("{.var vec_dir} must be a directory path")
  }

  cost_masked <- PEMprepr:::read_spatrast_if_necessary(cost_masked)

  fs::dir_create(out_dir, recurse = TRUE)

  if (!fs::file_exists(fs::path(vec_dir, "bec.gpkg"))) {
    cli::cli_abort(
      "bec.gpkg does not exist in {.var vec_dir}. Please check the function
        create_base_vectors() ran correctly or add this manually"
    )
  }
  bec <- sf::st_read(fs::path(vec_dir, "bec.gpkg"))

  fs::dir_create(out_dir, recurse = TRUE)

    boi <- unique(bec$MAP_LABEL)

    for (b in boi) {

      subzone <- bec |>

        dplyr::filter(.data$MAP_LABEL == {{ b }})

      subzone_buff <- sf::st_buffer(subzone, dist = -150)

      boi_mask <- terra::mask(cost_masked, subzone_buff)
      names(boi_mask) <- "cost"
      boi_mask <- 1 + (boi_mask * 0)
      output_raster <- fs::path(out_dir, paste0(b, "_exclude_mask.tif"))

      # if (fs::file_exists(output_raster)) {
      #   cli::cli_alert_warning(
      #     "BGC mask {.var {output_raster}} already exists"
      #   )
      # }

      terra::writeRaster(boi_mask, fs::path(output_raster), overwrite = overwrite, ...)

      output_gpkg <- fs::path(out_dir, paste0(b, "_exclude_poly.gpkg"))

      # if (fs::file_exists(output_gpkg)) {
      #   cli::cli_alert_warning(
      #     "BGC mask {.var {output_raster}} already exists"
      #   )
      # }

      mask_poly_boi <- terra::as.polygons(boi_mask, dissolve = TRUE)
      mask_poly_boi <- sf::st_as_sf(mask_poly_boi)

      sf::st_write(mask_poly_boi, output_gpkg, delete_dsn = TRUE)

    }
    invisible(out_dir)

}
