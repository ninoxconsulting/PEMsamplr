#' Create sample plan including spatial and tracking files
#'
#' This function used the selected clhs outputs and generates a full sampleplan
#' spatial data in geopackage output along with a csv tracking sheet per BEC
#' unit
#'
#' @param clhs_set A `character` containing the names of clhs files to be converted
#'  to full sample plan. One or more BEC units can be selected for a sampleplan
#' @param clhs_dir A path to the location of files specified in `clhs_set` above.
#'  Default location is based on standard workflow.
#' @param mask_dir A path to the location of masked bec spatial files for the BEC
#'  units specified by the clhs_set. Default location is based on standard workflow.
#' @param cost_dir A path to the location of final cost output `spatRast`.
#'  Default location is based on standard workflow.
#' @param out_dir A path to the location in which the output sample plan will be
#' saved. Default location is based on standard workflow.
#'
#' @return path to the output directory where files are written (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' generate_sampleplan (
#'   clhs_set <- c("ICHmc2_clhs_sample_3.gpkg", "ICHmc1_clhs_sample_3.gpkg"),
#'   clhs_dir = PEMprepr::read_fid()$dir_20103010_clhs$path_abs,
#'   mask_dir = PEMprepr::read_fid()$dir_201020_masks$path_abs,
#'   cost_dir = PEMprepr::read_fid()$dir_201010_inputs$path_abs,
#'   out_dir = PEMprepr::read_fid()$dir_20103020_review$path_rel)
#' }
create_sampleplan <- function(clhs_set,
                                clhs_dir = PEMprepr::read_fid()$dir_20103010_clhs$path_abs,
                                mask_dir = PEMprepr::read_fid()$dir_201020_masks$path_abs,
                                cost_dir = PEMprepr::read_fid()$dir_201010_inputs$path_abs,
                                out_dir = PEMprepr::read_fid()$dir_20103020_review$path_rel){


  cost <- terra::rast(fs::path(cost_dir, "cost_final.tif" ))

  make_sampleplan <- function(clhs_set){

    boi <- stringr::str_extract(clhs_set, "[^_]+")
    sample_points <- sf::st_read(fs::path(clhs_dir, pattern = clhs_set), quiet = T)

    mask_poly <- sf::st_read(fs::path(mask_dir, pattern = paste0(boi,"_exclude_poly.gpkg")), quiet = T)

    build_site_transects(sample_points, cost, mask_poly, centroid_distance = 400, out_dir)

  }

  purrr::map(clhs_set, make_sampleplan)

  cli::cli_alert_success("sample plan generated for {.var clhs_set}")

  allpoints <- grep("points_all", sf::st_layers(file.path(out_dir, "s1_sampling.gpkg"))$name, value = T)
  boi <- stringr::str_extract(allpoints, "[^_]+")

  allpoints <- grep("points_all", sf::st_layers(file.path(out_dir, "s1_sampling.gpkg"))$name, value = T)
  boi <- stringr::str_extract(allpoints, "[^_]+")

  for (ii in 1:length(boi)) {
    #ii = 1
    b <- boi[ii]
    points <- sf::st_read(file.path(out_dir, "s1_sampling.gpkg"), layer = paste0(b,"_points_all"), quiet = T)
    pointsout <- points |>
      cbind(sf::st_coordinates(points)) |>
      dplyr::select("bgc","id", "rotation", "X","Y") |>
      sf::st_drop_geometry() |>
      dplyr::mutate(Surveyor = "", Date_Completed = "", Transect_comment = "")

    utils::write.csv(pointsout, fs::path(out_dir, paste0(b, "_tracking_sheet.csv")))

    cli::cli_alert_success("tracking sheet exported to {.path {out_dir}}")

  }

  invisible(out_dir)
}
