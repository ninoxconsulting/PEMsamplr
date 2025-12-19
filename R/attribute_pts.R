#' Add covariate predictor data to training point file
#'
#' @param data_pts a file gpkg of training points
#' @param cov_dir folder containing covariate files
#' @param write_output A `logical`if the sf object be written to disk?
#'     If `TRUE` (default), will write to `out_dir` under the appropriate resolution subfolder.
#' @param out_dir A character string of path which points to output location. A default
#'    location and name are applied in line with standard workflow.
#' @param out_name A character string of the output file name. Default is `allpoints_att.gpkg`
#' @return an sf object
#' @export
#' @examples
#' \dontrun{
#' tpoints_ne <- attribute_points(dat_pts, cov_dir)
#' }
attribute_points <- function(data_pts,
                             cov_dir,
                             write_output = TRUE,
                             out_dir = fs::path(PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel, out_name = "allpoints.gpkg"),
                             out_name = "allpoints_att.gpkg") {
  data_pts <- PEMprepr:::read_sf_if_necessary(data_pts)

  if (!inherits(cov_dir, "character") || !fs::dir_exists(cov_dir)) {
    cli::cli_abort("{.var in_dir} does not exist, please check the path to your
                 landscape covariates is correct")
  }
  cli::cli_alert_warning("Hold tight - this might take awhile depending on number of points to attribute")

  # get list of raster
  rastlist <- list.files(cov_dir, pattern = ".sdat$|.tif$", recursive = T, full.names = T)
  ancDat <- terra::rast(rastlist)

  # check if multiple names are used
  if (length(names(ancDat)) > length(unique(names(ancDat)))) {
    cli::cat_line()
    cli::cli_alert_warning("Duplicated names are contained within the raster stack and will be removed")
    ancDat <- ancDat[[!duplicated(names(ancDat))]]
  }
  atts <- terra::extract(ancDat, data_pts)|> dplyr::select(-.data$ID)
  att_all <- dplyr::bind_cols(sf::st_as_sf(data_pts), atts)

  # write out point file
  if (write_output) {
    out_loc <- fs::path(out_dir, out_name)
    # if file exists
    if (fs::file_exists(out_loc)) {
      cli::cat_line()
      cli::cli_alert_warning("file already exists at {.var {out_dir}}, this file will be overwriten")
    }
    sf::st_write(att_all, out_loc, driver = "GPKG", append = FALSE)
    cli::cat_line()
    cli::cli_alert_success("field data formatted and written to {.var {out_dir}}")
  }
  return(att_all)
}
