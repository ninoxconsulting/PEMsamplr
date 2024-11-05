#' Generate a summary table of binned landscape classes per BGC
#'
#' @param bec A `SpatRast` of path to BEC raster generated at a landscape (25m)
#'      scale. This output is derived from the create_bgc_template().
#' @param binned_landscape A `SpatRast` with the landscape binned. This is the
#'      output of the [create_binned_landscape()] function.
#'
#' @return A dataframe with summary of landsclass class and BEC unit.
#'        A plot is also returned.
#' @export
#'
#' @examples
#' \dontrun{
#' check_bgc_landscapes = create_bgc_template(
#'   bec = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,
#'   "25m", "bec.tif")
#'   binned_landscape = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,
#'   "25m","modules_landscape")
#' }

check_bgc_landscapes <- function(
    bec = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,
                   "25m", "bec.tif"),
    binned_landscape = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,
                                "25m","modules_landscape", "landscape_binned.tif")
    ) {

  # testing
  #bec <- fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "25m", "bec.tif")
  #binned_landscape <- landscapes
  # binned_landscape = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,"25m","modules_landscape")

  if (inherits(bec, c("character"))) {
    bec <- terra::rast(bec)
  } else if (!inherits(bec, c("SpatRaster"))) {
    cli::cli_abort("{.var bec} must be a SpatRaster or a path to a file")
  }

  if (inherits(binned_landscape, c("character"))) {
    binned_landscape <- terra::rast(binned_landscape)
  } else if (!inherits(binned_landscape, c("SpatRaster"))) {
    cli::cli_abort("{.var binned_landscape} must be a SpatRaster or a path to a file")
  }

  # stack
  rout <- c(bec, binned_landscape)
  routdf <- as.data.frame(rout)

  routdf <- stats::na.omit(routdf)

 ggplot2::ggplot(routdf, ggplot2::aes(routdf$landscape)) +
  ggplot2::geom_histogram() +
  ggplot2::facet_wrap(~MAP_LABEL)

  return(routdf)
}
