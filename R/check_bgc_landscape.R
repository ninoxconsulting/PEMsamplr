#' Generate a summary table of binned landscape classes per BGC
#'
#' @param bec A `SpatRast` or path to BEC raster generated at a landscape (25m)
#'      scale. This output is derived from the create_bgc_template().
#' @param binned_landscape A `SpatRast` with the landscape binned. This is the
#'      output of the [create_binned_landscape()] function.
#' @param plot a logical if the plot is to be returned. Default is true
#' @return A dataframe with summary of landsclass class and BEC unit.
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
    bec = fs::path(
      PEMprepr::read_fid()$dir_1020_covariates$path_rel,
      "25m", "bec.tif"
    ),
    binned_landscape = fs::path(
      PEMprepr::read_fid()$dir_1020_covariates$path_rel,
      "25m", "modules", "landscape_binned.tif"
    ),
    plot = TRUE) {
  # testing
  # bec <- fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "25m", "bec.tif")
  # binned_landscape <- landscapes
  # binned_landscape = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,"25m","modules_landscape")


  bec <- PEMprepr::read_spatrast_if_necessary(bec)

  binned_landscape <- PEMprepr::read_spatrast_if_necessary(binned_landscape)

  if (!isTRUE(terra::compareGeom(bec, binned_landscape))) {
    cli::cli_abort("{.var bec} must match spatial extent of landscapes raster stack")
  } else {
    # stack
    rout <- c(bec, binned_landscape)
    routdf <- as.data.frame(rout)

    routdf <- stats::na.omit(routdf)

  if(plot){
    plot1 <- ggplot2::ggplot(routdf, ggplot2::aes(routdf$landscape)) +
      ggplot2::geom_histogram() +
      ggplot2::facet_wrap(~MAP_LABEL)

    print(plot1)
  }
    return(routdf)
  }
}
