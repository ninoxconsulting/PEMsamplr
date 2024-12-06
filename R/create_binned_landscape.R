#' Create a binned landscape
#'
#' Create a Spatrast object based on the binned landscape variables to assess
#' the landscape environmental space.
#'
#' @param in_dir A `character` or path which points to input location of
#'      landscape level `spatRaster`'s created using the [PEMprepr::create_landscape_covariate()].
#'      A default location and name are applied in line with standard workflow.
#' @param layers A `character` vector with the names of the landscape .tif files
#'      on which the binning will be based. Default names are
#'      c("dah_LS", "landform_LS","mrvbf_LS")
#' @param write_output should the binned landscape raster be written to disk?
#'     If `TRUE` (default), will write to `in_dir`.
#' @return a `SpatRast`
#' @export
#'
#' @examples
#' \dontrun{
#' combr = create_binned_landscape(
#'   in_dir = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "25m", "modules_landscape"),
#'   layers = c("dah_LS", "landform_LS","mrvbf_LS"))
#' }
create_binned_landscape <- function(
    in_dir = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "25m", "modules_landscape"),
    layers = c("dah_LS", "landform_LS","mrvbf_LS"),
    write_output = TRUE

){

  if (!dir.exists(fs::path(in_dir))) {
    cli::cli_abort("{.var in_dir} does not exist, please check the path to your
                 landscape covariates is correct")
  }

  rastlist <- fs::dir_ls(in_dir, glob = ("*.tif"))
  rastlist <- rastlist[grep(paste(layers, collapse = "|"), rastlist, value = TRUE)]

  if (length(layers) == length(rastlist)) {
    print("using the following files:")
    print(rastlist)

    ancDat <- terra::rast(rastlist)

  } else {
    cli::cli_abort("{.var layers} and not found in your {.var in_dir}, please
                   check layer names match names of landscape rasters")
  }

  # find unique combinations and assign an id column
  combinations <- terra::unique(ancDat)
  comb.df <- as.data.frame(combinations)
  comb.df <- stats::na.omit(comb.df) # remove NA values

  comb.df$landscape = seq_len(nrow(comb.df))
  ancDat.df <- as.data.frame(ancDat, xy = TRUE)
  anc_class <- dplyr::left_join(ancDat.df, comb.df)

  out_rast <- terra::rast(anc_class, type="xyz", crs= terra::crs(ancDat), digits=6)
  out_rast <- out_rast$landscape

  if(write_output){

    terra::writeRaster(out_rast, fs::path(in_dir, "landscape_binned.tif"), overwrite = TRUE)
    cli::cat_line()
    cli::cli_alert_success(
      "Binned landscape raster written to {.path {in_dir}}"
    )
  }

  out_rast

}
