#' Check sample cost per BGC
#'
#' Assess the costs of sampling areas for each BGC to help assess how well the cost layer describes to study area
#'
#' @param bec A `SpatRast` or path to BEC raster generated at a landscape (25m)
#'      scale. This output is derived from the create_bgc_template().
#' @param binned_landscape A `SpatRast` with the landscape binned. This is the
#'      output of the [create_binned_landscape()] function.
#' @param cost A `SpatRast` or path to the generated cost layer. This is the
#'      output of the function (prep_cost_layer TBFinalised)
#' @return data.frame with a class
#' @export
#'
#' @examples
#' \dontrun{
#' bec <- terra::rast(
#'   fs::path(PEMprepr::read_fid()$dir_201010_inputs$path_abs, "25m", "bec.tif"))
#' binned_landscape <- terra::rast(
#'     fs::path(PEMprepr::read_fid()$dir_201010_inputs$path_abs, "25m",
#'     "modules_landscape", "landscape_binned.tif"))
#' cost <- terra::rast(
#'     fs::path(PEMprepr::read_fid()$dir_201010_inputs$path_abs,"acost.tif"))
#' check_bgc_cost(bec, binned_landscape, cost)
#' }
check_bgc_cost <- function(bec, binned_landscapes, cost) {

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

  if (inherits(binned_landscape, c("character"))) {
    cost <- terra::rast(binned_landscape)
  } else if (!inherits(cost, c("SpatRaster"))) {
    cli::cli_abort("{.var cost} must be a SpatRaster or a path to a file")
  }

  if (!isTRUE(terra::compareGeom(bec, binned_landscape, cost))) {
    cli::cli_abort("{.var bec} must match spatial extent of landscapes raster stack")
  } else {

    names(cost) <- "cost"

    rcost <- c(bec, binned_landscape, cost)

    rcdf <- as.data.frame(rcost, xy = TRUE)
    rcdf <- na.omit(rcdf)
    rcdf <- rcdf |> dplyr::select(-c(x, y))

    rcdf_class <- rcdf |>
      dplyr::mutate(cost_code = dplyr::case_when(
        cost < 250 ~ "low",
        cost > 250 & cost < 500 ~ "moderate",
        cost > 500 & cost < 800 ~ "high",
        cost > 800 & cost < 1000 ~ "very high",
        cost > 1000 ~ "prohibative",
        TRUE ~ as.character("unknown")
      ))

    return(rcdf_class)
  }

}
