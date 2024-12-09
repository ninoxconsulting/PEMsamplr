#' Create a cost penalty layer for sample plan development
#'
#' Uses cost layer and applies a cost layers to the following criteria:
#' cutblocks (within the last 20 years), vri (age class 1 & 2), vri age class 3,
#' vri (deciduous leading), private land, fire intensity, fire boundaries,
#' transmission lines and steep slopes.
#'
#' @param vec_dir A `character` string or path which points to base vector
#'    layers created using create_base_vectors(). A default location and name
#'    are applied in line with standard workflow.
#' @param dem A `SpatRast` or path to the landscape scale DEM data. A default
#'    location and name is applied in line with standard workflow.
#' @param cost A `SpatRast` or path to cost layer created using the
#'    create_cost_layer() function. A default location and name is applied in
#'    line with standard workflow.
#' @param costval A `numeric` value representing a high cost value. Default is
#'    3000. These values are only used when calc_by_qq is FALSE.
#' @param vri_cost A `numeric` value representing the cost to be applied to VRI
#'    age class 1 and 2 cost value. Default is 2500.These values are only used
#'    when calc_by_qq is FALSE.
#' @param calc_by_qq should the cost values be calculated based on distribution
#'    of cost values rather than specified in costval and vri_cost. The costs
#'    assigned by distribution are 0.65 for vri_cost, 0.70 for costval and 0.9
#'    for max cost, applied to the slope greater than 45% degrees.
#' @param out_dir A `character` or path which points to input location
#'      of . A default location and name are applied in line with standard workflow.
#' @param write_output A `logical` should the cost_penalty spatRaster be
#'     written to disk? If `TRUE` (default), will write to `out_dir`.
#' @param overwrite A `logical` should the cost_penalty spatRaster overwrite any
#'    existing file? If `TRUE` (default), will overwrite the existing file. This
#'    is only applicable where `write_output` is `TRUE`.
#' @return A `SpatRaster` representing costs with cost penality values applied
#' @export
#'
#' @examples
#' \dontrun{
#'sampleplan_dir <- PEMprepr::read_fid()$dir_201010_inputs$path_abs
#'acost <- terra::rast(fs::path(sampleplan_dir, "acost.tif" ))
#'terra::crs(acost) <- terra::crs(bec)
#'names(acost) <- "cost"
#'vec_dir = fs::path(PEMprepr::read_fid()$dir_1010_vector$path_abs)
#'dem = terra::rast(fs::path(PEMprepr::read_fid()$dir_201010_inputs$path_abs, "25m", "dem.tif"))
#'cost =  acost
#'costval = 3000
#'vri_cost = 2500
#'calc_by_qq = TRUE
#'
#'cost_penalty <- create_cost_penalty(vec_dir = vec_dir,
#'                                    cost = acost,
#'                                    dem = dem,
#'                                    costval = 3000,
#'                                    vri_cost = 2500,
#'                                    calc_by_qq = TRUE,
#'                                    write_output = FALSE)
#'
#'
#' }
#'
#'
create_cost_penalty <- function(vec_dir = fs::path(PEMprepr::read_fid()$dir_1010_vector$path_abs),
                                dem,
                                cost,
                                costval = 3000,
                                vri_cost = 2500,
                                calc_by_qq = TRUE,
                                out_dir = fs::path(PEMprepr::read_fid()$dir_201010_inputs$path_abs),
                                write_output = TRUE,
                                overwrite = FALSE) {
  if (!inherits(vec_dir, "character") || !fs::dir_exists(vec_dir)) {
    cli::cli_abort("{.var vec_dir} must be a directory path")
  }

  dem <- PEMprepr:::read_spatrast_if_necessary(dem)

  cost <- PEMprepr:::read_spatrast_if_necessary(cost)

  if (isTRUE(calc_by_qq)) {
    qq <- terra::global(cost, stats::quantile, probs = c(0.65, 0.70, 0.90), na.rm = TRUE)

    vri_cost <- qq$X65.
    costval <- qq$X70.
    maxval <- qq$X90.
  }

  # 1. Assign high cost for cutblocks
  if (fs::file_exists(fs::path(vec_dir, "cutblocks.gpkg"))) {
    rcutblock <- .assign_highcost(file.path(vec_dir, "cutblocks.gpkg"), costval = costval, cost = cost)
    hc <- terra::cover(rcutblock, cost)
    cli::cat_line()
    cli::cli_alert_success(
      "Cutblocks added to cost penalty layer"
    )
  } else {
    cli::cli_alert_warning(
      "Cutblocks not found in {.path {vec_dir}}"
    )
  }

  # 2. Assign high cost to age class 1 and 2
  if (fs::file_exists(fs::path(vec_dir, "vri_class1_2.gpkg"))) {
    rvri12_class <- .assign_highcost(file.path(vec_dir, "vri_class1_2.gpkg"), costval = costval, cost = cost)
    hc <- terra::cover(rvri12_class, hc)
    cli::cat_line()
    cli::cli_alert_success(
      "Vri class 1 and 2 added to cost penalty layer"
    )
  } else {
    cli::cli_alert_warning(
      "Vri class 1 and 2 not found in {.path {vec_dir}}"
    )
  }



  # 3. Assign a slightly lower cost to age class 3.
  if (fs::file_exists(fs::path(vec_dir, "vri_class3.gpkg"))) {
    rvri3_class <- .assign_highcost(file.path(vec_dir, "vri_class3.gpkg"), costval = vri_cost, cost = cost)
    hc <- terra::cover(rvri3_class, hc)
    cli::cat_line()
    cli::cli_alert_success(
      "Vri class 3 added to cost penalty layer"
    )
  } else {
    cli::cli_alert_warning(
      "Vri class 3 not found not found in {.path {vec_dir}}"
    )
  }


  # 3 Assign a high cost to deciduous leading species area
  if (fs::file_exists(fs::path(vec_dir, "vri_decid.gpkg"))) {
    rvri_decid <- .assign_highcost(file.path(vec_dir, "vri_decid.gpkg"), costval = costval, cost = cost)
    hc <- terra::cover(rvri_decid, hc)
    cli::cat_line()
    cli::cli_alert_success(
      "Vri deciduous added to cost penalty layer"
    )
  } else {
    cli::cli_alert_warning(
      "Vri deciduous not found in {.path {vec_dir}}"
    )
  }

  # 4. Assign high cost to private lands
  if (fs::file_exists(fs::path(vec_dir, "private.gpkg"))) {
    rpriv <- .assign_highcost(file.path(vec_dir, "private.gpkg"), costval = costval, cost = cost)
    hc <- terra::cover(rpriv, hc)
    cli::cat_line()
    cli::cli_alert_success(
      "Private lands added to cost penalty layer"
    )
  } else {
    cli::cli_alert_warning(
      "Private lands not found in {.path {vec_dir}}"
    )
  }


  # 5. Add high cost for high and medium intensity fire areas or all fires
  if (fs::file_exists(fs::path(vec_dir, "fire_int.gpkg"))) {
    rfireint <- .assign_highcost(file.path(vec_dir, "fire_int.gpkg"), costval = costval, cost = cost)
    hc <- terra::cover(hc, rfireint)
    cli::cat_line()
    cli::cli_alert_success(
      "Fire intensity added to cost penalty layer"
    )
  } else {
    cli::cli_alert_warning(
      "Fire intensity not found in {.path {vec_dir}}"
    )
  }

  # 6. Add high cost for all fires
  if (fs::file_exists(fs::path(vec_dir, "fires.gpkg"))) {
    rfires <- .assign_highcost(file.path(vec_dir, "fires.gpkg"), costval = costval, cost = cost)
    hc <- terra::cover(hc, rfires)
  } else {
    cli::cli_alert_warning(
      "Fires not found in {.path {vec_dir}}"
    )
  }

  # 7. Assign high cost to transmission lines
  if (fs::file_exists(fs::path(vec_dir, "translines.gpkg"))) {
    rtrans <- .assign_highcost(file.path(vec_dir, "translines.gpkg"), costval = costval, cost = cost)
    hc <- terra::cover(hc, rtrans)
    cli::cat_line()
    cli::cli_alert_success(
      "Transmission lines added to cost penalty layer"
    )
  } else {
    cli::cli_alert_warning(
      "Transmission lines not found in {.path {vec_dir}}"
    )
  }

  # 8. Very steep areas
  slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "degrees")
  # degrees (45 degrees = 100%, use around 30 degrees ~ 60% )
  m <- c(
    45, 60, maxval,
    30, 45, costval
  )

  rclmat <- matrix(m, ncol = 3, byrow = TRUE)
  rc <- terra::classify(slope, rclmat)

  hc_out <- terra::mosaic(rc, hc, fun = "max")
  cli::cat_line()
  cli::cli_alert_success(
    "Steep slopes added to cost penalty layer"
  )

  terra::varnames(hc_out) <- "cost"
  names(hc_out) <- "cost"

  if (write_output) {
    if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir, recurse = TRUE)
      cli::cli_alert_warning(
        "write out folder does not exist, creating at location {.path {out_dir}}"
      )
    }

    output_file <- fs::path(fs::path_abs(out_dir), "cost_penalty.tif")

    if (fs::file_exists(output_file)) {
      cli::cli_alert_warning(
        "Cost penalty raster already exists in {.path {output_file}}"
      )
    }

    terra::writeRaster(hc_out, fs::path(output_file), overwrite = overwrite)
    cli::cat_line()
    cli::cli_alert_success(
      "Cost penalty Raster written to {.path {output_file}}"
    )
  }
  return(hc_out)
}

.assign_highcost <- function(shape, crs = 3005, costval, cost) {
  hcsf <- sf::st_read(shape, quiet = TRUE) |>
    sf::st_transform(crs) |>
    dplyr::mutate(cost = costval) |>
    dplyr::select(cost) |>
    sf::st_buffer(dist = 150) |>
    sf::st_cast("MULTIPOLYGON")

  rhc <- terra::rasterize(hcsf, cost, field = "cost", fun = "max")
  return(rhc)
}
