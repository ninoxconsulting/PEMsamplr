#' prepare_cost_layers
#'
#' Use roads speed and slope to calculate initial cost of travel to be used in cost layer
#'
#' @param vec_dir directory where clean vector layers are stored
#' @param sampleplan_dir directory where sample plan inputs are stored
#' @param heli TRUE or FALSE to indicate if road or heli design
#' @param heli TRUE or FALSE to indicate if road or heli design
#' @param out_dir A `character` or path where cost layer is written to.
#' @param write_output A `logical`if the sf object be written to disk?
#'     If `TRUE` (default), will write to `out_dir` under the appropriate resolution subfolder.
#' @param overwrite A `logical` if the output file already exists, should it be overwritten?
#' @return **SpatRast** cost surface to be used in cost layer creation
#' @export
#' @examples
#' \dontrun{
#' prep_cost_layers(vec_dir,  sampleplan_dir, heli, write_output, out_dir)
#' }

prep_cost_layer <- function(
    vec_dir = PEMprepr::read_fid()$dir_1010_vector$path_abs,
    sampleplan_dir = PEMprepr::read_fid()$dir_201010_inputs$path_abs,
    heli = FALSE,
    out_dir = PEMprepr::read_fid()$dir_201010_inputs$path_abs,
    write_output = TRUE,
    overwrite = FALSE){

  # testing lines

  #in_dir <- vec_dir <- PEMprepr::read_fid()$dir_1010_vector$path_abs
  #sampleplan_dir <- PEMprepr::read_fid()$dir_201010_inputs$path_abs
  #out_dir <- PEMprepr::read_fid()$dir_201010_inputs$path_abs
  #heli = FALSE

  # end testing lines

  if (!fs::file_exists(fs::path(sampleplan_dir, "dem.tif"))) {
    cli::cli_abort(
      "dem.tif does not exist in {.var sampleplan_dir}. Please check the function
        create_base_vectors() ran correctly or add this manually"
    )
  }

  dem <- terra::rast(fs::path(sampleplan_dir, "dem.tif"))

  # check water exists

  if (!fs::file_exists(fs::path(vec_dir, "water.gpkg"))) {
    cli::cli_abort(
      "water.gpkg does not exist in {.var vec_dir}. Please check the function
        create_base_vectors() ran correctly or add this manually"
    )
  }

  water <- sf::st_read(fs::path(vec_dir, "water.gpkg"), quiet = TRUE)


  #check if the start point exists

  if (!fs::file_exists(fs::path(sampleplan_dir, "start.gpkg"))) {
    cli::cli_abort(
      "start.gpkg does not exist in {.var sampleplan_dir}. Please check the function
        create_base_vectors() ran correctly or add this manually"
    )
  }

  start <- sf::st_read(fs::path(sampleplan_dir, "start.gpkg"), quiet = TRUE)


  # check if the road network exists

  if (!fs::file_exists(fs::path(sampleplan_dir, "road_network.gpkg"))) {
    cli::cli_abort(
      "road_network.gpkg does not exist in {.var sampleplan_dir}. Please check the function
        create_base_vectors() ran correctly or add this manually"
    )
  }

  roads <- sf::st_read(fs::path(sampleplan_dir, "road_network.gpkg"), quiet = TRUE)



  #prep_cost_layers_old <- function(in_dir, dem, heli = FALSE) {

  if(heli == FALSE) {

    ## read in the major roads
    roads$ROAD_CLASS[roads$trail == 1] <- "trail"
    roads <- roads[,c("ROAD_SURFACE","ROAD_CLASS", "ROAD_NAME_FULL")]
    roads <- roads |>
      dplyr::rename('road_surface' = .data$ROAD_CLASS,
                    'surface' = .data$ROAD_SURFACE,
                    'name' = .data$ROAD_NAME_FULL)
    rdsAll <-  data.table::as.data.table(roads) |>  sf::st_as_sf()
    rSpd <- dplyr::tibble(
      "road_surface" = c("resource", "unclassified", "recreation", "trail", "local", "collector", "highway", "service", "arterial", "freeway", "strata", "lane", "private", "yield", "ramp", "restricted", "water", "ferry", "driveway"),
      "speed" = c(30, 30, 50, 4.5, 50, 80, 80, 50, 80, 80, 30, 30, 4.5, 30, 60, 4.5, 0.1, 0.1, 4.5))
    #"speed" = c(3000, 3000, 5000, 4.5, 5000, 8000, 8000, 50, 8000, 8000, 3000, 3000, 4.5, 3000, 6000, 4.5, 0.1, 3000, 4.5, 3000))

    #   # convert speed to pace
    rSpd <- data.table::as.data.table(rSpd) |>
      dplyr::mutate(pace = 1.5*(1/.data$speed)) |>
      dplyr::select(-.data$speed) # km/h to minutes per 25m pixal

    rdsAll <- merge(rdsAll, rSpd, by = "road_surface", all = F)
    rdsAll <- rdsAll[,"pace"]
    #allRast <- terra::rasterize(rdsAll, dem, field = "pace")
    #allRast[is.nan(allRast[])] <- NA

    #   # create a roads raster (buffered)
    rdsAll <- sf::st_buffer(rdsAll, dist = 25, endCapStyle = "SQUARE", joinStyle = "MITRE")
    rdsAll <- sf::st_cast(rdsAll, "MULTIPOLYGON")
    rdsRast <- terra::rasterize(rdsAll, dem, field = "pace", fun = "max")
    rdsRast[is.nan(rdsRast[])] <- NA
    #rm(allRast)

    cli::cli_alert_info("road layers prepared")

    #   # prepare the water data

    water <- water |>
      dplyr::filter(.data$WATERBODY_TYPE != "W") |>
      dplyr::mutate(cost = 10000) |>
      sf::st_cast("MULTIPOLYGON")  |>
      dplyr::select(.data$cost)
    water_r <- terra::rasterize(water, dem, field = "cost", fun = "max")

    dem[water_r] <- 0
    rm(water_r)

    cli::cli_alert_info("water layers prepared")

    #   # prepare walking terrain function
    slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "radians") # convert these radians to rise/run in next line

    dem <- (3/5) * 6*exp(-3.5*abs(tan(slope) + 0.05)) * (40/60)## this converts km/hr to minutes/25m pixel
    # 40 x 25 = 1lm / 60 minutes from hours
    dem_toblers <- 1/dem |>  round(3)

    altAll  <- terra::cover(rdsRast, dem_toblers)
    #terra::plot(altAll)

    cli::cli_alert_info("walking terrain surface (minutes by 25m pixal) prepared")


  } else {
    #
    #  cli::cat_line()
    #  cli::cli_abort("heli methodology is still to be developed, please select ")
    #  }

    #   # prepare walking terrain function
    slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "radians") # convert these radians to rise/run in next line

    dem <- (3/5) * 6*exp(-3.5*abs(tan(slope) + 0.05)) * (40/60)## this converts km/hr to minutes/25m pixel
    # 40 x 25 = 1lm / 60 minutes from hours
    dem_toblers <- 1/dem |>  round(3)

    altAll  <- dem_toblers
    #terra::plot(altAll)

    print("walking terrain surface (minutes by 25m pixal) prepared")

    gc()

  }

  altAllr <- raster::raster(altAll)

  # create  transition layer
  tr <- gdistance::transition(altAllr, transitionFunction = function(x) 1/mean(x), directions = 8, symm = F)

  #saveRDS(tr, file.path(sampleplan_dir, "transition_layer.rds"))
  #tr <- readRDS(file.path(out_path,"input_raster", "transition_layer.rds"))

  altAll = NA
  rdsAll = NA
  rdsRast = NA
  alt = NA

  tr <- gdistance::geoCorrection(tr)

  # output transition layer to use in creating TSP paths
  #saveRDS(tr, fs::path(sampleplan_dir, "transition_layer.rds"))

  #tr1 <- readRDS(file.path(out_path,"input_raster", "transition_layer.rds"))
  #tr1 = tr
  start <- sf::as_Spatial(start)

  acost <- gdistance::accCost(tr,start)

  tacost <- terra::rast(acost)

  terra::crs(tacost)  <- "epsg:3005"

  names(tacost) <- "cost"


  if (write_output) {
    if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir, recurse = TRUE)
      cli::cli_alert_warning(
        "write out folder does not exist, creating at location {.path {out_dir}}"
      )
    }

    output_file <- fs::path(fs::path_abs(out_dir), "acost.tif")

    if (fs::file_exists(output_file)) {
      cli::cli_alert_warning(
        "Cost raster already exists in {.path {output_file}}"
      )
    }

    terra::writeRaster(tacost, fs::path(output_file), overwrite = overwrite)
    cli::cat_line()
    cli::cli_alert_success(
      "Cost raster written to {.path {output_file}}"
    )
  }


  return(tacost)

}
