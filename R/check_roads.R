#' Check road network layer
#'
#' @param roads a `sf` object or path to roads layers. Default location is based
#'          on standard workflow. Roads is created using the create_base_vectors()
#'          function.
#' @return TRUE
#' @export
#'
#' @examples
#' \dontrun{
#'roads_raw <- sf::st_read(
#'fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel,"road_network.gpkg"))
#'check_road_layer(roads_raw)
#' }
check_roads <- function(
    roads = fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel,"road_network.gpkg")
      ) {
  ## read in the major roads
 # roads = sf::st_read(fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel,"road_network.gpkg"))

  if (inherits(roads, c("character"))) {
    roads <- sf::st_read(roads, quiet = TRUE)
  } else if (!inherits(roads, c("sf", "sfc"))) {
    cli::cli_abort("{.var aoi} must be an sf or an sfc object or a path to a file")
  }

  roads_check <- roads |>
    sf::st_drop_geometry() |>
    dplyr::select("ROAD_CLASS", "ROAD_SURFACE", "ROAD_NAME_FULL")

  if (length(roads_check == 3)) {
    cli::cli_alert_success("check 1: road layer contain required fields")
  } else {
    cli::cli_abort("road layer does not contain required fields : check that
    the layer includes the following fields :
    ROAD_CLASS, ROAD_SURFACE, ROAD_NAME_FULL")

  }

  # check road surface
  roads_check <- roads_check |>
    dplyr::rename(
      "road_surface" = "ROAD_CLASS",
      "surface" = "ROAD_SURFACE",
      "name" = "ROAD_NAME_FULL"
    )

  rsurface <- unique(roads_check$surface)
  if ("overgrown" %in% rsurface) {
    cli::cli_alert_warning("check 2: road class contains overgrown road segment, are you sure these are actual roads?")
  } else {
    cli::cli_alert_success("check 2: road class does not contain overgrown road segment")
  }

  rSpd <- dplyr::tibble(
    "road_surface" = c("resource", "unclassified", "recreation", "trail", "local", "collector", "highway", "service", "arterial", "freeway", "strata", "lane", "private", "yield", "ramp", "restricted", "water", "boat", "ferry", "driveway", "unclassifed"),
    # "speed" = c(30, 30, 50, 4.5, 50, 80, 80, 50, 80, 80, 30, 30, 4.5, 30, 60, 4.5, 0.1, 0.1))
    "speed" = c(3000, 3000, 5000, 4.5, 5000, 8000, 8000, 50, 8000, 8000, 3000, 3000, 4.5, 3000, 6000, 4.5, 0.1, 0.1, 3000, 4.5, 3000)
  )

  rclass <- data.table::as.data.table(unique(roads_check$road_surface))
  names(rclass) <- "road_surface"
  rdsAll <- merge(rclass, rSpd, by = "road_surface", all = T)
  rdsAll <- rdsAll |>
    dplyr::filter(is.na("speed"))
  rdsAll <- as.data.frame(rdsAll)

  if (nrow(rdsAll) > 0) {
    cli::cli_abort("check 3: undefined speeds for the following road surface types")
  } else {
    cli::cli_alert_success("check 3: all road surface types assigned a speed")
  }

  # options to add more checks here (geometry etc)

  return(TRUE)
}
