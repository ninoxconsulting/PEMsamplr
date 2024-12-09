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
  roads = sf::st_read(fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel,"road_network.gpkg"))

  roads <- PEMprepr::read_sf_if_necessary(roads)

  # check that the road layer contains the required fields
  roads_check <- roads |>
    dplyr::select("ROAD_CLASS", "ROAD_SURFACE", "ROAD_NAME_FULL")

  if (all(c("ROAD_CLASS", "ROAD_SURFACE", "ROAD_NAME_FULL") %in% names(roads_check))){
    cli::cli_alert_success("check 1: road layer contain required fields")
  } else {
    cli::cli_abort("road layer does not contain required fields : check that
    the layer includes the following fields :
    ROAD_CLASS, ROAD_SURFACE, ROAD_NAME_FULL")

  }


  rsurface <- unique(roads_check$ROAD_SURFACE)
  if ("overgrown" %in% rsurface) {
    cli::cli_alert_warning("check 2: road class contains overgrown road segment, are you sure these are actual roads?")
  } else {
    cli::cli_alert_success("check 2: road class does not contain overgrown road segment")
  }


  # check if road class is contained within the acceptable road classes defined
  #in the function
  road_surface_types_acceptable =  c("resource", "unclassified", "recreation",
                                     "trail", "local", "collector", "highway",
                                     "service", "arterial", "freeway", "strata",
                                     "lane", "private", "yield", "ramp",
                                     "restricted", "water", "boat", "ferry",
                                     "driveway", "unclassifed")

  rclass <- unique(roads_check$ROAD_CLASS)

  if (all(rclass %in% road_surface_types_acceptable)) {
    cli::cli_alert_success("check 3: road class contains acceptable road surface types")
  } else {
    cli::cli_abort("check 3: road class contains unacceptable road surface types")
  }

  return(TRUE)
}
