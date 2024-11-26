#' Check start locations meet sample plan criteria
#'
#' This functions checks that selected start points are all or partially
#' connected to the road network
#'
#' @param roads An `sf` object or path to roads layers. Default location is based
#'          on standard workflow. Roads is created using the create_base_vectors()
#'          function.
#' @param locations An `sf` object with start locations points. These represent the
#'        start locations (such as a town or location) from which the cost layer
#'        will be based. Locations may be single or multi point.
#' @return TRUE
#' @export
#'
#' @examples
#' \dontrun{
#'roadsls <- get_roads(aoils, PEMprepr::read_fid()$dir_201010_inputs$path_abs)
#'cities <- sf::st_read(fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel,"major_towns_bc.gpkg"))
#'nearest_town = "Hazelton"
#'start <- cities[cities$NAME == nearest_town,"NAME"]
#'check_locations(roadsls, start)
#' }
check_locations <- function(roads, locations) {

  locations <- terra::vect(locations)

  roadsbuf <- sf::st_buffer(roads, 50)

  #roadsbuf <- dplyr::mutate(roadsbuf, roads = "roads") |> dplyr::select(roads)

  roadsbuf <- sf::st_union(roadsbuf)

  roadssv <- terra::vect(roadsbuf)

  connectroads <- terra::relate(roadssv, locations, "intersects") |> which()

  if (length(connectroads) == length(locations)) {
    cli::cli_alert_success("start locations are confirmed to be connected to road network")
  }
  if (length(connectroads) < length(locations) & length(connectroads) > 0) {
    cli::cli_alert_warning("some start locations are not connected to roads network, please review the location")

  }
  if (length(connectroads) < 1 ) {
    cli::cli_abort(" no locations are not connected to roads network, please review and edit as needed")
  }

}
