# # this is a series of helper functions that can be used to create to prep the cost layer (new version)
# # TODO: decide on method for new or old prep cost layer (review by bcgov staff)
# # once decide incorporate into
#
# neighbourhood <- function(neighbours) {
#
#   neighbours_32 <- matrix(c(0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1,
#                             1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0), nrow = 7, ncol = 7, byrow = TRUE)
#
#   neighbours_48 <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1,
#                             1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0,
#                             1, 0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 9, ncol = 9, byrow = TRUE)
#
#   if (inherits(neighbours, "matrix")) {
#     neighbours <- neighbours
#   } else if (neighbours == 4) {
#     neighbours <- 4
#   } else if (neighbours == 8) {
#     neighbours <- 8
#   } else if (neighbours == 16) {
#     neighbours <- 16
#   } else if (neighbours == 32) {
#     neighbours <- neighbours_32
#   } else if (neighbours == 48) {
#     neighbours <- neighbours_48
#   } else (
#     stop(paste0("neighbours argument invalid. Expecting 4, 8, 16, 32, 48, or a matrix object"))
#   )
#
#   return(neighbours)
# }
#
#
#
#
#
# calculate_distance <- function(x, adj) {
#
#   xy1 <- terra::xyFromCell(x, adj[, 1])
#   xy2 <- terra::xyFromCell(x,adj[, 2])
#
#   xy3 <- (xy1[,1] - xy2[,1])^2
#   xy4 <- (xy1[,2] - xy2[,2])^2
#
#   dist <- sqrt(xy3 + xy4)
#
#   return(dist)
#
# }
#
#
#
# create_accum_cost <- function(x, origins, FUN = mean, rescale = FALSE, check_locations = FALSE) {
#
#   #  x = cl
#   #  origins = xstart
#   #  FUN = mean
#   # check_locations = FALSE
#   # rescale = FALSE
#   #
#
#
#   if(check_locations) {
#     check_locations(x, origins)
#   }
#
#   cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
#
#   from_coords <- get_coordinates(origins)
#   from_cell <- terra::cellFromXY(cs_rast, from_coords)
#
#   cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)
#
#   igraph::E(cm_graph)$weight <- (1/igraph::E(cm_graph)$weight)
#
#   from_distances <- igraph::distances(cm_graph, v = from_cell,  mode="out", algorithm = "dijkstra")
#
#   accum_rasts <- c(rep(cs_rast, nrow(from_distances)))
#
#   for(i in 1:terra::nlyr(accum_rasts))  {
#
#     accum_rasts[[i]] <- terra::setValues(accum_rasts[[i]], from_distances[i,])
#
#   }
#
#   accum_rast <- terra::app(accum_rasts, fun = FUN)
#
#   accum_rast[is.infinite(accum_rast)] <- NA
#
#   if(rescale) {
#     rast_min <- terra::minmax(accum_rast)[1]
#     rast_max <- terra::minmax(accum_rast)[2]
#
#     accum_rast <- ((accum_rast - rast_min)/(rast_max - rast_min))
#   }
#
#   return(accum_rast)
#
# }
#
#
#
#
#
#
#
# get_coordinates <- function(x) {
#
#   if(inherits(x, "sf")) {
#     coords <- sf::st_coordinates(x)[, 1:2, drop = FALSE]
#   }
#   else if (inherits(x, "SpatVector")) {
#     coords <- terra::crds(x)
#   }
#   else if (inherits(x, "data.frame")) {
#     coords <- as.matrix(x)
#   }
#   else if (inherits(x, "matrix")) {
#     coords <- x
#   }
#   else if (inherits(x, "numeric")) {
#     coords <- matrix(x, nrow = 1)
#   }
#
#   return(coords)
#
# }
#
# lcp_dist_mat <- function(x, origins, destinations, cost_distance = FALSE) {
#
#   cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
#
#   from_coords <- get_coordinates(origins)
#   to_coords <- get_coordinates(destinations)
#
#   from_cell <- terra::cellFromXY(cs_rast, from_coords)
#   to_cell <- terra::cellFromXY(cs_rast, to_coords)
#
#   cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)
#
#   igraph::E(cm_graph)$weight <- (1/igraph::E(cm_graph)$weight)
#
#   message("Calculating distance matrix...")
#   cost <- igraph::distances(graph = cm_graph, v = from_cell, to = to_cell, mode = "out")
#   distMat <- as.matrix(cost)
#
#   return(distMat)
# }
#
#
#
#
# cost <- function(cost_function) {
#
#   cfs <- c("tobler", "tobler offpath")
#
#   if (inherits(cost_function, "character")) {
#     if (!cost_function %in% cfs) {
#       stop("cost_function argument is invalid. See details for accepted cost functions")
#     }
#
#     # mathematical slope to degrees
#     slope2deg <- function(slope) { (atan(slope) * 180/pi)}
#
#     # degrees to radians
#     deg2rad <- function(deg) {(deg * pi) / (180)}
#
#     # Tobler Hiking Function measured in km/h. Divide by 3.6 to turn into m/s
#     if (cost_function == "tobler") {
#
#       # 3.6 converts from km/h to m/s
#       cf <- function(x) {
#         (6 * exp(-3.5 * abs(x + 0.05))) / 3.6
#       }
#     }
#
#     if (cost_function == "tobler offpath") {
#
#       cf <- function(x) {
#         ((6 * exp(-3.5 * abs(x + 0.05))) * 0.6) / 3.6
#       }
#
#     }
#
#   }
#
#   if(is.function(cost_function)) {
#
#     cf <- cost_function
#
#   }
#
#   return(cf)
# }
