#' Build site transects
#' Build the associates triangles, buffered triangles and selected paired triangle to sample, based on lowest cost.
#'
#' @param sample_points **sf** spatial object of clhs points
#' @param cost **SpatRast** cost layer generate for sample plan
#' @param mask_poly **sf** spatial object of mask for specific bgc
#' @param centroid_distance Numeric value at which the triangles are placed apart, default is 400 based on albers crs meters
#' @param outname A character name for output file. Default is s1_sampling
#' @param out_dir text string with location in which output sample plan as a geopackage is written
#'
#' @return writes out sf geompackage with multiple layers
#' @export
#' @examples
#' \dontrun{
#' build_site_transects(sample_points, cost, centroid_distance = 400, out_dir)
#' }

build_site_transects <- function(sample_points, cost, mask_poly, centroid_distance = 400, out_dir, outname = "s1_sampling.gpkg") {
  sample_points <- dplyr::select(sample_points, c("slice_num", "point_num", "bgc")) |>
    dplyr::arrange("slice_num", "point_num") |>
    dplyr::mutate(cid = seq(1, nrow(sample_points), 1))

  # b <- unique(sample_points$bgc)

  sf::st_geometry(sample_points) <- "geometry"
  # g <- sample_points
  # name<- "geometry"
  # current = attr(g, "sf_column")
  # names(g)[names(g)==current] = name
  # sf::st_geometry(g)=name
  # sample_points <- g

  # create paired outputs
  sample_points_clhs <- sf::st_as_sf(sample_points) |>
    sf::st_transform(3005) |>
    dplyr::mutate(aoi = NA)

  rotation_angles <- seq(0, 315, 45) # Rotation degrees

  sample_points_rotations <- sf::st_sf(sf::st_sfc()) |> sf::st_set_crs(3005)

  print("generating site points")

  for (i in 1:nrow(sample_points_clhs)) {
    # i = 1
    pnt <- sample_points_clhs[i, ]
    pGeom <- sf::st_geometry(pnt)
    pGeom <- pGeom + c(0, centroid_distance)
    pnt_feat <- sf::st_set_geometry(pnt, pGeom)

    rotated_points <- sf::st_sf(sf::st_sfc()) |> sf::st_set_crs(3005)

    rotated_points <- foreach::foreach(Bear = rotation_angles, .combine = rbind) %do% {
      # Bear = rotation_angles[5]
      Feature_geo <- sf::st_geometry(pnt_feat)
      PivotPoint <- sf::st_geometry(pnt)
      ## Convert bearing from degrees to radians
      d <- ifelse("Bear" > 180, pi * (("Bear" - 360) / 180), pi * ("Bear" / 180))
      rFeature <- (Feature_geo - PivotPoint) * .rot(d) + PivotPoint
      rFeature <- sf::st_set_crs(rFeature, sf::st_crs(pnt_feat))
      pnt_feat$geometry <- sf::st_geometry(rFeature) ## replace the original geometry
      pnt_feat$Rotation <- Bear
      pnt_feat <- pnt_feat |> sf::st_set_crs(3005)
    }

    sample_points_rotations <- rbind(rotated_points, sample_points_rotations)
  }

  sample_points_rotations <- sf::st_as_sf(sample_points_rotations, crs = 3005) |>
    dplyr::mutate(rotation = plyr::mapvalues("Rotation", "rotation_angles", c("N", "NE", "SE", "W", "E", "NW", "SW", "S"))) |>
    dplyr::filter(!is.na("Rotation")) |>
    sf::st_join(mask_poly, join = sf::st_intersects) |>
    dplyr::mutate(aoi = dplyr::case_when(
      is.na(cost) ~ FALSE,
      TRUE ~ TRUE
    )) |>
    dplyr::select(-cost)

  cost <- terra::extract(cost, sample_points_rotations, ID = FALSE)
  sample_points_rotations <- cbind(sample_points_rotations, cost)

  sample_points_low_cost <- sample_points_rotations |>
    dplyr::group_by("slice_num", "point_num") |>
    dplyr::filter(aoi == TRUE) |>
    dplyr::slice(which.min("cost")) |>
    dplyr::ungroup()

  sample_points_low_cost <- sample_points_low_cost |>
    dplyr::select(-c("cost", "Rotation", "aoi"))

  sample_points_clhs <- sample_points_clhs |>
    dplyr::mutate(rotation = "cLHS") |>
    dplyr::select(-aoi)

  sample_points_rotations <- sample_points_rotations |>
    dplyr::select(-"cost", -"Rotation", -"aoi") |>
    dplyr::filter(!is.na("rotation"))

  all_points <- rbind(sample_points_clhs, sample_points_rotations) |>
    dplyr::mutate(id = paste(paste(bgc, paste(slice_num, point_num, sep = "."), .$cid, sep = "_"), .$rotation, sep = "_"))

  paired_sample <- rbind(sample_points_clhs, sample_points_low_cost) |>
    dplyr::mutate(id = paste(paste(bgc, paste(slice_num, point_num, sep = "."), .$cid, sep = "_"), .$rotation, sep = "_"))

  # Create triangle around each point and randomly rotate
  print("generating site transects")

  all_triangles <- sf::st_sf(sf::st_sfc()) |> sf::st_set_crs(3005)

  for (i in 1:nrow(all_points)) {
    # i = 1
    poc <- all_points[i, ]

    triangle <- .Tri_build(id = poc$id, x = sf::st_coordinates(poc)[1], y = sf::st_coordinates(poc)[2])
    random_rotation <- stats::runif(1, min = 0, max = 360)
    triangle <- .rotFeature(triangle, poc, random_rotation)
    all_triangles <- rbind(all_triangles, triangle)
  }


  paired_triangles <- all_triangles[all_triangles$id %in% paired_sample$id, ]

  print("writting out spatial data")

  ##### write Transects####################

  sf::st_write(all_points, fs::path(out_dir, outname),
               layer = paste0(b, "_points_all"), delete_layer = TRUE, quiet = T
  )

  sf::st_write(all_triangles, fs::path(out_dir, outname),
               layer = paste0(b, "_transects_all"), delete_layer = TRUE, quiet = T
  )

  sf::st_write(paired_sample, file.path(out_dir, outname),
               layer = paste0(b, "_points"), delete_layer = TRUE, quiet = T
  )


  #### write buffer#########################
  triangle_buff <- sf::st_buffer(all_triangles, dist = 10)

  sf::st_write(triangle_buff, fs::path(out_dir, outname), layer = paste0(b, "_transects_all_buffered"), delete_layer = TRUE, quiet = T)

  sf::st_write(paired_sample, fs::path(out_dir, outname), layer = paste0(b, "_points"), delete_layer = TRUE, quiet = T)

  # paired_triangles
  sf::st_write(paired_triangles, fs::path(out_dir, outname), layer = paste0(b, "_transects"), delete_layer = TRUE, quiet = T)

  #### write buffer#########################
  ptriangle_buff <- sf::st_buffer(paired_triangles, dist = 10)
  sf::st_write(ptriangle_buff, fs::path(out_dir, outname), layer = paste0(b, "_transects_buffered"), delete_layer = TRUE, quiet = T)

  # write out clhs points only
  sf::st_write(sample_points_clhs, fs::path(out_dir, outname), layer = paste0(b, "_points_clhs"), delete_layer = TRUE, quiet = T)
}


.rot <- function(a) {
  out <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  return(out)
}

.Tri_build <- function(id, x, y) {
  tris <- LearnGeom::CreateRegularPolygon(3, c(
    as.numeric(paste(x)),
    as.numeric(paste(y))
  ), 145)

  MoonLineCentre <- data.frame(tris)
  MoonLineCentre <- sf::st_as_sf(MoonLineCentre, coords = c("X", "Y"), crs = 3005)
  MoonLineCentre <- MoonLineCentre |>
    dplyr::mutate(id = id) |>
    dplyr::group_by(id) |>
    dplyr::summarise() |>
    sf::st_cast("POLYGON") |>
    sf::st_cast("MULTILINESTRING")

  return(MoonLineCentre)
}

.rotFeature <- function(Feature, PivotPt, Bearing) {
  # where Feature is the Shape to be rotated, eg:  #Feature <- tri
  # Bearing is the compass bearing to rotate to    #PivotPt <- pt.sf
  # PivotPt is the point to rotate around          #Bearing <- 15

  ## extract the geometry
  Feature_geo <- sf::st_geometry(Feature)
  PivotPoint <- sf::st_geometry(PivotPt)

  ## Convert bearing from degrees to radians
  d <- ifelse(Bearing > 180, pi * ((Bearing - 360) / 180), pi * (Bearing / 180))

  rFeature <- (Feature_geo - PivotPoint) * .rot(d) + PivotPoint
  rFeature <- sf::st_set_crs(rFeature, sf::st_crs(Feature))

  Feature$geometry <- sf::st_geometry(rFeature) ## replace the original geometry
  return(Feature)
}
