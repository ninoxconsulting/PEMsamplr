
create_clhs <- function(all_cov,
                        num_slices,
                        to_include = NULL,
                        n_points = 5,
                        min_dist = 900,
                        num_sample = 5000000){



  #all_cov = sample_layers_masked
  #num_slices = 3
  #to_include = NULL
  #n_points = 5
  #num_sample = 5000000
  #min_dist = 1000

  if (num_slices < 1) {
    cli::cli_abort("Hold up! {.var num_slices} must have at least one slice.")
  }


  if (length(terra::cells(all_cov)) > num_sample){
    cli::cli_alert_warning("{.var num_sample} is greater than possible sampling options available and will be
                             reduced")
    num_sample = length(terra::cells(all_cov))

  }

  layer_names <- names(all_cov)
  samp_dat <- terra::spatSample(all_cov ,
                                size = num_sample,
                                method = "regular",
                                xy = TRUE,
                                as.df = F)
  samp_dat <- samp_dat[!is.na(samp_dat[,"cost"]) & !is.infinite(samp_dat[,ncol(samp_dat)]),]

  coords <- samp_dat[,c("x","y")]
  curr_dat <- samp_dat[,layer_names]

  if(is.null(to_include)){
    inc_idx <- NULL
    size = n_points
  }else{
    inc_pts <- terra::extract(all_cov, to_include)
    inc_pts <- inc_pts[,-(1)]
    inc_pts <- sf::st_as_sf(inc_pts)
    inc_idx <- 1:nrow(inc_pts)
    size = n_points + nrow(inc_pts)
    curr_dat <- rbind(to_include, curr_dat)
    include_coords <- sf::st_coordinates(to_include)
    coords <- rbind(coords, include_coords)
  }

  if(num_slices == 1){
    cli::cli_alert_success("Gen-R-ating one slice...")

    for(i in 1:5){
      templhs <- clhs(curr_dat,
                      size = size,
                      must.include = inc_idx,
                      iter = 20000 ,
                      simple = FALSE,
                      progress = TRUE,
                      cost = "cost",
                      use.cpp = T,
                      latlon = coords)#,
      #min.dist = min_dist)
      #if(sum(templhs$final_obj_distance) == 0) break
    }

  }else{
    cli::cli_alert_success("Gen-R-ating multiple slices...")

    for(snum in 1:num_slices){
      # snum = 1
      for(i in 1:5){
        templhs <- clhs::clhs(curr_dat,
                              size = snum * size,
                              must.include = inc_idx,
                              iter = 20000 ,
                              simple = FALSE,
                              progress = TRUE,
                              cost= "cost",
                              use.cpp = T,
                              latlon = coords)
      }
      inc_idx <- templhs$index_samples
    }
  }
  out <- data.table::as.data.table(samp_dat[templhs$index_samples,])
  out[,`:=`(slice_num = rep(num_slices:1,each = n_points),
            point_num = rep(1:n_points, times = num_slices))]
  out_sf <- sf::st_as_sf(out,coords = c("x","y"),crs = 3005)
  terra::plot(all_cov$cost)
  terra::points(terra::vect(out_sf["slice_num"]))
  return(out_sf)
}
