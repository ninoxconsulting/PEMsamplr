test_that("create_binned_landscape fails with invalid inputs", {

  outdir <- withr::local_tempdir()
  aoi <- fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg")
  aoi_rast <- PEMprepr::create_template_raster(aoi, res = 50, write_output = FALSE)
  terra::writeRaster(aoi_rast, fs::path(outdir, "aoi0.tif"))
  aoi_rast1 <- aoi_rast
  terra::values(aoi_rast1) <- 1
  names(aoi_rast1) <- "rast1"
  terra::writeRaster(aoi_rast1, fs::path(outdir, "ao1.tif"))
  aoi_rast2 <- aoi_rast
  terra::values(aoi_rast2) <- 2
  names(aoi_rast2) <- "rast2"
  terra::writeRaster(aoi_rast2, fs::path(outdir, "a2.tif"))

  #fs::dir_ls(fs::path(outdir))

  # perhaps this is a little fragile as dependant on naming of files
  expect_error(create_binned_landscape(in_dir = outdir,
                         layers = c("aoi", "aoi2", "aoi3")))

  expect_error(create_binned_landscape(in_dir = fs::path(outdir, "test"),
                          layers = c("aoi", "aoi2", "aoi3")))

})

test_that("create_binned_landscape output is correct format", {

  outdir <- withr::local_tempdir()
  aoi <- fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg")
  aoi_rast <- PEMprepr::create_template_raster(aoi, res = 50, write_output = FALSE)
  terra::writeRaster(aoi_rast, fs::path(outdir, "aoi0.tif"))
  aoi_rast1 <- aoi_rast
  terra::values(aoi_rast1) <- 1
  names(aoi_rast1) <- "rast1"
  terra::writeRaster(aoi_rast1, fs::path(outdir, "ao1.tif"))
  aoi_rast2 <- aoi_rast
  terra::values(aoi_rast2) <- 2
  names(aoi_rast2) <- "rast2"
  terra::writeRaster(aoi_rast2, fs::path(outdir, "a2.tif"))
  #
  #fs::dir_ls(fs::path(outdir))
  #

  bb <- create_binned_landscape(in_dir = outdir,
                                layers = c("aoi0", "ao1", "a2"),
                                write_output = FALSE)

  expect_s4_class(bb, "SpatRaster")

})
