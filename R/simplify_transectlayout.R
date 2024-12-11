#' Simplify sample plan layout for post processing
#'
#' @param input_path A character string of path where sample plan layout geopackage is stored.
#'  Default location is based on standard workflow.
#' @param out_dir A path to the location in which the simplifeid transect layout will be
#' saved. Default location is based on standard workflow.
#' @param writeout should the simplifeid transect layout sf object be written to disk?
#'     If `TRUE` (default), will write to `out_dir`. Default location is based
#'     on standard workflow.
#' @param overwrite a `logical` to determine if the output file be overwritten
#'      if it already exists? Only used when `writeout = TRUE`. Default is `FALSE`.
#'
#' @return sf object of simplified transect layout
#' @export
#'
#' @examples
#' \dontrun{
#' t1 <- simplify_transectlayout(
#'     input_dir = fs::path(PEMprepr::read_fid()$dir_20104020_transect$path_abs),
#'     out_dir = fs::path(PEMprepr::read_fid()$dir_20104020_transect$path_abs),
#'     writeout = TRUE,
#'     overwrite = FALSE)
#'}
simplify_transectlayout <- function(input_path = fs::path(PEMprepr::read_fid()$dir_20104020_transect$path_abs),
                                    out_dir = fs::path(PEMprepr::read_fid()$dir_20104020_transect$path_abs),
                                    writeout = TRUE,
                                    overwrite = FALSE){

  trans <- list.files(input_path, pattern = ".gpkg$", full.names = TRUE, recursive = FALSE)

  transect_layout <- do.call(rbind, lapply(trans, function(x) {
    clhs_layers <- sf::st_layers(x)
    lines <- which(clhs_layers[["geomtype"]] %in% c("Line String", "Multi Line String"))
    if (length(lines)) {
      do.call(rbind, lapply(clhs_layers$name[lines], function(y) {
        transect <- sf::st_read(x, y, quiet = TRUE)
        names(transect) <- tolower(names(transect))
        transect <- transect[, "id", drop = FALSE]
        transect$id <- as.character(transect$id)
        sf::st_transform(transect, 3005)
      }))
    }
  }))

  transect_layout <- unique(transect_layout)

  #if write out is true, check if file exists and overwrite if necessary

  if (writeout) {

    file_path <- fs::path(out_dir, "transect_layout.gpkg")
    if (file.exists(file_path) && !overwrite) {
      cli::cli_alert("Transect layout geopackage already exists. Use overwrite = TRUE to overwrite.")
    } else {
      if (file.exists(file_path)) {
        file.remove(file_path)
        cli::cli_alert("Overwriting existing transect layout .gpkg")
      }
      sf::st_write(transect_layout, file_path, driver = "GPKG", quiet = TRUE)
      cli::cli_alert_success("Transect layout geopackage written to {.path {file_path}}")
    }
  }


  return(transect_layout)

}
