#' Simplify sample plan layout for post processing
#'
#' @param input_path A `character` string or path where sample plan layout geopackage is stored.
#'  Default location is based on standard workflow. Note this function is set up to work with
#'  multiple files so ensure any `.gpkg` files in this folder are part of the sample plan.
#' @param out_dir A path to the location in which the simplified transect layout will be
#' saved. Default location is based on standard workflow.
#' @param write_output should the simplifeid transect layout sf object be written to disk?
#'     If `TRUE` (default), will write to `out_dir`. Default location is based
#'     on standard workflow.
#' @param overwrite a `logical` to determine if the output file be overwritten
#'      if it already exists? Only used when `write_output = TRUE`. Default is `FALSE`.
#'
#' @return sf object of simplified transect layout
#' @export
#'
#' @examples
#' \dontrun{
#' t1 <- simplify_transectlayout(
#'     input_dir = fs::path(PEMprepr::read_fid()$dir_20104020_transect$path_abs),
#'     out_dir = fs::path(PEMprepr::read_fid()$dir_20104020_transect$path_abs),
#'     write_output = TRUE,
#'     overwrite = FALSE)
#'}
simplify_transectlayout <- function(input_path = fs::path(PEMprepr::read_fid()$dir_20104020_transect$path_rel),
                                    out_dir = fs::path(PEMprepr::read_fid()$dir_20104020_transect$path_rel),
                                    write_output = TRUE,
                                    overwrite = FALSE){

  # check which files are in the folder
  trans <- list.files(input_path, pattern = ".gpkg$", full.names = TRUE, recursive = FALSE)

  transect_layout <- purrr::map(trans, function(i) {
    #i = trans[1]
    clhs_layers <- sf::st_layers(i)
    lines <- which(clhs_layers[["geomtype"]] %in% c("Line String", "Multi Line String"))
    if (length(lines)) {
      transect_layout_lines <- purrr::map(clhs_layers$name[lines], function(y) {
        # y = lines$name[1]
        transect <- sf::st_read(i, layer = y, quiet = TRUE)
        names(transect) <- tolower(names(transect))
        transect <- transect[, "id", drop = FALSE]
        transect$id <- as.character(transect$id)
        sf::st_transform(transect, 3005)
      })|>
        dplyr::bind_rows()
    }
  }) |>
    dplyr::bind_rows() |>
    unique()

  if (write_output) {

    file_path <- fs::path(out_dir, "transect_layout.gpkg")
    if (file.exists(file_path) && !overwrite) {
      cli::cat_line()
      cli::cli_alert("Transect layout geopackage already exists. Use overwrite = TRUE to overwrite.")
    } else {
      if (file.exists(file_path)) {
        file.remove(file_path)
        cli::cat_line()
        cli::cli_alert("Overwriting existing transect layout .gpkg")
      }
      sf::st_write(transect_layout, file_path, driver = "GPKG", quiet = TRUE)
      cli::cat_line()
      cli::cli_alert_success("Transect layout geopackage written to {.path {file_path}}")
    }
  }


  return(transect_layout)

}
