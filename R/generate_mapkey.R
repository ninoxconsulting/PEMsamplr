#' Generate a mapkey from provincial BEC unit list and flag unmatched field data
#'
#' @param data_pts A `sf`object with standardised attribute fields
#' @param key A `data.frame` with the provincial BEC unit list. This is an internal dataset
#' @param write_output A `logical`if the map key should be be written to disk?
#'     If `TRUE` (default), will write to `out_dir` under the appropriate resolution subfolder.
#' @param out_dir A character string of path which points to output location. A default
#'    location and name are applied in line with standard workflow.
#' @param out_name A character string of the output file name. Default is `mapunitkey.csv`
#'
#' @returns a tibble with unique mapkey for the study area along with unmatched fieldcalls to manually review
#' @export
#'
#' @examples
#' \dontrun{
#' key <- generate_mapkey(
#'   data_pts = fs::path(
#'     PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel,
#'     "s1_points_raw.gpkg"
#'   ),
#'   key = utils::read.csv(fs::path_package("PEMsamplr", "extdata/mapkey_all_draft.csv")),
#'   write_output = FALSE,
#'   out_dir = fs::path(PEMprepr::read_fid()$dir_3010_inputs$path_rel),
#'   out_name = "mapunitkey.csv"
#' )
#' }
generate_mapkey <- function(data_pts,
                            key = utils::read.csv(fs::path_package("PEMsamplr", "extdata/mapkey_all_draft.csv")),
                            write_output = FALSE,
                            out_dir = fs::path(PEMprepr::read_fid()$dir_3010_inputs$path_rel),
                            out_name = "mapunitkey.csv") {
  # read in points if needed
  tps <- PEMprepr:::read_sf_if_necessary(data_pts)

  # generate outname
  outfile <- fs::path(out_dir, out_name)

  # if file exists
  if (fs::file_exists(outfile) & write_output == FALSE) {
    cli::cat_line()
    cli::cli_abort("WARNING! {.var {outfile}} already exists, use write_output = TRUE to overwrite this file")
  }

  # format spaces
  tps <- tps |>
    sf::st_drop_geometry() |>
    dplyr::mutate(mapunit1 = stringr::str_trim(.data$mapunit1)) |>
    dplyr::mutate(mapunit2 = stringr::str_trim(.data$mapunit2)) |>
    dplyr::select(.data$mapunit1, .data$mapunit2)

  # get unique list of mapunits
  allmapunits <- unique(c(tps$mapunit1, tps$mapunit2))

  # subset the mapkey which match and output a csv
  output <- key |>
    dplyr::filter(.data$basemapunit %in% allmapunits)


  # check if all names appear in list
  if (!all(allmapunits %in% key$basemapunit)) {
    cli::cat_line()
    cli::cli_alert_warning("The field data contains non-standard mapunits and requires manual review.
      Please review the output file {.var {outfile}} and add equivalent mapunit names to the 'basemapunit' field for the following units: ")
    unmatched_units <- dplyr::setdiff(allmapunits, key$basemapunit)
    print(unmatched_units)

    # add unmatched field calls to key
    toadd <- tibble::as_tibble_col(unmatched_units, column_name = "fieldcall")
    output <- dplyr::bind_rows(output, toadd)
  }

  if (write_output) {
    if (fs::file_exists(outfile)) {
      cli::cat_line()
      cli::cli_alert_warning("file already exists at {.var {outfile}}, this file will be overwriten")
    }

    output <- utils::write.csv(output, fs::path(outfile), row.names = FALSE)
  }

  return(output)
}
