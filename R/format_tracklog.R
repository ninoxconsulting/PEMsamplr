#' Format Track log data
#'
#' Standardize tracks collected during the field sampling process
#'
#' @param data_dir text string with location of raw files in shp or gpk format
#' @param transect_layout sf object with simplified transect layout
#' @param buffer numeric value for buffer distance around transect layout
#'
#' @return A`sf` multiline vector
#' @export
#'
#' @examples
#' \dontrun{
#' clean_tracks <- format_tracklog(inputfolder, transect_layout, buffer = 10)
#' }
#'
format_tracklog <- function(data_dir = NULL, transect_layout, buffer = 10) {
  # data_dir <- rawdat
  # buffer = 10

  transect_layout_buf <- sf::st_buffer(transect_layout, buffer)
  sf::st_geometry(transect_layout_buf) <- "geom"

  if (!inherits(data_dir, "character") || !fs::dir_exists(data_dir)) {
    cli::cli_abort("{.var datafolder} must be a directory path")
  }

  lines <- fs::dir_ls(path = data_dir, recurse = TRUE, regexp = ".gpkg$|.shp$")

  all_lines <- purrr::map(lines, function(i) {
    # i <- lines[1]

    s1_layers <- sf::st_layers(i)
    lns <- which(s1_layers[["geomtype"]] %in% c("LINE", "LINESTRING", "3D Line String", "3D Measured Multi Line String", "3D Multi Line String"))

    if (length(lns) > 0) {

      tdat <- sf::st_read(i, quiet = TRUE) |>
        sf::st_transform(3005) |>
        sf::st_zm() |>
        dplyr::rename_all(.funs = tolower)


      # 1) fix date and times

      tdat <- .fix_timestamp(tdat)


      # 2) check the transact id

      if (anyNA(sf::st_is_valid(tdat) == T)) {
        inval <- sf::st_is_valid(tdat)
        which(inval == TRUE)

        fixed <- tdat[which(inval == TRUE), ]
        tdat <- fixed
      }


      # 3) intersect with transect layout to define transect_id

      tdat <- .transect_intersect(tdat, transect_layout_buf)


      # 3) assign data type

      tdat <- tdat |>
        dplyr::mutate(data_type = ifelse(is.na(transect_id), "incidental", "s1"))



      # 4) add missing columns if not in data

      tdat <- .add_missing_cols(tdat, c("photos", "date_ymd", "time_hms"))



      # 5) filter columns

      tdat <- tdat |>
        dplyr::select(any_of(c("transect_id", "date_ymd", "time_hms", "photos", "data_type")))

      sf::st_geometry(tdat) <- "geom"

      tdat
    }
  }) |> dplyr::bind_rows()


  return(all_lines)
}


