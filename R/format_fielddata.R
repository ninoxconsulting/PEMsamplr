#' Format field data
#'
#' Standardizes format from raw field data to standard point data
#'
#' @param data_dir text string with location of raw files in shp or gpk format
#' @param transect_layout A `sf` object with simplified transect layout
#' @param buffer numeric value for buffer distance around transect layout
#' @param write_output should the sf object be written to disk?
#'     If `TRUE` (default), will write to `out_dir` under the appropriate resolution subfolder.
#' @param out_dir A character string of path which points to output location. A default
#'    location and name are applied in line with standard workflow.
#' @param out_name A character string of the output file name. Default is `s1_points_raw.gpkg`
#' @return sf point data with standardized and consolidated datasets
#' @export
#'
#' @examples
#' \dontrun{
#' clean_pts <- format_fielddata(inputfolder, transect_layout, buffer = 10)
#' }
format_fielddata <- function(data_dir = NULL,
                             transect_layout,
                             buffer = 10,
                             write_output = TRUE,
                             out_dir = fs::path(PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel),
                             out_name = "s1_points_raw.gpkg"){
  # data_dir <- rawdat
  # buffer = 10

  # add check for transect_layout
  if (!inherits(transect_layout, "sf")) {
    cli::cat_line()
    cli::cli_abort("{.var transect_layout} must be an sf object")
  }

  # add check for buffer
  if (!is.numeric(buffer)) {
    cli::cat_line()
    cli::cli_abort("{.var buffer} must be numeric")
  }

  # add check for out_dir
  if (!inherits(out_dir, "character") || !fs::dir_exists(out_dir)) {
    cli::cat_line()
    cli::cli_abort("{.var out_dir} must be a directory path")
  }


  transect_layout_buf <- sf::st_buffer(transect_layout, buffer)
  sf::st_geometry(transect_layout_buf) <- "geom"

  if (!inherits(data_dir, "character") || !fs::dir_exists(data_dir)) {
    cli::cat_line()
    cli::cli_abort("{.var data_dir} must be a directory path")
  }


  if (!inherits(data_dir, "character") || !fs::dir_exists(data_dir)) {
    cli::cat_line()
    cli::cli_abort("{.var data_dir} must be a directory path")
  }


  points <- fs::dir_ls(path = data_dir, recurse = TRUE, regexp = ".gpkg$|.shp$")

  if(length(points) == 0){
    cli::cat_line()
    cli::cli_abort("no files found in {.var data_dir}")
  }


  all_points <- purrr::map(points, function(i) {

    s1_layers <- sf::st_layers(i)
    pts <- which(s1_layers[["geomtype"]] %in% c("Point", "3D Point", "3D Measured Point"))

    if (length(pts) > 0) {
      points_read <- sf::st_read(i, quiet = TRUE) |>
        sf::st_transform(3005) |>
        sf::st_zm() |>
        dplyr::rename_all(.funs = tolower)

      sf::st_geometry(points_read) <- "geom"

      start_length <- length(points_read$geom)


      # 1) check the names of the columns are unifom across all files

      points_read <- .check_col_names(points_read)


      # 2) fix date and times.

      points_read <- .fix_timestamp(points_read)


      # 3) An a order to points

      if ("name" %in% names(points_read)) {
        points_read <- points_read |>
          dplyr::mutate(order = as.numeric(gsub("Placemark ", "", .data$name)))
      }

      if ("objectid" %in% names(points_read)) {
        points_read <- points_read |>
          dplyr::mutate(order = as.numeric(.data$objectid))
      }

      if (("order" %in% names(points_read)) == FALSE) {
        points_read <- points_read |>
          dplyr::mutate(order = as.numeric(seq(1, length(points_read$geom), 1)))
      }


      # 4) add the transect id number using the transect layout buffered.

      points_read <- .transect_intersect(points_read, transect_layout_buf)



      # 5) assign incidental to points outside the transect buffer and give warning

      if (any(is.na(unique(points_read$transect_id)))) {
        points_read <- points_read |>
          dplyr::mutate(data_type = ifelse(is.na(.data$transect_id), "incidental", "s1"))
        cli::cat_line()
        cli::cli_alert_warning("points outside the transect buffer, assigned to incidental,
                               please check these and re-run if needed")
      }


      # 6) add observer name to points

      points_read <- points_read |>
        dplyr::mutate(observer = stringr::str_trim(.data$observer)) |>
        dplyr::mutate(observer = dplyr::na_if(.data$observer, ""))

      if (all(is.na(points_read$observer))) {
        cli::cat_line()
        cli::cli_abort("observer name missing in original data, check and re-run the above transect data")
      } else {

        points_read <- .fill_observer(points_read)
      }


      # 7) check the mapunit 1 is filled if mapunit 2 is not NA

      points_read <- points_read |>
        dplyr::mutate(mapunit1 = dplyr::case_when(
          is.na(mapunit1) & !is.na(mapunit2) ~ mapunit2,
          is.null(mapunit1) & !is.na(mapunit2) ~ mapunit2,
          mapunit1 == " " & !is.na(mapunit2) ~ mapunit2,
          TRUE ~ as.character(mapunit1)
        ))

      points_read <- points_read |>
        dplyr::mutate(mapunit2 = dplyr::case_when(
          mapunit1 == mapunit2 ~ NA_character_,
          TRUE ~ as.character(mapunit2)
        ))


      # 8) add back sight/ line of site check

      # STILL TO ADD


      # 9) add missing columns if not in data

      points_read <- .add_missing_cols(
        points_read,
        c(
          "photos", "comments", "date_ymd",
          "time_hms", "struc_stage", "struc_mod"
        )
      )

      # 10) reorder and subset cols of interest

      points_read <- points_read |>
        dplyr::select(dplyr::any_of(c(
          "order", "mapunit1", "mapunit2", "point_type", "transect_id",
          "observer", "transition", "struc_stage", "struc_mod",
          "date_ymd", "time_hms", "edatope", "comments", "photos", "data_type"
        ))) |>
        dplyr::group_by(.data$transect_id) |>
        dplyr::arrange(as.numeric(order), by_group = TRUE) |>
        dplyr::ungroup()

      sf::st_geometry(points_read) <- "geom"

      endlength <- length(points_read$order)

      if (endlength != start_length) {
        cli::cat_line()
        cli::cli_alert_warning("length of input file does not match cleaned file review raw data:")
      }

      points_read
    }
  }) |> dplyr::bind_rows()


  if(write_output) {

    out_loc <- fs::path(out_dir, out_name)

    #if file exists
    if(fs::file_exists(out_loc)){
      cli::cat_line()
      cli::cli_alert_warning("file already exists at {.var out_dir}, this file will be overwriten")
    }

    sf::st_write(all_points, out_loc, driver = "GPKG", append = FALSE)
    cli::cat_line()
    cli::cli_alert_success("field data formatted and written to {.var {out_dir}}")

  }

  return(all_points)
}


# split timestamp into date and time

.fix_timestamp <- function(points_read){
  if ("timestamp" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::mutate(date_ymd = lubridate::as_date(points_read$timestamp))

    if (stringr::str_length(points_read$timestamp[1]) > 10) {
      points_read <- points_read |>
        dplyr::mutate(date_time = lubridate::as_datetime(points_read$timestamp))

      points_read <- points_read |>
        dplyr::mutate(time_hms = format(.data$date_time, format = "%H:%M:%S"))
    }
  }
  return(points_read)
}



# fill observer name

.fill_observer <- function(input_data) {

  observer_key <- input_data |>
    dplyr::select(.data$transect_id, .data$observer) |>
    dplyr::rename("observer_fill" = .data$observer) |>
    sf::st_drop_geometry() |>
    dplyr::distinct() |>
    stats::na.omit() |>
    dplyr::mutate(observer_fill = trimws(.data$observer_fill, which = "both")) |>
    dplyr::filter(.data$observer_fill != "")

  if (length(observer_key$transect_id) != length(unique(observer_key$transect_id))) {
    cli::cat_line()
    cli::cli_abort(" number of observers does not match unique transect number")
  }

  input_data <- input_data |>
    dplyr::group_by(.data$transect_id) |>
    tidyr::fill(.data$observer, .direction = "downup") |>
    dplyr::ungroup()

  return(input_data)
}



.check_col_names <- function(points_read) {

  # update "f0 cols to "x0 columns
  points_read <- points_read |>
    dplyr::rename_with(.fn = ~ gsub("f0", "x0", .x, fixed = TRUE), .col = dplyr::starts_with("f0")) |>
    dplyr::rename_with(.fn = ~ gsub("f1", "x1", .x, fixed = TRUE), .col = dplyr::starts_with("f1"))

  # recode the new and old names into table

  recode_df <- data.frame(
    old = c(
      "x01_transect_id", "x01_transec", "x01_transe", "x01_trans",
      "x1observer", "x02_observ", "x02_observer", "x02_observe",
      "pt_type", "x6pointtype", "x03_pt_typ", "x03_pt_type",
      "x2mapunit", "x2mapunit1", "x04_mapunit1", "x04_mapuni", "x04_mapunit",
      "x06_mapuni", "x4mapunit2", "x06_mapunit2", "x06_mapunit",
      "x3transitio", "x05_transition", "x05_transi", "x05_transit",
      "x07_struc_", "x07_struct", "x07_struct_", "x07_struct_stage", "x7structsta",
      "x08_struct_", "x08_struct_", "x08_struct_stage_mod",
      "x10_edatope", "x6edatope",
      "x09_commen", "x5comments", "x09_comment"
    ),
    new = c(
      "transect_id", "transect_id", "transect_id", "transect_id",
      "observer", "observer", "observer", "observer",
      "point_type", "point_type", "point_type", "point_type",
      "mapunit1", "mapunit1", "mapunit1", "mapunit1", "mapunit1",
      "mapunit2", "mapunit2", "mapunit2", "mapunit2",
      "transition", "transition", "transition", "transition",
      "struc_stage", "struc_stage", "struc_stage", "struc_stage", "struc_stage",
      "struc_mod", "struc_mod", "struc_mod",
      "edatope", "edatope",
      "comments", "comments", "comments"
    )
  )


  recode_vec <- stats::setNames(recode_df$old, recode_df$new)

  points_read <- points_read |>
    dplyr::rename(dplyr::any_of(recode_vec))

  return(points_read)
}



# add missing columns if not in data

.add_missing_cols <- function(points_read, cols) {
  add <- cols[!cols %in% names(points_read)]
  if (length(add) != 0) points_read[add] <- NA
  return(points_read)
}


# intersect with transect layout to define transect_id

.transect_intersect <- function(points_read, transect_layout_buf) {
  points_read <- sf::st_join(points_read, transect_layout_buf, join = sf::st_intersects)
  points_read <- points_read |>
    dplyr::rename_all(.funs = tolower) |>
    dplyr::distinct()

  points_read <- points_read |>
    dplyr::mutate(id = gsub("\\s", "", .data$id)) |>
    dplyr::mutate(transect_id = .data$id)

return(points_read)
}
