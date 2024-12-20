#' Format field data
#'
#' Standardizes format from raw field data to standard point data
#'
#' @param data_dir text string with location of raw files in shp or gpk format
#' @param transect_layout sf object with simplified transect layout
#' @param buffer numeric value for buffer distance around transect layout
#'
#' @return sf point data with standardized and consolidated datasets
#' @export
#'
#' @examples
#' \dontrun{
#' clean_pts <- format_fielddata(inputfolder, transect_layout, buffer = 10)
#' }
format_fielddata <- function(data_dir = NULL, transect_layout, buffer = 10) {

  #  datafolder <- rawdat
  #  buffer = 10

  transect_layout_buf <- sf::st_buffer(transect_layout, buffer)
  sf::st_geometry(transect_layout_buf) <- "geom"

  if (!inherits(data_dir, "character") || !fs::dir_exists(data_dir)) {
    cli::cli_abort("{.var datafolder} must be a directory path")
  }


  points <- fs::dir_ls(path = data_dir, recurse = TRUE, regexp = ".gpkg$|.shp$")
  # points <- points[c(2,4)]

  all_points <- purrr::map(points, function(i) {

    # apply function to point datatypes only

    s1_layers <- sf::st_layers(x)
    pts <- which(s1_layers[["geomtype"]] %in% c("Point", "3D Point", "3D Measured Point"))

    if (length(pts) > 0) {
      points_read <- sf::st_read(x, quiet = TRUE) |>
        sf::st_transform(3005) |>
        sf::st_zm() |>
        dplyr::rename_all(.funs = tolower)

      sf::st_geometry(points_read) <- "geom"

      start_length <- length(points_read$geom)


      # 1) check the names of the columns are unifom across all files

      points_read <- .check_col_names(points_read)


      # 2) fix date and times.

      if ("timestamp" %in% names(points_read)) {
        points_read <- points_read |>
          dplyr::mutate(date_ymd = lubridate::as_date(points_read$timestamp))

        if (stringr::str_length(points_read$timestamp[1]) > 10) {
          points_read <- points_read |>
            dplyr::mutate(date_time = lubridate::as_datetime(points_read$timestamp))

          points_read <- points_read |>
            dplyr::mutate(time_hms = format(date_time, format = "%H:%M:%S"))
        } else {
          points_read <- dplyr::mutate(points_read, time_hms = NA)
        }
      }


      # 3) An a order to points

      if ("name" %in% names(points_read)) {
        points_read <- points_read |>
          dplyr::mutate(order = as.numeric(gsub("Placemark ", "", name)))
      }

      if ("objectid" %in% names(points_read)) {
        points_read <- points_read |>
          dplyr::mutate(order = as.numeric(objectid))
      }

      if (("order" %in% names(points_read)) == FALSE) {
        points_read <- points_read |>
          dplyr::mutate(order = as.numeric(seq(1, length(points_read$geom), 1)))
      }


      # 4) add the transect id number using the transect layout buffered.

      if ("id" %in% names(points_read)) {

        # print("transect id already present")

      } else {

        points_read <-
          sf::st_join(points_read, transect_layout_buf, join = sf::st_intersects)
        points_read <- points_read |>
          dplyr::rename_all(.funs = tolower) |>
          dplyr::distinct()

        points_read <- points_read |>
          dplyr::mutate(id = gsub("\\s", "", id)) |>
          dplyr::mutate(transect_id = id)
      }


      # 5) assign incidental to points outside the transect buffer and give warning

      if (any(is.na(unique(points_read$transect_id)))) {
        points_read <- points_read |>
          dplyr::mutate(data_type = ifelse(is.na(transect_id), "incidental", "s1"))

        cli::cli_alert_warning("points outside the transect buffer, assigned to incidental,
                               please check these and re-run if needed")
      }

      # 6) add observer name to points

      points_read <- points_read |>
        dplyr::mutate(observer = stringr::str_trim(observer)) |>
        dplyr::mutate(observer = dplyr::na_if(observer, ""))

      if (all(is.na(points_read$observer))) {
        # print(x)
        #  cli::cli_abort("observer name missing in original data, check and re-run the above transect data")
      } else {
        # print ("filling observer names")

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

      points_read <- .add_missing_cols(points_read)

      # 10) reorder and subset cols of interest

      points_read <- points_read |>
        dplyr::select(any_of(c(
          "order", "mapunit1", "mapunit2", "point_type", "transect_id",
          "observer", "transition", "struc_stage", "struc_mod",
          "date_ymd", "time_hms", "edatope", "comments", "photos", "data_type"
        ))) |>
        dplyr::group_by(transect_id) |>
        dplyr::arrange(as.numeric(order), by_group = TRUE) |>
        dplyr::ungroup()

      sf::st_geometry(points_read) <- "geom"

      endlength <- length(points_read$order)


      if (endlength != start_length) {
        cli::cli_warning("length of input file does not match cleaned file review raw data:")
        print(x)
      }

      points_read
    }
  }) |> dplyr::bind_rows()


  return(all_points)
}




.fill_observer <- function(input_data) {
  # input_data <- points_read

  observer_key <- input_data |>
    dplyr::select(.data$transect_id, .data$observer) |>
    dplyr::rename("observer_fill" = .data$observer) |>
    sf::st_drop_geometry() |>
    dplyr::distinct() |>
    stats::na.omit() |>
    dplyr::mutate(.data$observer_fill = trimws(.data$observer_fill, which = "both")) |>
    dplyr::filter(.data$observer_fill != "")

  if (length(observer_key$transect_id) != length(unique(observer_key$transect_id))) {
    cli::cli_abort(" number of observers does not match unique transect number")
  }

  input_data <- input_data |>
    dplyr::group_by(.data$transect_id) |>
    tidyr::fill(.data$observer, .direction = "downup") |>
    dplyr::ungroup()

  return(input_data)
}






.check_col_names <- function(points_read) {
  # 1) transect name
  if ("f01_transec" %in% names(points_read)) {
    dnames <- names(points_read)
    colnames(points_read) <- gsub("f0", "x0", dnames)
    points_read <- points_read |>
      dplyr::mutate(x10_edatope = f10_edatope)
  }

  if ("f01_transe" %in% names(points_read)) {
    dnames <- names(points_read)
    colnames(points_read) <- gsub("f0", "x0", dnames)
  }

  if ("f01_transect_id" %in% names(points_read)) {
    dnames <- names(points_read)
    colnames(points_read) <- gsub("f0", "x0", dnames)
  }

  if ("x01_trans" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(transect_id = x01_trans)
  }

  if ("x01_transec" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(transect_id = x01_transec)
  }

  if ("x01_transe" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::mutate() |>
      dplyr::rename(transect_id = x01_transe)
  }

  if ("x01_transect_id" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::mutate() |>
      dplyr::rename(transect_id = x01_transect_id)
  }

  # 2) observer
  if ("x02_observe" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(observer = x02_observe)
  }

  if ("x02_observer" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(observer = x02_observer)
  }

  if ("x02_observ" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::mutate() |>
      dplyr::rename(observer = x02_observ)
  }

  if ("x1observer" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(observer = x1observer)
  }

  # 3) point_type
  if ("x03_pt_type" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(point_type = x03_pt_type)
  }
  # 3) point_type
  if ("x03_pt_typ" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(point_type = x03_pt_typ)
  }

  # 3) point_type
  if ("x6pointtype" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(point_type = x6pointtype)
  }
  # 3) point_type
  if ("pt_type" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(point_type = pt_type)
  }

  # 4) Mapunit1
  if ("x04_mapunit" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(mapunit1 = x04_mapunit)
  }

  # 4) Mapunit1
  if ("x04_mapuni" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(mapunit1 = x04_mapuni)
  }

  if ("x04_mapunit1" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(mapunit1 = x04_mapunit1)
  }

  #  Mapunit1
  if ("x2mapunit1" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(mapunit1 = x2mapunit1)
  }
  #  Mapunit1
  if ("x2mapunit" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(mapunit1 = x2mapunit)
  }

  # 5) Mapunit2
  if ("x06_mapunit" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(mapunit2 = x06_mapunit)
  }

  if ("x06_mapunit2" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(mapunit2 = x06_mapunit2)
  }

  if ("x4mapunit2" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(mapunit2 = x4mapunit2)
  }

  # 5) Mapunit2
  if ("x06_mapuni" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(mapunit2 = x06_mapuni)
  }

  # 6) transtition
  if ("x05_transit" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(transition = x05_transit)
  }

  if ("x05_transi" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(transition = x05_transi)
  }

  if ("x05_transition" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(transition = x05_transition)
  }
  if ("x3transitio" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(transition = x3transitio)
  }


  # 7) Stand struc
  if ("x07_struc_" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(struc_stage = x07_struc_)
  }

  if ("x07_struct" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(struc_stage = x07_struct)
  }

  if ("x07_struct_" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(struc_stage = x07_struct_)
  }

  if ("x07_struct_stage" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(struc_stage = x07_struct_stage)
  }
  if ("x7structsta" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(struc_stage = x7structsta)
  }

  # 8) Stand struc
  if ("x08_struct" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(struc_mod = x08_struct)
  }

  # 8) Stand struc
  if ("x08_struct_" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(struc_mod = x08_struct_)
  }

  if ("x08_struct_stage_mod" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(struc_mod = x08_struct_stage_mod)
  }

  # 9) Edatope
  if ("x10_edatope" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(edatope = x10_edatope)
  }

  # 9) Edatope
  if ("x6edatope" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(edatope = x6edatope)
  }

  # 10) comments
  if ("x5comments" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(comments = x5comments)
  }
  if ("x09_comment" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(comments = x09_comment)
  }

  if ("x09_comments" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(comments = x09_comments)
  }

  if ("x5comments" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(comments = x5comments)
  }
  if ("x09_commen" %in% names(points_read)) {
    points_read <- points_read |>
      dplyr::rename(comments = x09_commen)
  }

  return(points_read)
}


# add missing columns if not in data

.add_missing_cols <- function(points_read) {
  if ("photos" %in% names(points_read)) {

  } else {
    # add missing columns if not in data
    if ("pdfmaps_ph" %in% names(points_read)) {
      points_read <- points_read |>
        dplyr::mutate(photos = pdfmaps_ph)
    } else {
      points_read <- points_read |>
        dplyr::mutate(photos = NA)
    }
  }

  if ("comments" %in% names(points_read)) {

  } else {
    points_read <- points_read |>
      dplyr::mutate(comments = NA)
  }

  if ("edatope" %in% names(points_read)) {

  } else {
    points_read <- points_read |>
      dplyr::mutate(edatope = NA)
  }


  if (("date_ymd" %in% names(points_read)) == FALSE) {
    points_read <- points_read |>
      dplyr::mutate(
        date_ymd = NA,
        time_hms = NA
      )
  }

  if ("struc_stage" %in% names(points_read)) {

  } else {
    points_read <- points_read |>
      dplyr::mutate(struc_stage = NA)
  }

  if ("struc_mod" %in% names(points_read)) {

  } else {
    points_read <- points_read |>
      dplyr::mutate(struc_mod = NA)
  }

  return(points_read)
}
