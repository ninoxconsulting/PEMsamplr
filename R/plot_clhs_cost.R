#' Plot the cost of clhs sample plans
#'
#' @param clhs_dir A path or directory where the multiple clhs sample plans are saved.
#'  Default location is based on standard workflow.
#'
#' @return ggplt2 plot
#' @export
#'
#' @examples
#' \dontrun{
#'check_clhs_cost(PEMprepr::read_fid()$dir_20102010_clhs$path_abs)
#' }
plot_clhs_cost <- function(
    clhs_dir = PEMprepr::read_fid()$dir_20102010_clhs$path_abs) {
  ftemp <- fs::dir_ls(clhs_dir, regexp = ".gpkg$")

  all_samples <- foreach::foreach(fs = 1:length(ftemp), .combine = "rbind") %do% {
    ff <- ftemp[fs]
    layers <- sf::st_layers(ff)$name

    sample_points_all <- foreach::foreach(l = 1:length(layers), .combine = rbind) %do% {
      ll <- layers[l]
      sptemp <- sf::st_read(ff, layer = ll, quiet = TRUE)
      sptemp <- sptemp |>
        dplyr::mutate(clhs_repeat = basename(ff)) |>
        dplyr::mutate(bgc = stringr::str_extract(layers, "[^_]+"))

      sptemp
    }
  }

  # summarise the costs per sample plan
  repsum <- all_samples |>
    sf::st_drop_geometry() |>
    dplyr::select(all_samples$clhs_repeat, all_samples$bgc, all_samples$cost) |>
    dplyr::mutate(file_name = gsub("_clhs_sample", "", all_samples$clhs_repeat)) |>
    dplyr::mutate(file_name = gsub(".gpkg", "", all_samples$file_name))

  repsum <- repsum |>
    dplyr::group_by(repsum$file_name, repsum$bgc) |>
    dplyr::mutate(tcost = sum(repsum$cost)) |>
    dplyr::select(-repsum$cost) |>
    dplyr::distinct()

  # plot the total costs by subzone
  ggplot2::ggplot(repsum, ggplot2::aes(y = repsum$tcost, x = repsum$file_name)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~repsum$bgc, scales = "free_x") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

}
