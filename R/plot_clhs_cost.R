#' Plot the cost of clhs sample plans
#'
#' @param clhs_dir A path or directory where the multiple clhs sample plans are saved.
#'  Default location is based on standard workflow.
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
#' \dontrun{
#'check_clhs_cost(PEMprepr::read_fid()$dir_20102010_clhs$path_abs)
#' }

plot_clhs_cost <- function(
    clhs_dir = PEMprepr::read_fid()$dir_20102010_clhs$path_abs) {
  ftemp <- fs::dir_ls(clhs_dir, regexp = ".gpkg$")

  all_samples <- do.call(rbind, lapply(ftemp, function(ff) {

    layers <- sf::st_layers(ff)$name

    do.call(rbind, lapply(layers, function(ll) {
      sptemp <- sf::st_read(ff, layer = ll, quiet = TRUE)
      sptemp$clhs_repeat <- basename(ff)
      sptemp$bgc <- sub("_.*", "", ll)
      sptemp
    }))
  }))

  # summarise the costs per sample plan
  repsum <- all_samples
  repsum$file_name <- gsub("_clhs_sample", "", repsum$clhs_repeat)
  repsum$file_name <- gsub(".gpkg", "", repsum$file_name)

  repsum <- stats::aggregate(cost ~ file_name + bgc, data = repsum, sum)
  names(repsum)[names(repsum) == "cost"] <- "tcost"

  # plot the total costs by subzone
  p1 <- ggplot2::ggplot(repsum, ggplot2::aes(y = repsum$tcost, x = repsum$file_name)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ repsum$bgc, scales = "free_y") +
    ggplot2::labs(x = "Sampleplan", y = "Total Cost") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

  print(p1)
}
