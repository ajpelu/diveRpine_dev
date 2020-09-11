#' plot_landscape_pine
#'
#' Plot the landscape with pine plantation
#'
#' @param x Raster* object
#' @param landscape_colors a vector with the colors to plot the landscape.
#' @param legend_title title of the legend
#'
#' This function is inspired by the show_landscape function within
#' landscapetools pkg (\url{https://github.com/ropensci/landscapetools}).
#'
#' @return ggplot2 Object
#'
#' @export
plot_landscape_pine <- function(x,
                            landscape_colors,
                            legend_title,
                            ...){
  # derive ratio for plot, cells should be a square and axis equal in length
  if (raster::ncol(x) == raster::nrow(x)) {
    ratio <- 1
  } else {
    ratio <- raster::nrow(x) / raster::ncol(x)
  }

  xyz <- raster::as.data.frame(x, xy=TRUE) %>%
    rename_at(vars(contains("layer_")),
              list(~sub("layer_", "", .))) %>%
    replace_na(list(lu = "Other"))


  ggplot2::ggplot(xyz) +
    ggplot2::geom_raster(ggplot2::aes(x, y, fill = lu)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      aspect.ratio = ratio,
      legend.position = "bottom",
      panel.border = ggplot2::element_rect(fill = NA,
                                           colour = "black", size = 1),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(
      values = landscape_colors,
      name = legend_title
    )
}
