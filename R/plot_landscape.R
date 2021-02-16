#' plot_landscape
#'
#' Plot the landscape
#'
#' @param x Raster* object
#'
#' This function is inspired by the show_landscape function within
#' landscapetools pkg (\url{https://github.com/ropensci/landscapetools}).
#'
#' @return ggplot2 Object
#'
#' @export
plot_landscape <- function(x,
                                ...){
  # derive ratio for plot, cells should be a square and axis equal in length
  if (raster::ncol(x) == raster::nrow(x)) {
    ratio <- 1
  } else {
    ratio <- raster::nrow(x) / raster::ncol(x)
  }

  xyz <- raster::as.data.frame(x, xy=TRUE)

  # %>%
  #   rename_at(vars(contains("layer_")),
  #             list(~sub("layer_", "", .))) %>%
  #   replace_na(list(lu = "Other"))


  ggplot2::ggplot(xyz) +
    ggplot2::geom_raster(ggplot2::aes(x, y, fill = as.factor(xyz[,3]))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = element_rect(color = "black"),
      aspect.ratio = ratio,
      legend.position = "bottom",
      panel.border = ggplot2::element_rect(fill = NA,
                                           colour = "black", size = 1),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0))

}