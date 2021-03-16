theme_diverpine <- function(){
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(color = "black"),
      legend.position = "bottom",
      panel.border = ggplot2::element_rect(fill = NA,
                                           colour = "black", size = 1),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")
    )
}
