#' create_pine
#'
#' Create a pine plantation stand
#'
#' @param x Raster* object
#' @param pine_size area (virtual dimensions) of the pine
#' plantation stand. It is defined using the slider "patch Area"
#' within the module pine_plantation of diveRpine.
#' @param pine_density tree density of the pine plantation stand. One of
#' \code{low}, \code{medium}, or \code{high}. These categories were defined
#' according to the data from Sierra Nevada (Spain): < 300 trees/ha (low);
#' 300 - 1500 trees/ha (medium); and > 1500 trees/ha (high). The choice of
#' stand density is made by the user in the module pine_plantation of diveRpine.
#' Default value: \code{medium}
#' @param pine_pastUse is the past land-use of the pine plantation before it
#' was established. User can choose from: "Natural Forests", "Shrublands",
#' "Pasture", and "Croplands". Default value: \code{Shrublands}. The choice of
#' past land-use is made by the user in the module pine_plantation of diveRpine.
#'
#' @return raster Object
#'
#' @export


is_raster <- function(x){
  inherits(x, "RasterBrick") || inherits(x, "RasterStack") || inherits(x, "RasterLayer")
}

create_pine <- function(x,
                        pine_size,
                        pine_density,
                        pine_pastUse) {

  require("tidyverse")
  require("landscapeR")
  require("raster")

  if (!is_raster(x)) stop("x must be a raster object")
  if (missing(pine_density)) { pine_density <- "medium"}
  if (missing(pine_pastUse)) { pine_pastUse <- "Shrublands"}


  p_pine <- landscapeR::makePatch(x,
                                  val = 1, rast = TRUE, bgr = 0,
                                  size = pine_size,
                                  spt = matrix(
                                    c(nrow(x) / 2, ncol(x) / 2),
                                    nrow = 1
                                  )
  )

  p_pine <- as.factor(p_pine)
  a <- levels(p_pine)[[1]] %>%
    mutate(lu = c(NA, "Pine"),
           pine_density = c(NA, pine_density),
           pine_pastUse = c(NA, pine_pastUse))

  levels(p_pine) <- a

  return(p_pine)
}


