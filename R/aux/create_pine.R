#' create_pine
#'
#' Create a pine plantation stand
#'
#' @param x Raster* object
#' @param pine_size area (virtual dimensions) of the pine
#' plantation stand. It is defined using the slider "patch Area"
#' within the module pine_plantation of diveRpine.
#'
#' @return raster Object
#'
#' @export


is_raster <- function(x){
  inherits(x, "RasterBrick") || inherits(x, "RasterStack") || inherits(x, "RasterLayer")
}

create_pine <- function(x,
                        pine_size) {

  require("tidyverse")
  require("landscapeR")
  require("raster")

  if (!is_raster(x)) stop("x must be a raster object")

  p_pine <- landscapeR::makePatch(x,
                                  val = 1, rast = TRUE, bgr = 0,
                                  size = pine_size,
                                  spt = matrix(
                                    c(nrow(x) / 2, ncol(x) / 2),
                                    ncol = 1
                                  ),
                                  edge = TRUE
  )

  return(p_pine)
}
