#' Seed input propagules into pine plantation
#'
#' Compute the propagule input into focal pine plantation.

input_propagule <- function(x, pd, pp_value){
  #'
  #' @param x A \code{raster} object with the landscape configured
  #'
  #' @param pd A \code{raster stack} object with raster of the potential
  #' dispersion. See \code{potential_dispersion} function.
  #'
  #' @param pp_value The value of "pine plantation" class within the raster
  #' (default value = 1)

  pp <- rasterToPolygons(x, fun=function(x){x == pp_value}, dissolve = TRUE)

  propagules_pp <- raster::mask(pd, pp)
  return(propagules_pp)
}
