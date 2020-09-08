create_pine <- function(r, size_pine) {
  if (!is_raster(r)) stop("r must be a raster object")

  p_pine <- landscapeR::makePatch(r,
                                  val = 1, rast = TRUE, bgr = 0,
                                  size = size_pine,
                                  spt = matrix(
                                    c(nrow(r) / 2, ncol(r) / 2),
                                    nrow = 1
                                  )
  )

  return(p_pine)
}
