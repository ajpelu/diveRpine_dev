#' Seed input by dispersers
#'
#' Compute the propagule input from each patch to focal pine plantation using
#' three classes of dispersers and different equations.
#'
#' @details
#' It computes the propagule input from each patch to focal pine plantation
#' using three classes of dispersers and different equations.
#' \itemize{The quantity and quality of seed dispersion are influenced by:
#'   \item Seed sources: seed diversity in seed source patch, and patch size
#'   \item Disperser: percentage of each disperser
#'   \item Landscape configuration
#'}
#' \itemize{Three classes of dispersers were considered:
#'   \item small birds, \emph{e.g.} European robin (\emph{Erithacus rubecula}),
#'   Sardinian warbler (\emph{Sylvia melanocephala})
#'   \item medium birds, \emph{e.g.} Eurasian jay (\emph{Garrulus glandarius})
#'   \item mammals, \emph{e.g.} Red fox (\emph{Vulpes vulpes})
#'}
#'
#' \itemize{For each type of disperser, different dispersion kernels have been
#' considered.
#'   \item Small-sized birds rarely exceed 100 m in distance, and approximately
#'   50\% of the seeds are dispersed in the first 50 m.
#'   \item Medium-sized birds disperse 50\% of the seeds over a distance of more
#'   than 100 m. The Eurasyan jay shows a dispersion range between 5 and
#'   1000 m for Sierra Nevada mountains (SE Spain). The distance at which
#'   the maximum dispersion occurs depends on the target patch, being
#'   approximately 400 me when the target patch is a pine plantation.
#'   \item Mammals disperse in a range from 0 to more than 1500 m, with
#'   the dispersion peak at 650 - 700 m. More than 50\% of the seeds
#'   dispersed by mammals are deposited at distances greater than 495 m.
#' }
#'
#' @param x A \code{raster} object with the landscape configured
#'
#' @param xr A \code{raster} object with richness computed
#'
#' @param nf_value The value of "Natural Forests" class within the raster
#' (default value = 2)
#'
#' @param pp_value The value of "pine plantation" class within the raster
#' (default value = 1)
#'
#' @references
#' \insertRef{Gomez2003}{diveRpine}
#'
#' \insertRef{GonzalezVaro2013}{diveRpine}
#'
#' \insertRef{Jordano2007}{diveRpine}
#'
#' \insertRef{Matias2010}{diveRpine}
#'
#' \insertRef{Pons2007}{diveRpine}
#'
#' \insertRef{Zamora2010}{diveRpine}
#'
#' @import raster
#' @import rgeos
#' @import sp
#' @importFrom stats dlnorm dweibull
#' @importFrom methods as
#' @importFrom Rdpack reprompt
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})

disper <- function(x, xr, nf_value, pp_value, rich_nf, rich_pp) {

  # Output stacks
  nf_singles <- stack()
  sb <- stack()
  mb <- stack()
  ma <- stack()
  sbpot <- stack()
  mbpot <- stack()
  mapot <- stack()

  # Rasterize pine plantations
  aux_pine <- raster::calc(x, fun = function(x) ifelse(x == pp_value, pp_value, 0))

  # Get polygons of pine plantation and length
  aux_shape_pine <- rasterToPolygons(aux_pine, fun=function(x){x == pp_value}, dissolve = TRUE)
  perimeter_pine <- rgeos::gLength(aux_shape_pine)

  # Contribution from each Natural Forest patch
  ## Get boundary limits of NF, and save as shapefile
  nf_edges <- raster::rasterToPolygons(x, fun=function(x){x == nf_value}, dissolve = TRUE)

  ## Dissagregate nf polygons
  nf_pol <- disaggregate(nf_edges)

  # Operations for each polygon
  for (i in 1:length(nf_pol)) {
    # Distance between el NF polygon i and all cells of the raster
    d  = gDistance(nf_pol[i,], methods::as(x,"SpatialPoints"), byid=TRUE)
    dnf_i <- x
    dnf_i[] = apply(d,1,min) # Minimun distance
    names(dnf_i) <- paste0('nf',i) # Add name of layer (nfi, i is the number)

    dnf_i10 <- calc(dnf_i, fun = function(x){x*10}) # Multiply to 10 meters
    names(dnf_i10) <- paste0('nf',i,'_meters')

    # --- Richess values for each nf i ---
    rich_nf_i <- mask(xr, nf_pol[i,])
    rpot_i <- cellStats(rich_nf_i, mean)

    # --- Adjacency module ----
    ## Rasterizar nf i
    aux_nfi <- rasterize(nf_pol[i,], x)
    aux_nfi[aux_nfi == 1] <- nf_value
    aux_nfi[is.na(aux_nfi[])] <- 0

    # Merge nf i and aux_pine
    aux <- calc(stack(aux_nfi, aux_pine), fun = function(x){x[1]+x[2]})
    aux[aux == 0] <- NA

    # Get polygons of pine plantation and nf i
    aux_shape <- rasterToPolygons(aux, dissolve = TRUE)

    intersectan <- rgeos::gIntersects(aux_shape[1,], aux_shape[2,])
    if (intersectan == TRUE){
      # Calcula la intersection
      inter <- rgeos::gIntersection(aux_shape[1,], aux_shape[2,], byid = FALSE)
      # esto es por el problema de las classes de rgeos
      if (class(inter)[1] == "SpatialLines") {
        # compute length inter
        length_inter <- sp::SpatialLinesLengths(inter)
      } else {
        interL <- methods::as(inter@lineobj, "SpatialLines")
        length_inter <- sum(sp::SpatialLinesLengths(interL))
      }
    } else {
      length_inter <- 0
    }

    # Compute the adjacency of each nf i
    adj <- ( length_inter / perimeter_pine)*100
    # a = 0.7327; b = -0.0039
    a <- 0.7330
    b <- -0.0039

    # Compute the seed limit (only for )
    seedlimit <- a + b*adj
    seedentry <- 1 - seedlimit
    # Standardize seedentry from 0 to 1
    # 0 % adj --> 1 - (a + b*0%) = 1 - a; 0.267
    # 100 % adj --> 1 - (a + b*100); 0.657
    # std --> seedentry - 0% / (100% - 0%)
    adjF <- (seedentry - 0.267) / (0.657 - 0.267)

    # --- Dispersion contribution
    ## Small bird dispersion
    sb_i <- calc(dnf_i10, fun = function(x){dlnorm(x, meanlog = log(51), sdlog = .7)})
    names(sb_i) <- paste0('sb',i)
    sb_i_pot <- sb_i * (0.5 * rpot_i) * (1+adjF) # We assumed that it takes half the seeds (improve)
    names(sb_i_pot) <- paste0('sb',i, 'pot')

    ## Medium bird dispersion
    mb_i <- calc(dnf_i10, fun = function(x){dlnorm(x, meanlog = log(201), sdlog = .7)})
    names(mb_i) <- paste0('mb',i)
    mb_i_pot <- mb_i * (0.5 * rpot_i) * (1+adjF) # We assumed that it takes half the seeds (improve)
    names(mb_i_pot) <- paste0('mb',i, 'pot')

    ## Mammal dispersion
    ma_i <- calc(dnf_i10, fun = function(x){
      ifelse(x <= 400, dweibull(x, shape = 1.385, scale = 137),
             dlnorm(x, meanlog = 6.621, sdlog = 0.297))})
    names(ma_i) <- paste0('ma',i)
    ma_i_pot <- ma_i * ((0.5 * rpot_i) + 1)  # We assumed that it takes half the seeds (improve). We also add seeds from cropland (maximum 3)
    names(ma_i_pot) <- paste0('ma',i, 'pot')

    rasters_i <- stack(dnf_i10,
                       sb_i, mb_i, ma_i,
                       sb_i_pot, mb_i_pot, ma_i_pot)

    nf_singles <- stack(nf_singles, rasters_i)

    sb <- stack(sb, sb_i)
    mb <- stack(mb, mb_i)
    ma <- stack(ma, ma_i)

    sbpot <- stack(sbpot, sb_i_pot)
    mbpot <- stack(mbpot, mb_i_pot)
    mapot <- stack(mapot, ma_i_pot)
  }

  # Compile an unique raster by disperser vector (sum all single raster )
  r_sbpot <- sum(sbpot)
  names(r_sbpot) <- 'r_sbpot'
  r_mbpot <- sum(mbpot)
  names(r_mbpot) <- 'r_mbpot'
  r_mapot <- sum(mapot)
  names(r_mapot) <- 'r_mapot'


  # Mask by pine plantantion
  msb <- mask(r_sbpot, aux_shape_pine)
  names(msb) <- 'msb' # mask small bird
  mmb <- mask(r_mbpot, aux_shape_pine)
  names(mmb) <- 'mmb' # mask medium bird
  mma <- mask(r_mapot, aux_shape_pine)
  names(mma) <- 'mma' # mask mammal

  out <- stack(nf_singles,
               r_sbpot, r_mbpot, r_mapot,
               msb, mmb, mma)

  return(out)
}
