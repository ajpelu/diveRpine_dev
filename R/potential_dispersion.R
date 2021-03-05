potential_dispersion <- function(x, nf_value, pp_value, rich_nf) {

  # Output stacks
  sb <- stack()
  mb <- stack()
  ma <- stack()
  sbpot <- stack()
  mbpot <- stack()
  mapot <- stack()

  # Get perimeter of the pine plantations
  pine <- raster::rasterToPolygons(x, fun=function(x){x == pp_value}, dissolve = TRUE)
  perimeter_pine <- rgeos::gLength(pine)

  # Get polygons of Natural forests
  nf_edges <- raster::rasterToPolygons(x, fun=function(x){x == nf_value}, dissolve = TRUE)
  nf_patches <- sp::disaggregate(nf_edges)

  # Operations for each polygon
  for (i in 1:length(nf_patches)) {
    # --- distance between nf polygon i and cells of the raster
    d <- rgeos::gDistance(nf_patches[i,], methods::as(x,"SpatialPoints"), byid=TRUE)
    d_nfi <- x
    d_nfi[] <- apply(d, 1, min)*10 # compute minimun distance; and multiply by 10 meters
    names(d_nfi) <- paste0("nf", i)

    # --- Adjacency
    intersectan <- st_intersects(sf::st_as_sf(nf_patches[i,]),
                                 st_as_sf(pine), sparse = FALSE)
    if(intersectan == TRUE){
    length_inter  <- st_length(
      st_intersection(sf::st_as_sf(nf_patches[i,]),
                      st_as_sf(pine), sparse = FALSE))
    } else {
      length_inter <- 0
    }

    ## Generate adj factor for nf i
    adj <- (length_inter / perimeter_pine)*100
    ## Compute the seed entry (1 - seed limit) (only for nf with intersection)
    # (seedlimit=a + b*adj) see Zamora et al.2010  a=0.7327; b = -0.0039
    seedentry <- (1 - (0.7330 - (0.0039*adj)))
    # Standardize seedentry from 0 to 1
    # 0 % adj --> 1 - (a + b*0%) = 1 - a; 0.267
    # 100 % adj --> 1 - (a + b*100); 0.657
    # std --> seedentry - 0% / (100% - 0%)
    adjF <- (seedentry - 0.267) / (0.657 - 0.267)
    fc <- 1+adjF

    # --- Richness stats (mean, se, sd ) for each nf patch
    rich_nfi <- raster::mask(rich_nf, nf_patches[i,])
    rich.mean <- raster::cellStats(rich_nfi, stat = "mean", na.rm=TRUE)
    rich.sd <- raster::cellStats(rich_nfi, stat = "sd", na.rm=TRUE)
    rich.se <- rich.sd/(sqrt(length(rich_nfi) - freq(rich_nfi, value=NA)))

    # --- Dispersion contribution
    ## Small bird
    sbi <- raster::calc(d_nfi, fun = function(x){dlnorm(x, meanlog = log(51), sdlog = .7)})
    names(sbi) <- paste0('sb',i)
    ### potential contribution
    sbipot <- stack(sbi*rich.mean*fc, sbi*rich.sd*fc, sbi*rich.se*fc)
    names(sbipot) <- c(paste0("sbpot_", i, "_mean"), paste0("sbpot_", i, "_sd"), paste0("sbpot_", i, "_se"))

    ## Medium bird
    mbi <- raster::calc(d_nfi, fun = function(x){dlnorm(x, meanlog = log(201), sdlog = .7)})
    names(mbi) <- paste0('mb',i)
    ### potential contribution
    mbipot <- stack(mbi*rich.mean*fc, mbi*rich.sd*fc, mbi*rich.se*fc)
    names(mbipot) <- c(paste0("mbpot_", i, "_mean"), paste0("mbpot_", i, "_sd"), paste0("mbpot_", i, "_se"))

    ## Mammals (not included adjacency correction)
    mai <- raster::calc(d_nfi, fun = function(x){
      ifelse(x <= 400,
             dweibull(x, shape = 1.385, scale = 137),
             dlnorm(x, meanlog = 6.621, sdlog = 0.297))})
    names(mai) <- paste0('ma',i)
    ### potential contribution
    maipot <- stack(mai*rich.mean, mai*rich.sd, mai*rich.se)
    names(maipot) <- c(paste0("mapot_", i, "_mean"), paste0("mapot_", i, "_sd"), paste0("mapot_", i, "_se"))

    sb <- stack(sb, sbi)
    mb <- stack(mb, mbi)
    ma <- stack(ma, mai)

    sbpot <- stack(sbpot, sbipot)
    mbpot <- stack(mbpot, mbipot)
    mapot <- stack(mapot, maipot)
  }

  # Get values of sb, mb, ma for all nf
  sb_all <- sum(raster::subset(sbpot, grep("^sbpot.+_mean$", names(sbpot), value = T)))
  names(sb_all) <- "sb"
  mb_all <- sum(raster::subset(mbpot, grep("^mbpot.+_mean$", names(mbpot), value = T)))
  names(mb_all) <- "mb"
  ma_all <- sum(raster::subset(mapot, grep("^mapot.+_mean$", names(mapot), value = T)))
  names(ma_all) <- "ma"

  sb_all.sd <- sum(raster::subset(sbpot, grep("^sbpot.+_sd$", names(sbpot), value = T)))
  names(sb_all.sd) <- "sb.sd"
  mb_all.sd <- sum(raster::subset(mbpot, grep("^mbpot.+_sd$", names(mbpot), value = T)))
  names(mb_all.sd) <- "mb.sd"
  ma_all.sd <- sum(raster::subset(mapot, grep("^mapot.+_sd$", names(mapot), value = T)))
  names(ma_all.sd) <- "ma.sd"

  sb_all.se <- sum(raster::subset(sbpot, grep("^sbpot.+_sd$", names(sbpot), value = T)))
  names(sb_all.se) <- "sb.se"
  mb_all.se <- sum(raster::subset(mbpot, grep("^mbpot.+_sd$", names(mbpot), value = T)))
  names(mb_all.se) <- "mb.se"
  ma_all.se <- sum(raster::subset(mapot, grep("^mapot.+_sd$", names(mapot), value = T)))
  names(ma_all.se) <- "ma.se"

  out <- stack(sb, mb, ma,
               sb_all, mb_all, ma_all,
               sb_all.sd, mb_all.sd, ma_all.sd,
               sb_all.se, mb_all.se, ma_all.se)

  return(out)

}