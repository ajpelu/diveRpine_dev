# para testear

# Cargo init params
source("R/init_params.R")
source("R/dist2nf.R")
source("R/plot_landscape.R")
input <- list()

# Select pine size
pp_size <- 1000
pp_den <- "low" # c("low", "medium", "high")
pp_use <- "Natural Forests" # c("Natural Forests", "Shrublands", "Pasture", "Croplands")
input$pp_size <- pp_size


# Create pine
pine <- landscapeR::makePatch(empty_landscape,
                        val = 1, rast = TRUE, bgr = 0,
                        size = input$pp_size,
                        spt = position_pine)

plot(pine)

### Natural forests submodule -----------------------
#### Get the positions for the creation of the NF patches.
nf_n <- 5
input$nf_n <- nf_n

#### Get the positions for the creation of the NF patches.
positions_nf <-
  sample(
    which(t(raster::as.matrix(pine) == 0)), nf_n)


#### Generate the sizes of the natural forests patch
nf_size <- c(100, 200)
input$nf_size <- nf_size

nf_sizes <-
  round(runif(nf_n,
              input$nf_size[1],
              input$nf_size[2]),
        digits = 0)


##   pine_oak
pine_oak <- makeClass(pine,
          val = 2, rast = TRUE,
          npatch = nf_n,
          pts = positions_nf,
          size = nf_sizes
)
plot(pine_oak)


crops_size <-
  sample(10:ceiling(
    length(which(t(raster::as.matrix(pine_oak)) == 0))*0.05),
    size = n_crops)

landscape <-
  makeClass(pine_oak,
            val = 3, rast = TRUE,
            npatch = n_crops,
            size = crops_size)



# pp_denR()$col <-#a1d99b


d <- plot_landscape(landscape) +
  scale_fill_manual(
    values =
      c("0" = "#FFFFe5",
        "1" = "#a1d99b", # pp_denR()$col
        "2" = "green",
        "3" = "lightgoldenrod1"),
    labels = c("Other", "Pine plantation", "Natural Forests", "Crops"),
    name = "Present land uses")



## Compute initial Richnness
rasterRich <- reactive({
  initRichness(r = landscapeInit(), draster = dist_raster(),
               r_range = ri_range, treedensity = den_pp()$den,
               pastUse = pastUse(), rescale = FALSE)
})



# Add factor levels (landscapes types)
l <- ratify(landscape)
rat_l <- levels(l)[[1]]
rat_l$landuseValue <- c(0:3)
rat_l$landuse <- c("Shrublands", "Pine plantation", "Natural Forest", "Crops")
levels(l) <- rat_l

limit_pp <- rasterToPolygons(landscape, fun=function(x){x==pp_value}, dissolve = TRUE)



lll <-fortify(limit_pp, region = "layer") %>%
  rename(x=long, y=lat)


# limit_pp <- reactive({
#   rasterToPolygons(landscape(), fun=function(x){x==pp_value}, dissolve = TRUE)
# })
