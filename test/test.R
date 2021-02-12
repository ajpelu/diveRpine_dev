source("./R/init_params.R")
source("./R/diveR_landscape.R")

## -------------------------------------------------
### Create empty landscape
set.seed(123)

# size
ancho <- 63 * 2 # n cols
alto <- 53 * 2 # n rows

m <- matrix(nrow=alto, ncol=ancho, byrow = T)
empty_landscape <- raster(m)
# Define extent of raster
extent(empty_landscape) <- matrix(c(0, 0, ancho, alto), nrow=2)
empty_landscape[] <- 0
## -------------------------------------------------



##### 1) Create pine patch
source("./R/create_pine.R")
source("./R/create_pine2.R")

pp <- create_pine(empty_landscape, pine_size = 1000)

# test error
# raster?
create_pine(m, pine_size = 1000)

pp <- create_pine(empty_landscape, pine_size = 1000,
            pine_pastUse = "Crops")



pp <- create_pine(empty_landscape, pine_size = 1000)





##### 2) Plot landscape pine with the selection from UI

landscape_colors <- c("0" = "white",
                      "1" = "green")
# "Pine" = den_pp()$col)
source("./R/plot_landscape_pine.R")

plot_landscape_pine(pp) +
  scale_fill_manual(
    breaks = c("0", "1"),
    values = c("white", "green"),
    labels = c("Other", "Pine"),
    name = "Present land uses"
  )



















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
    panel.border = ggplot2::element_rect(fill = NA, colour = "black", size = 1),
    plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")
  ) +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::scale_fill_manual(
    values = landscape_colors,
    name = legend_title)




    labels = if (is.null(legend_labels)) {c
      ggplot2::waiver()
    } else {
      legend_labels
    },
    name = legend_title
  )









landscape_colors <- c("NA" = "white",
                      # "1" = den_pp()$col)
                      1 = "darkgreen")
#"2" = "green",
#"3" = "lightgoldenrod1")

landscape_labels <- c("Others", "Pine plantation")

diveR_landscape(pp,
                landscape_categories = landscape_colors,
                legend_labels = landscape_labels,
                legend_title = "Land uses categories")

diveR_landscape(pp,
                )

+
  scale_fill_manual(breaks = c(0,1),
                    values = c("white", "green"), name = "Land Uses")


# convertir el raster en factor
pp <- as.factor(pp)
# ver los niveles que tiene
x <- levels(pp)[[1]]
x$lu <- c(NA, "Pine")
x$past_lu <- c(NA, "NF")


x <- levels(pp)[[1]] %>%
  mutate(lu = c(NA, "Pine"),
         density_pp = c(NA, "medium"),
         past_lu = c(NA, "Shrubland"))
levels(pp) <- x



landscapetools::show_landscape(empty_landscape)


landscapetools::show_landscape(pp, discrete=TRUE) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = c("gray", "green")) +


pp




xyz <- raster::as.data.frame(pp, xy = TRUE)



plot_landscape()








                               # xlab="", ylab="",
                              # axis = FALSE)


##### 2) Create Natural forest patchs

# npatch <- n_nf ## Viene del número de n_forest que pongamos
# size <- size_nf ## Vector que vendrá de la
# pts <- locate_nf  # a matrix

n_nf <- 5
size_nf <- c(200, 300, 200, 200, 200)
locate_nf <- matrix(
  nrow = n_nf, ncol = 2,
  data = c(10, 80, 70, 100, 60, 40, 10, 90, 100, 100),
  byrow = TRUE
)

nf <- makeClass(context = pp,
                npatch = n_nf,
                size = size_nf,
                pts = locate_nf,
                val = 2, rast = TRUE
)

show_landscape(nf)

# https://stackoverflow.com/questions/37523323/identify-position-of-a-click-on-a-raster-in-leaflet-in-r












create_nf <- function(r, size_pine) {
  if (!is_raster(r)) stop("r must be a raster object")

  p_ <- landscapeR::makePatch(r,
    val = 1, rast = TRUE, bgr = 0,
    size = size_pine,
    spt = matrix(
      c(nrow(r) / 2, ncol(r) / 2),
      nrow = 1
    )
  )

  return(p_pine)
}




size = c(100, 200)
size = 100
npatch = 2 # number of nforest (1 to 5)
bgr = 0


# Transformo el raster a matrix
mtx <- t(raster::as.matrix(pp))

# Si solo especifico un tamñano y mas de un parche, repito el tamaño para cada parche
if (length(size) == 1) {
  size <- rep(size, npatch)}



bgrCells <- which(is.element(mtx, bgr))
if (length(bgrCells) == 0) {
  stop("No background cells available with value ", bgr,
       ". Try checking argument \"bgr\".")
}







nf <- makeClass(pp,
                val = 2, rast = TRUE,
                npatch = n_nf,
                size = size_nf
)








# Computar el espacio disponible
r1 <- empty_landscape















