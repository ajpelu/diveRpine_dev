
# Load pkgs
library(raster)
library(landscapeR)
library(landscapemetrics)
library(landscapetools)


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


## -------------------------------------------------
### Position for target_plantation
position_pine <- matrix(c(nrow(empty_landscape) / 2,
                          ncol(empty_landscape) / 2),
                        ncol = 2, nrow = 1)

## ------------------------------------------------
### Potential number of crops patches
n_crops <- sample(3:8, size = 1)
