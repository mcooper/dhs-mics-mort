library(tidyverse)
library(data.table)
library(raster)
library(sf)

setwd('~/mortalityblob/mortnew/masks/')

r <- raster('gpw_v4_population_count_rev11_2005_2pt5_min.tif')

##############################################
# Resample to CHIRPS
##############################################

ref <- raster('/mnt/prcp_daily_1981-1981.nc')

res <- raster::aggregate(r, fact=6, method='sum', na.rm=T)

ref_pts <- data.frame(rasterToPoints(ref))
res_pts <- data.frame(rasterToPoints(res))

res_pts$x[res_pts$x < 0] <- res_pts$x[res_pts$x < 0] + 360

all(res_pts$y %in% ref_pts$y)
all(res_pts$x %in% ref_pts$x)

names(res_pts)[3] <- 'pop'

ggplot(res_pts) + 
  geom_raster(aes(x=x, y=y, fill=pop)) + 
  theme_void()

fwrite(res_pts, '../princeton_polynomials/pop.csv')
