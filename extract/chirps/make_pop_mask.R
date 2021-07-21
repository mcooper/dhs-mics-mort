library(tidyverse)
library(data.table)
library(raster)
library(sf)

setwd('~/mortalityblob/mortnew/masks/')

r <- raster('gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2005_2pt5_min.tif')

##############################################
# Resample to CHIRPS
##############################################

chirps <- raster('chirps-v2.0.1981.01.01.tif')

res <- raster::resample(r, chirps, method='bilinear')
writeRaster(res, 'pop_chirps_scale.tif')

