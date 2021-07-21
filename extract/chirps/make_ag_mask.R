library(tidyverse)
library(data.table)
library(raster)
library(sf)

setwd('~/mortalityblob/mortnew/masks/')

crop <- raster('cropland.tif')
past <- raster('pasture.tif')

ag <- crop + past

aa <- read_sf('../admin_areas', 'admin_areas')

########################################################
# Vizualize - make sure all areas have some ag
########################################################
e <- raster::extract(crop > 0 | past > 0, as(aa, "Spatial")) 

es <- lapply(e, function(x){sum(x, na.rm=T)})

aa$ag_ct <- unlist(es)

ne <- rnaturalearth::ne_countries(returnclass='sf')

ggplot() +
  geom_sf(data=ne) + 
  geom_sf(data=aa %>% filter(ag_ct == 0), color='red', fill='red') + 
  theme_void()
sum(aa$ag_ct == 0, na.rm=T)

#Look like almost all do, except for some islands and coastal polygons

##############################################
# Resample to CHIRPS
##############################################

chirps <- raster('chirps-v2.0.1981.01.01.tif')

res <- raster::resample(ag, chirps, method='bilinear')
writeRaster(res, 'agland_chirps_scale.tif')
