library(tidyverse)
library(sf)
library(raster)
library(lubridate)

setwd('~/mortalityblob')

sp <- read_sf('mortnew/admin_areas', 'admin_areas')

r <- raster('mortnew/masks/chirps-v2.0.1981.01.01.tif') %>%
  rasterToPoints %>%
  data.frame %>%
  st_as_sf(coords=c('x', 'y'), crs=st_crs(sp))

int <- st_intersection(sp, r)

saveRDS(int, '~/mortalityblob/mortnew/chirps_int.RDS')

no_direct <- sp[!sp$uuid %in% int$uuid, ]

d <- st_nearest_feature(no_direct, r)
saveRDS(d, '~/mortalityblob/mortnew/geo_pts.RDS')

no_direct[ , c('chirps_x', 'chirps_y')] <- st_coordinates(r)[d, ]
int[ , c('chirps_x', 'chirps_y')] <- st_coordinates(int)

comb <- bind_rows(no_direct %>%
                    st_drop_geometry,
                  int %>%
                    st_drop_geometry %>%
                    dplyr::select(-`X1901.01.16`))

write.csv(comb, '~/mortalityblob/mortnew/uuids_chirps_matching.csv', row.names=F)

system('telegram "done"')


