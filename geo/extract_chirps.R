library(data.table)
library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(rgdal)

setwd('~/mortalityblob')

sp <- readOGR('mortnew/admin_areas', 'admin_areas')

r <- raster('mortnew/masks/chirps-v2.0.1981.01.01.tif') %>%
  rasterToPoints %>%
  data.frame %>%
  st_as_sf(coords=c('x', 'y'), crs=st_crs(sp))

df <- data.frame()
for (i in 1:nrow(sp)){
  print(i/nrow(sp))
  rst <- data.frame(rasterToPoints(rasterize(sp[i, ], r)))
  df <- bind_rows(df, rst %>% mutate(uuid=sp$uuid[i]))
}

fwrite(df, '~/mortalityblob/mortnew/chirps/admin_areas_matching.csv', row.names=F)


system('telegram "done with everything"')

system('sudo poweroff')

library(raster)

r <- brick('~/mortalityblob/chirps/chirps-v2.0.monthly.nc')
m <- rasterToPoints(r)

saveRDS(m, '~/mortalityblob/mortnew/chirps/rastToPts.RDS')

m <- readRDS('~/mortalityblob/mortnew/chirps/rastToPts.RDS')

r <- brick('~/mortalityblob/chirps/chirps-v2.0.monthly.nc')
r <- brick('~/mortalityblob/chirps/chirps-v2.0.monthly.nc')
m <- rasterToPoints(r)
saveRDS(m, '~/mortalityblob/mortnew/chirps/rastToPts.RDS')

# Looks like no NAs?
