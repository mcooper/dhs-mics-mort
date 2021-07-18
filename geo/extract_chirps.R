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
m <- data.table(rasterToPoints(r))
n <- names(m)[3:ncol(m)]
names(m)[3:ncol(m)] <- paste0(substr(n, 2, 5), '-', substr(n, 7, 8))
m$x <- round(m$x, 3)
m$y <- round(m$y, 3)

m2 <- melt(m, id.vars=c('x', 'y'))
names(m2) <- c('x', 'y', 'date', 'precip')
fwrite(m2, 'mortnew/chirps/chirps.xyz')
