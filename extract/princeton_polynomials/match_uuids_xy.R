library(sf)
library(raster)
library(tidyverse)

setwd('~/mortalityblob/mortnew/')

sp <- read_sf('admin_areas', 'admin_areas')
sp$nope <- 0

pop <- read.csv('princeton_polynomials/pop.csv')
pop$x2 <- pop$x
pop$x2[pop$x > 180] <- pop$x[pop$x > 180] - 360

popr <- rasterFromXYZ(pop[ , c('x2', 'y', 'pop')])
popr[is.na(popr)] <- 0

all <- data.frame()
for (s in 1:nrow(sp)){
  print(s/nrow(sp))
  r <- data.frame(rasterToPoints(rasterize(sp[s, ], popr)))
  if (nrow(r) == 0){
    r <- data.frame(rasterToPoints(rasterize(st_centroid(sp[s, ]), field='nope', popr)))
  }
  r$layer <- NULL
  r$uuid <- sp$uuid[s]
  all <- bind_rows(all, r)
}

write.csv(all, 'princeton_polynomials/uuids_matching.csv', row.names=F)
