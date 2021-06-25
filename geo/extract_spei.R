library(tidyverse)
library(sf)
library(raster)
library(lubridate)

setwd('~/mortalityblob')

sp <- read_sf('mortnew/admin_areas', 'admin_areas')

r <- stack('spei_product//spei01.nc')[[1]] %>%
  rasterToPoints %>%
  data.frame %>%
  st_as_sf(coords=c('x', 'y'), crs=st_crs(sp))

int <- st_intersection(sp, r)

saveRDS(int, '~/mortalityblob/mortnew/geo_int.RDS')

no_direct <- sp[!sp$uuid %in% int$uuid, ]

d <- st_nearest_feature(no_direct, r)
saveRDS(d, '~/mortalityblob/mortnew/geo_pts.RDS')

no_direct[ , c('spei_x', 'spei_y')] <- st_coordinates(r)[d, ]
int[ , c('spei_x', 'spei_y')] <- st_coordinates(int)

comb <- bind_rows(no_direct %>%
                    st_drop_geometry,
                  int %>%
                    st_drop_geometry %>%
                    dplyr::select(-`X1901.01.16`))

write.csv(comb, '~/mortalityblob/mortnew/uuids_spei_matching.csv', row.names=F)

readNCtoPts <- function(x){
  r <- stack(paste0('spei_product//', x, '.nc'))
  ind <- year(ymd(substr(names(r), 2, 10))) > 1980
  r <- r[[names(r)[ind]]]

  d <- rasterToPoints(r) %>%
    data.frame %>%
    rename(spei_x=x, spei_y=y)

  d <- d %>%
    gather(date, spei, -spei_x, -spei_y)

  names(d)[names(d) == 'spei'] <- x

  d$ymd <- ymd(substr(d$date, 2, 10))
  d$cmc <- 12*(year(d$ymd) - 1900) + month(d$ymd)

  d$date <- NULL
  d$ymd <- NULL

  return(d)
}

l <- list()
speis <- c('spei01', 'spei03', 'spei06', 'spei12', 'spei24', 'spei36')
for (i in 1:length(speis)){
  print(speis[i])
  l[[i]] <- readNCtoPts(speis[i])
}
m <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)}, l)

library(data.table)
fwrite(m, 'mortnew/spei_prod_dat.csv', row.names=F)

system('telegram "Donezo!"')




