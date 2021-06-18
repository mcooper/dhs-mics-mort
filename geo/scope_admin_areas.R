library(sf)
library(tidyverse)

setwd('~/mortalityblob/mortnew/')

# DHS
d <- read.csv('dhs-geo-manualmatch.csv') %>%
  select(gadm_code, geo_code) %>%
  mutate(source = 'DHS')

#MICS 
m <- read.csv('mics-geo-manualmatch.csv') %>%
  select(geo_code, gadm_code) %>%
  mutate(source = 'MICS')

a <- bind_rows(d, m)

###############################
# Get master df of all polygons
###############################
g <- unique(a$gadm_code)

fs <- list.files('GADM', pattern='shp$')

all_sp <- list()
for (f in fs){
  print(f)
  n <- substr(f, 12, 12)
  t <- read_sf('GADM', gsub('.shp', '', f)) %>%
    select(GID=paste0('GID_', n)) %>%
    filter(GID %in% g)
  if (nrow(t) > 0){
    all_sp[[length(all_sp) + 1]] <- t
  }
}

all <- bind_rows(all_sp)
write_sf(all, 'all_sp', driver='ESRI Shapefile')

###############################
# Make polygons for each geo_id
###############################

new_sp <- list()
for (i in 1:nrow(a)){
  print(i)
  codes <- str_split(a$gadm_code[i], ' ')[[1]]
  codes <- codes[codes != '']
  s <- all[all$GID %in% codes, ]
  if (nrow(s) > 1){
    s <- st_union(s)
    s <- data.frame(s, gadm_code=a$gadm_code[i])
  }
  new_sp[[length(new_sp) + 1]] <- t
}
new <- bind_rows(new_sp)
