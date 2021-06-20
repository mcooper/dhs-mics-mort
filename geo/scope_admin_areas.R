# Best with sf 1.0 for spherical area calculations
# https://r-spatial.org/r/2020/06/17/s2.html

library(sf)
library(tidyverse)
library(countrycode)

setwd('~/mortalityblob/mortnew/')

# DHS
d <- read.csv('dhs-geo-manualmatch.csv') %>%
  select(gadm_code, geo_code)

#MICS 
m <- read.csv('mics-geo-manualmatch.csv') %>%
  select(geo_code, gadm_code)

#Combine
a <- bind_rows(d, m)

#Get unique GADM polygons
uu <- a %>%
  select(gadm_code) %>%
  unique %>%
  mutate(uuid=paste0('uu', substr(10000 + row_number(), 2, 5)))

#Add uuids back to full dataset
a <- merge(a, uu)

write.csv(a, 'geo_matching.csv', row.names=F)

###############################
# Get master df of all polygons
###############################
g <- unique(str_split(paste0(a$gadm_code, collapse=' '), ' ')[[1]])

fs <- list.files('admin_matching/GADM', pattern='shp$')

all_sp <- list()
for (f in fs){
  print(f)
  n <- substr(f, 12, 12)
  t <- read_sf('admin_matching/GADM', gsub('.shp', '', f)) %>%
    select(GID=paste0('GID_', n)) %>%
    filter(GID %in% g)
  if (nrow(t) > 0){
    all_sp[[length(all_sp) + 1]] <- t
  }
}

all <- bind_rows(all_sp)
write_sf(all, 'admin_matching/all_sp', driver='ESRI Shapefile')

###############################
# Make individual polygons for each uuid
###############################

new_sp <- list()
for (i in 1:nrow(uu)){
  print(i)
  codes <- str_split(uu$gadm_code[i], ' ')[[1]]
  codes <- codes[codes != '']
  s <- all[all$GID %in% codes, 'geometry']
  if (nrow(s) > 1){
    s <- st_union(s)
    s <- st_as_sf(data.frame(s))
  }
  #get area
  s <- s %>%
    mutate(uuid = uu$uuid[i])

  s$area <- st_geod_area(s)/1e6

  new_sp[[length(new_sp) + 1]] <- s
}

new <- bind_rows(new_sp)
write_sf(new, 'admin_areas', driver='ESRI Shapefile')

#####################################
# Make survey-specific maps for QAQC
#####################################

a$survey <- gsub('\\_.*', '', a$geo_code)

for (surv in unique(a$survey)){
  sel <- a %>%
    filter(survey == surv)

  plt <- new %>%
    filter(uuid %in% sel$uuid)

  cty <- read_sf('admin_matching/GADM', paste0('gadm36_', substr(sel$gadm_code[1], 1, 3), '_0'))

  ggplot() + 
    geom_sf(data=plt, aes(fill=uuid), alpha=0.5) + 
    geom_sf(data=cty, color='black', fill=NA) + 
    guides(fill=FALSE) + 
    theme_void()
  ggsave(paste0('admin_matching/cty_admin/', surv, '.png'))
}

