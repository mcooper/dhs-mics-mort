library(tidyverse)
library(sf)

setwd('~/mortalityblob/mortnew/')

ipums <- read.csv('ipums-geo.csv')

# Read in and process all ipums shapefiles
fs <- list.files('admin_matching/IPUMS', pattern='.shp$', full.names=T)
fs <- fs[!grepl('br', fs)] #Drop Brazil
sp <- list()
for (i in 1:length(fs)){
  s <- read_sf(fs[i]) %>%
    select(-ADMIN_NAME)
  sp[[i]] <- s
}
sp <- bind_rows(sp)

m <- merge(sp %>% rename(geo_id = GEOLEVEL2, ipums)
