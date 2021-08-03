library(data.table)
library(tidyverse)
library(zoo)

setwd('~/mortalityblob/mortnew')

#################################
# Mortality
#################################

dhs <- fread('dhs.csv')
mics <- fread('mics.csv')

comb <- rbind(dhs, mics)

comb$survey <- gsub('_.*', '', comb$geo_code)

tab(comb$survey, comb$date_cmc > 1458)

#Though I fixed nepal but maybe not?
#Also Ethiopia???
comb$date_cmc[comb$survey %in% c('NP-4-1', 'NP-5-1', 'NP-6-1', 'NP-7-2')] <- 
comb$date_cmc[comb$survey %in% c('NP-4-1', 'NP-5-1', 'NP-6-1', 'NP-7-2')] - 681
comb$date_cmc[comb$survey %in% c('NP-3-1')] <- comb$date_cmc[comb$survey %in% c('NP-3-1')] + 519
comb$date_cmc[substr(comb$survey, 1, 2) == 'ET'] <- comb$date_cm[substr(comb$survey, 1, 2) == 'ET'] + 92


rm(dhs, mics)

#################################
# Climate Data
#################################

geom <- fread('geo_matching.csv') %>%
  select(geo_code, uuid)

spei <- fread('chirps_spei_uuids.csv')

spei$year <- as.numeric(substr(spei$date, 1, 4))
spei$month <- as.numeric(substr(spei$date, 6, 7))
spei$date_cmc <- 12*(spei$year - 1900) + spei$month

spei <- spei %>%
  select(-date, -wb_pop, -wb_ag, -year, -month)

all <- merge(comb, geom)
all <- merge(all, spei, all.x=T, all.y=F, by=c('date_cmc', 'uuid'))
all <- merge(all, uuids, all.x=T, all.y=F, by='uuid')

all <- all %>%
  filter(!is.na(spei01))

fwrite(all, 'all.csv', row.names=F)


