library(sp)
library(data.table)
library(tidyverse)

setwd('~/mortalityblob/mortnew')

dhs <- fread('dhs.csv')
mics <- fread('mics.csv')

comb <- rbind(dhs, mics)

rm(dhs, mics)

comb$survey <- gsub('_.*', '', comb$geo_code)

#Though I fixed nepal but maybe not?
#Also Ethiopia???
comb$date_cmc[comb$survey %in% c('NP-4-1', 'NP-5-1', 'NP-6-1', 'NP-7-2')] <- 
comb$date_cmc[comb$survey %in% c('NP-4-1', 'NP-5-1', 'NP-6-1', 'NP-7-2')] - 681
comb$date_cmc[comb$survey %in% c('NP-3-1')] <- comb$date_cmc[comb$survey %in% c('NP-3-1')] + 519
comb$date_cmc[substr(comb$survey, 1, 2) == 'ET'] <- comb$date_cm[substr(comb$survey, 1, 2) == 'ET'] + 92

geo_cmc <- comb %>%
  group_by(geo_code) %>%
  summarize(max_date = max(date_cmc),
            min_date = min(date_cmc) - 11)

rm(comb)

geom <- fread('geo_matching.csv') %>%
  select(geo_code, uuid)

geo_cmc_uuids <- merge(geo_cmc, geom, all=T)

uuids_cmc <- geo_cmc_uuids %>%
  group_by(uuid) %>%
  summarize(max_date = max(max_date),
            min_date = min(min_date))

write.csv(uuids_cmc, 'princeton_polynomials/uuid_dates.csv', row.names=F)

