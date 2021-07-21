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

spei <- fread('spei_prod_dat.csv')
uuid <- fread('uuids_spei_matching.csv')
uuid <- merge(uuid, spei, all.x=T, all.y=F)

uuid <- uuid[ , .(spei01=mean(spei01, na.rm=T), spei03=mean(spei03, na.rm=T), 
                  spei06=mean(spei06, na.rm=T), spei12=mean(spei12, na.rm=T),
                  spei24=mean(spei24, na.rm=T), spei36=mean(spei36, na.rm=T)), 
                .(uuid, area, cmc)]

uuid <- uuid %>%
  rename(date_cmc=cmc)

all <- merge(comb, geom)
all <- merge(all, uuid, all.x=T, all.y=F, by=c('date_cmc', 'uuid'))

all <- all %>%
  filter(date_cmc >= 973 & date_cmc <= 1428)

#Interpolate missing data, since it seems to be only a few random, non-sequential months in random uuids
all <- all %>%
  group_by(uuid, survey) %>%
  arrange(date_cmc) %>%
  mutate(spei01 = na.approx(spei01),
         spei03 = na.approx(spei03),
         spei06 = na.approx(spei06))

fwrite(all, 'all.csv', row.names=F)



