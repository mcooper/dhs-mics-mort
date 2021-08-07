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

polys <- fread('princeton_polynomials//monthly_polynomials_wave.csv')

polys$date_cmc <- 12 * (as.numeric(substr(polys$month, 1, 4)) - 1900) + as.numeric(substr(polys$month, 6, 7))

###########################
# Combine All
###########################

comb <- merge(comb, geom)

comb <- merge(comb, polys[ , .(uuid, date_cmc, p1, p2, p3, p4, t1, t2, t3, t4)],
              by=c('date_cmc', 'uuid'))

fwrite(comb, 'princeton_polynomials/all_monthly.csv')

comb[ , c('p1', 'p2', 'p3', 'p4', 't1', 't2', 't3', 't4')] <- NULL
