#####################################
# Notes: 
# Compared to DHS & MICS, IPUMS has lots of poorly coded mortality.
# Brazil doesnt have birth month, so we excluded
# For surveys with date of child death, in many cases, the child's death was before the birth in ~2% of cases
# For surveys with the age of death, a large number are coded 98 for "unknown" when they died
# So, not using IPUMS

library(tidyverse)
library(data.table)

dat <- fread('~/mortalityblob/ipums/ipumsi_00004.csv') %>%
  select(COUNTRY, YEAR, GEOLEV2, LASTBMO, LASTBYR, CHDEADYR, CHDEADMO, AGEDEADYR, 
         AGEDEADMO, LASTBMORT, HHWT) %>%
  merge(data.frame(COUNTRY=c(76, 332, 404, 484, 710, 800),
                   ISO3=c("BRA", "HTI", "KEN", "MEX", "ZAF", "UGA"))) %>%
  mutate(survey = paste0(ISO3, '-', YEAR))

####### For surveys with age of death, many unknowns #######
tab(dat$survey, case_when(dat$AGEDEADMO == 98 ~ 'unknown', 
                          dat$AGEDEADMO == 97 ~ '< 1 yr, months unknown',
                          dat$AGEDEADMO < 97 ~ 'known',
                          dat$AGEDEADMO == 99 ~ 'NIU', 
                          is.na(dat$AGEDEADYR) ~ NA_character_))

####### For surveys with date of death, many unknowns, many died before born! #############
tab(dat$survey, case_when(dat$CHDEADMO == 98 ~ 'unknown', 
                          dat$CHDEADMO == 99 ~ 'NIU', 
                          is.na(dat$CHDEADYR) ~ NA_character_))

sel <- dat %>%
  # Valid CHDEADMO records
  filter(CHDEADYR < 3000, CHDEADMO < 90, LASTBMORT == '2', LASTBYR < 3000, LASTBMO < 90) %>% 
  mutate(death_cmc = 12*(CHDEADYR - 1900) + CHDEADMO,
         birth_cmc = 12*(LASTBYR - 1900) + LASTBMO) 


################# SO: NOT USING IPUMS ################################

# Old code:

library(tidyverse)
library(data.table)

dat <- fread('~/mortalityblob/ipums/ipumsi_00004.csv')

#Only collect mortality and other data
dat <- dat %>%
  select(COUNTRY, YEAR, GEOLEV2, LASTBMO, LASTBYR, CHDEADYR, CHDEADMO, AGEDEADYR, 
         AGEDEADMO, LASTBMORT, HHWT)

cty <- data.frame(COUNTRY=c(76, 332, 404, 484, 710, 800),
                  ISO3=c("BRA", "HTI", "KEN", "MEX", "ZAF", "UGA"))

dat <- merge(dat, cty)

## Cant use Brazil, bc we only have deathdate, not age or birthdate
dat <- dat[dat$ISO3 != 'BRA', ]

## Cant use those where mortality status of child is 'Unknown' or 'NIU'
dat <- dat[!dat$LASTBMORT %in% c(0, 9), ]

## Make Survey Code
dat$survey <- paste0(dat$ISO3, '-', dat$YEAR)

#########################################
### JUST DO COUNTRIES WITH CHDEAD DATA
#########################################
sel <- dat %>%
  filter(survey %in% c('HTI-2003', 'KEN-1989', 'ZAF-2011', 'ZAF-2016'))

sel <- sel[(sel$CHDEADYR < 3000 & sel$CHDEADMO < 90), ] #Remove children that are Unknown or NIU, and are missing death age sela
sel$death_cmc <- 12*(sel$CHDEADYR - 1900) + sel$CHDEADMO
sel$death_cmc[sel$CHDEADYR == 0] <- NA

sel <- sel[sel$LASTBYR < 3000 & sel$LASTBMO < 90, ] #Remove children that are Unknown or NIU
sel$birth_cmc <- 12*(sel$LASTBYR - 1900) + sel$LASTBMO

sel <- sel %>%
  mutate(dage = death_cmc - birth_cmc) %>%
  filter((dage < 12) | is.na(dage))


#########################################
### NOW DO COUNTRIES WITH AGEDEAD DATA
#########################################
sel2 <- dat %>%
  filter(

##################
# Birth Year
##################
dat <- dat[dat$LASTBYR < 3000 & dat$LASTBMO < 90, ] #Remove children that are Unknown or NIU

dat$birth_cmc <- 12*(dat$LASTBYR - 1900) + dat$LASTBMO

#######################################################
# Earliest possible CMC date - cutoff for analysis
######################################################
dat$survey_cmc <- 12*(dat$YEAR - 1900) + 1

#############################
# Age at Death in Months
#############################
tab(dat$survey, case_when(dat$AGEDEADMO == 98 ~ 'unknown', 
                          dat$AGEDEADMO == 97 ~ '< 1 yr, months unknown',
                          dat$AGEDEADMO < 97 ~ 'known',
                          dat$AGEDEADMO == 99 ~ 'NIU', 
                          is.na(dat$CHDEADYR) ~ NA_character_))

dat$AGEDEADMO



######################################

geo <- dat %>%
  select(survey, geo_id=GEOLEV2) %>%
  unique
write.csv(geo, '~/mortalityblob/mortnew/ipums-geo.csv', row.names=F)
