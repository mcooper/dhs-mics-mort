library(tidyverse)
library(data.table)

dat <- fread('~/mortalityblob/ipums/ipumsi_00004.csv')

#Only collect mortality and other data
dat <- dat %>%
  select(COUNTRY, YEAR, GEOLEV2, LASTBMO, LASTBYR, CHDEADYR, CHDEADMO, AGEDEADYR, 
         AGEDEADMO, LASTBMORT)

cty <- data.frame(COUNTRY=c(76, 332, 404, 484, 710, 800),
                  ISO3=c("BRA", "HTI", "KEN", "MEX", "ZAF", "UGA"))

dat <- merge(dat, cty)

## Cant use Brazil, bc we only have deathdate, not age or birthdate
dat <- dat[dat$ISO3 != 'BRA', ]

## Cant use those where mortality status of child is 'Unknown' or 'NIU'
dat <- dat[!dat$LASTBMORT %in% c(0, 9), ]

#################
# Survey
#################
dat$survey <- paste0(dat$ISO3, '-', dat$YEAR)

##################
# Death Year
##################
#tab(dat$survey, case_when(dat$CHDEADYR == 0 ~ 0, dat$CHDEADYR > 2020 ~ 9999, is.na(dat$CHDEADYR) ~ NA_real_, TRUE ~ 1))

dat <- dat[(dat$CHDEADYR < 3000 & dat$CHDEADMO < 90) | !is.na(dat$AGEDEADMO), ] #Remove children that are Unknown or NIU, and are missing death age data

dat$death_cmc <- 12*(dat$CHDEADYR - 1900) + dat$CHDEADMO
dat$death_cmc[dat$CHDEADYR == 0] <- NA

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
#tab(dat$survey, case_when(dat$AGEDEADMO <= 97 ~ 1, dat$AGEDEADMO > 97 ~ 99, is.na(dat$CHDEADYR) ~ NA_real_))

dat$AGEDEADMO



######################################

geo <- dat %>%
  select(survey, geo_id=GEOLEV2) %>%
  unique
write.csv(geo, '~/mortalityblob/mortnew/ipums-geo.csv', row.names=F)
