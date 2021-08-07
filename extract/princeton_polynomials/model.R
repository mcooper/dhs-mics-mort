library(data.table)
library(fixest)
library(splines)

setwd('~/mortalityblob/mortnew/princeton_polynomials/')

d <- fread('all_monthly.csv')

d$moy <- factor(d$date_cmc %% 12)

d$srv_moy <- paste0(d$survey, '-', d$moy)

mod <- feols(death ~ age + p1 + p2 + p3 + p4 + t1 + t2 + t3 + t4 + ns(date_cmc, df=4) | geo_code + srv_moy, data=d)

m1coef <- coefficients(mod)

rm(mod)

mod <- feols(death ~ age + p1 + p2 + p3 + p4 + t1 + t2 + t3 + t4 + ns(date_cmc, df=4) | geo_code + srv_moy, data=d[d$months_before_survey <= 24])

m2coef <- coefficients(mod)

saveRDS(m1coef, '~/mortalityblob/mortnew/princeton_polynomials/month_mod/all.RDS')
saveRDS(m2coef, '~/mortalityblob/mortnew/princeton_polynomials/month_mod/2yr.RDS')
