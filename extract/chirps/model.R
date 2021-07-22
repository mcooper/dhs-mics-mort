library(fixest)
library(data.table)
library(tidyverse)

d <- fread('~/mortalityblob/mortnew/all.csv')

#Define Segmenting Function
piece.formula <- function(var.name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
      paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
            collapse = " + ", sep=""))
}

df <- data.frame(spei_range=seq(-2, 2, 0.1))
df$date_cmc <- 1320 #2010
df$survey <- "TZ-7-2"
df$uuid <- "uu1030"
for (spei in c('spei01', 'spei02', 'spei03', 'spei06', 'spei12', 'spei24', 'spei36', 'spei48')){
  print(spei)
  formula <- paste0("death ~ ", 
                       piece.formula(spei, c(-1, -0.5, 0, 0.5, 1)), ' + ',
                       'date_cmc | survey + uuid')
  
  mod <- feols(as.formula(formula), data=d %>% filter(age == 0, 
                                                      months_before_survey < 12,
                                                      area < 1e5))
  
  df[ , spei] <- df$spei_range
  
  df[ , paste0(spei, '_res')] <- predict(mod, df)
  rm(mod)
  gc()
}

df <- df %>%
  select(-matches('spei..$')) %>%
  gather(key, value, -spei_range, -date_cmc, -survey, -uuid)

df <- df %>%
  group_by(key) %>%
  mutate(value2 = value - value[spei_range == 0])

ggplot(df) + 
  geom_line(aes(x=spei_range, y=value2, color=key))
ggsave('~/dhs-mics-mort/res/chirps_spei_curves-age0_12mo_before_survey-area_1e5.png')
system('telegram "DONE"')
system('sudo poweroff')
