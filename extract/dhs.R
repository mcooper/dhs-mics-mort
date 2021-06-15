library(tidyverse)
library(haven)

setwd('~/mortalityblob/dhsraw/')

fs <- list.files(pattern='..BR.....(dta|DTA|Dta)')

#######################################
# Scope birth files, remove duplicates
#######################################

df <- data.frame()
for (f in fs){
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                          ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                                 ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                        ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4, "X"))))
  temp <- data.frame(num=num, cc=cc, subversion=subversion, file=f, stringsAsFactors = F)
  df <- bind_rows(df, temp)
  print(f)
}

# #Scope duplicate surveys
# df <- df %>%
#   group_by(num, cc, subversion) %>%
#   mutate(n())

####################
# Read in all data
####################

alldhs <- data.frame()
for (i in 1:nrow(df)){
  print(i/nrow(df))
  
  dat <- read_dta(df$file[i])

  cols <- c('b3',   #CMC of date of birth
            'b5',   #whether child was alive or dead at time of interview
            'b7',   #age at death
            'v008', #CMC of date of interview
            'v101' #Region of residence
  )

  dat <- dat[ , cols[cols %in% names(dat)]]
 
  if ('b5' %in% names(dat)){
    dat$b5_int <- as.integer(dat$b5)
    dat$b5_chr <- as.character(as_factor(dat$b5))
    dat$b5 <- NULL
  } 

  if ('v101' %in% names(dat)){
    dat$v101_int <- as.integer(dat$v101)
    dat$v101_chr <- as.character(as_factor(dat$v101))
    dat$v101 <- NULL
  } 

  dat$survey <- paste(df$cc[i], df$num[i], df$subversion[i], sep='-')

  alldhs <- bind_rows(alldhs, dat)
}

write.csv(alldhs, '~/mortalityblob/mortnew/dhsraw.csv', row.names=F)

system('telegram "DONEZO"')
system('sudo poweroff')
