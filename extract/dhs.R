library(tidyverse)
library(haven)
library(data.table)

setwd('~/mortalityblob/dhsraw/')

fs <- list.files(pattern='..BR.....(dta|DTA|Dta)', recursive=T)

#######################################
# Scope birth files, remove duplicates
#######################################

df <- data.frame()
for (f in fs){
  b <- basename(f)
  num <- substr(b, 5, 5)
  cc <- toupper(substr(b, 1, 2))
  subversion <- ifelse(toupper(substr(b, 6, 6)) %in% as.character(seq(0, 9)), 1,
                          ifelse(toupper(substr(b, 6, 6)) %in% LETTERS[1:8], 2, 
                                 ifelse(toupper(substr(b, 6, 6)) %in% LETTERS[9:17], 3, 
                                        ifelse(toupper(substr(b, 6, 6)) %in% LETTERS[18:26], 4, "X"))))
  india <- grepl('India', f)
  temp <- data.frame(num=num, cc=cc, subversion=subversion, india=india, file=f, stringsAsFactors = F)
  df <- bind_rows(df, temp)
}

# #Scope duplicate surveys
# df %>%
#   group_by(num, cc, subversion, india) %>%
#   mutate(n = n()) %>%
#   arrange(desc(n))

#################################
# Read in all data
#################################
alldhs <- data.frame()
for (i in 304:nrow(df)){
  print(i/nrow(df))
  
  dat <- read_dta(df$file[i])

  cols <- c('b3',   #CMC of date of birth
            'b7',   #age at death
            'v008', #CMC of date of interview
            'v101'  #Region of residence
  )

  dat <- dat[ , cols[cols %in% names(dat)]]
 
  if ('v101' %in% names(dat)){
    dat$v101_int <- as.integer(dat$v101)
    dat$v101_chr <- as.character(as_factor(dat$v101))
    dat$v101 <- NULL
  } 

  dat$survey <- paste(df$cc[i], df$num[i], df$subversion[i], sep='-')
  dat$survey <- ifelse(df$india[i], paste0(dat$survey, '-IA'), dat$survey)

  alldhs <- bind_rows(alldhs, dat)
}

### Fix India Issues:
### Remove national survey and get state names from state-level surveys
alldhs <- alldhs %>%
  filter(!survey %in% c('IA-4-1'))

ind <- read.csv(na.strings="",
                text='"cc", "state"\n"AP","Andhra Pradesh"\n"AR","Arunachal Pradesh"\n"AS","Assam"\n"BH","Bihar"\n"DL","Delhi"\n"GJ","Gujarat"\n"GO","Goa"\n"HP","Himachal Pradesh"\n"HR","Haryana"\n"JM","Jammu"\n"KA","Karnataka"\n"KE","Kerala"\n"MG","Meghalaya"\n"MH","Maharastra"\n"MN","Manipur"\n"MP","Madhya Pradesh"\n"MZ","Mizoram"\n"NA","Nagaland"\n"OR","Orissa"\n"PJ","Punjab"\n"RJ","Rajasthan"\n"TN","Tamil Nadu"\n"TR","Tripura"\n"UP","Uttar Pradesh"\n"WB","West Bengal"\n"SK","Sikkim"\n')

inddhs <- alldhs %>%
  filter(grepl("-IA", survey)) %>%
  mutate(cc = substr(survey, 1, 2)) %>%
  merge(ind) %>%
  mutate(v101_chr = state, 
         survey = 'IA-4-1') %>%
  select(-cc, -state)

alldhs <- bind_rows(inddhs,
                    alldhs %>%
                      filter(!grepl("-IA", survey)))

# Missing regions for Botswana, just use country level
alldhs$v101_chr[alldhs$survey == 'BT-0-1'] <- "Botswana"

geo <- unique(alldhs[ , c('survey', 'v101_chr')])

geo <- geo %>%
  group_by(survey) %>%
  mutate(geo_code = paste0(survey, '_', row_number()))

alldhs <- merge(alldhs, geo, all.x=T)

fwrite(alldhs, '~/mortalityblob/mortnew/dhsraw.csv', row.names=F)
fwrite(geo, '~/mortalityblob/mortnew/dhs-geo.csv', row.names=F)

################ Collect relevant cols and make event-history structure ###################
alldhs <- alldhs %>%
  select(survey_cmc=v008, birth_cmc=b3, deathage_months=b7, geo_code)
alldhs <- alldhs[alldhs$survey_cmc >= alldhs$birth_cmc, ]

dhs <- list()
for (i in 1:nrow(alldhs)){
  if (i %% 1000 == 0){
    print(i/nrow(alldhs))
  }
  df <- data.frame(date_cmc=alldhs$birth_cmc[i]:alldhs$survey_cmc[i])
  df$age <- df$date_cmc - alldhs$birth_cmc[i]
  df$months_before_survey <- (nrow(df):1) - 1
  if (is.na(alldhs$deathage_months[i])){
    df$death <- FALSE
  } else{
    df$death <- df$age >= alldhs$deathage_months[i]
    if (any(df$death)){
      df <- df[1:min(which(df$death)), ]
    }
  }
  df$geo_code <- alldhs$geo_code[i]
  df <- df[df$age < 12, ]
  dhs[[i]] <- df
}

final <- bind_rows(dhs)

write.csv(final, '~/mortalityblob/mortnew/dhs.csv')

system('telegram "DONEZO!"')




