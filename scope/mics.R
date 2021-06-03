library(tidyverse)
library(haven)

setwd('~/mortalityblob/micsraw')

options(stringsAsFactors=F)

fs <- list.files(recursive=T, pattern='sav|SAV$')

df <- data.frame(full=fs, stringsAsFactors=F)

df$basename <- basename(df$full)
df$dir <- dirname(df$full)

#Get all household files for surveys that were for an entire nation (not subnational)
bhfiles <- which(tolower(df$basename) == 'bh.sav') 

df <- df[bhfiles, ]

label_process <- function(x){
  lab <- attributes(x)$label
  
  if(is.null(attributes(x)$label)){
    lab <- ""
  }
  
  final <- paste0(as.character(lab), collapse="")
  
  return(final)
  
}


fulldict <- data.frame(codes=NA, labels=NA)
for (i in 1:nrow(df)){
  table <- tryCatch(read_sav(df$full[i], encoding='utf-8'),
                    error=function(x){read_sav(df$full[i], encoding='latin1')})
  
  print(i/nrow(df))
  
  labels <- sapply(table, label_process) %>% unlist
  codes <- names(table)
 
  fulldicttmp <- data.frame(codes=tolower(codes), labels=tolower(labels), 
                            nas=sapply(table, function(x){length(unique(x[!is.na(x)]))}))

  if (basename(df$dir[i]) %in% names(fulldict)){
    stop("already have ", surveylab)
  }

  names(fulldicttmp)[3] <- basename(df$dir[i])
  
  fulldict <- merge(fulldict, fulldicttmp, all.x=T, all.y=T)
}

##########################################
# Make Survey-Specific Dictionaries
##########################################

surveys <- names(fulldict)[!names(fulldict) %in% c('codes', 'labels', 'codelabel')]

sd <- data.frame(surveys = c(surveys))
for (i in 1:nrow(sd)){
  survey <- sd$surveys[i]
  sel <- fulldict[!is.na(fulldict[ , survey]) , c('codes', 'labels', survey)]
  
  # #Use this to find codes based on labels
  # u(fulldict[grepl('interview|entrevista', fulldict$labels), c('codes', 'labels')])

  ################
  # survey_date
  ################
  #day
  s <- sel$codes[sel$codes %in% c('hh5d', 'wm6d')]
  if (length(s) > 0){
    sd[i, 'survey_day'] <- s[1]
  }
  #month
  s <- sel$codes[sel$codes %in% c('hh5m', 'wm6m')]
  if (length(s) > 0){
    sd[i, 'survey_month'] <- s[1]
  }
  #year
  s <- sel$codes[sel$codes %in% c('hh5y', 'wm6y')]
  if (length(s) > 0){
    sd[i, 'survey_year'] <- s[1]
  }
  #cmc
  s <- sel$codes[sel$codes %in% c('wdoi', 'hh5c', 'cmcdoiw', 'wm6c')]
  if (length(s) > 0){
    sd[i, 'survey_cmc'] <- s[1]
  }
  #flag
  s <- sel$codes[sel$codes %in% c('hh5f', 'wm6f')]
  if (length(s) > 0){
    sd[i, 'survey_flag'] <- s
  }

  ######################
  # birth_date
  ############################
  #day
  s <- sel$codes[sel$codes %in% c('bh4d')]
  if (length(s) > 0){
    sd[i, 'birth_day'] <- s
  }
  #month
  s <- sel$codes[sel$codes %in% c('bh4m', 'cm11em')]
  if (length(s) > 0){
    sd[i, 'birth_month'] <- s[1]
  }
  #year
  s <- sel$codes[sel$codes %in% c('bh4y', 'cm11ey')]
  if (length(s) > 0){
    sd[i, 'birth_year'] <- s[1]
  }
  #cmc
  s <- sel$codes[sel$codes %in% c('bh4c', 'ccdob')]
  if (length(s) > 0){
    sd[i, 'birth_cmc'] <- s
  }
  #flag
  s <- sel$codes[sel$codes %in% c('bh4f', 'impbh4f', 'cm11ef')]
  if (length(s) > 0){
    sd[i, 'birth_flag'] <- s
  }

  ##########################
  # age at death
  ###################
  # age (number)
  s <- sel$codes[sel$codes %in% c('bh9n', 'bh9b')]
  if (length(s) > 0){
    sd[i, 'deathage_number'] <- s[1]
  }
  # age (units)
  s <- sel$codes[sel$codes %in% c('bh9u', 'bh9a')]
  if (length(s) > 0){
    sd[i, 'deathage_units'] <- s[1]
  }
  # age (flag)
  s <- sel$codes[sel$codes %in% c('bh9f', 'impbh9f', 'cm11jf')]
  if (length(s) > 0){
    sd[i, 'deathage_flag'] <- s
  }

  ######################
  # hhweight
  #####################
  s <- sel$codes[sel$codes %in% c('hhweight')]
  if (length(s) > 0){
    sd[i, 'hhweight'] <- s
  }

  ######################
  # wmweight
  #####################
  s <- sel$codes[sel$codes %in% c('wmweight')]
  if (length(s) > 0){
    sd[i, 'wmweight'] <- s
  }

  #################
  # region
  ################
  rc <- c('area', 'hh6', 'hh7', 'dis', 'reg', 'prov', 
          'urb', 'zone')
  s <- sel$codes[grepl(paste0(rc, collapse='|'), sel$codes) & !grepl('disabil', sel$codes)]
  for (j in 1:length(s)){
    sd[i, paste0('region', j)] <- s[j]
  }

}

allmics <- data.frame()
for (i in 1:nrow(df)){
  print(i/nrow(df))
  table <- tryCatch(read_sav(df$full[i], encoding='utf-8'),
                    error=function(x){read_sav(df$full[i], encoding='latin1')})

  sel <- sd[i, -1]
  sel <- sel[, which(!is.na(sel)), drop=T]

  table <- table[ , match(sel, tolower(names(table)))]

  names(table) <- names(sel)

  for (n in names(table)){
    if (is.labelled(table[ , n, drop=T])){
      table[ , paste0(n, '_chr')] <- as.character(as_factor(table[ , n, drop=T]))
      table[ , n] <- as.character(as.numeric(table[ , n, drop=T]))
    }
    else{
      table[ , n] <- as.character(table[ , n, drop=T])
    }
  }

  table$country <- trimws(gsub('-|_|[[:digit:]]+', '', substr(basename(df$dir[i]), 1, sapply(basename(df$dir[i]), function(x){gregexpr('MICS', x)[[1]][1] - 2}))))
  table$survey <- basename(df$dir[i])

  allmics <- bind_rows(allmics, table)
}

####################################
# Get Geographic Variables, Make ID
####################################

geo <- unique(allmics[ , c('country', names(allmics)[grepl('region', names(allmics))])])

geo <- geo %>%
  group_by(country) %>%
  mutate(geo_code = paste0(country, row_number()))

allmics <- merge(allmics, geo, all.x=T)

a <- allmics

####################################
# Process Other Data
####################################

############# Survey Date ##############
allmics$survey_cmc <- as.numeric(allmics$survey_cmc)
allmics$survey_cmc[allmics$country == 'Nepal'] <- allmics$survey_cmc[allmics$country == 'Nepal'] - 681

allmics$survey_year_cmc <- 1900 + floor((as.numeric(allmics$survey_cmc) - 1)/12)
allmics$survey_month_cmc <- allmics$survey_cmc - 12 * (allmics$survey_year_cmc - 1900)

############# Birth Date ##############
allmics$birth_cmc <- as.numeric(allmics$birth_cmc)
allmics$birth_cmc[allmics$country == 'Nepal'] <- allmics$birth_cmc[allmics$country == 'Nepal'] - 681

allmics$birth_year_cmc <- 1900 + floor((as.numeric(allmics$birth_cmc) - 1)/12)
allmics$birth_month_cmc <- allmics$birth_cmc - 12 * (allmics$birth_year_cmc - 1900)

# Remove imputed/flagged birthdays
allmics$birth_month_cmc[allmics$birth_flag != '1'] <- NA
allmics$birth_year_cmc[allmics$birth_flag != '1'] <- NA

# Remove weird Madagascar records with a year of 2733
allmics$birth_month_cmc[allmics$birth_year_cmc == 2733] <- NA
allmics$birth_year_cmc[allmics$birth_year_cmc == 2733] <- NA

############### Death Age ###################

# 1 - days
# 2 - months
# 3 - years
# 9 - NA


write.csv(geo, '~/mortalityblob/mortnew/geo.csv', row.names=F)




