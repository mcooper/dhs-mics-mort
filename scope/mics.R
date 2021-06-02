library(tidyverse)
library(haven)

setwd('~/mortalityblob/micsraw')

options(stringsAsFactors=F)

fs <- list.files(recursive=T, pattern='sav|SAV$')

df <- data.frame(full=fs, stringsAsFactors=F)

df$basename <- basename(df$full)
df$dir <- dirname(df$full)

label_process <- function(x){
  lab <- attributes(x)$label
  
  if(is.null(attributes(x)$label)){
    lab <- ""
  }
  
  final <- paste0(as.character(lab), collapse="")
  
  return(final)
  
}

labels <- sapply(dat, label_process) %>% unlist

sel <- df %>%
  filter(basename == 'bh.sav')

#######################################
# Get Labels
#######################################
fulldict <- data.frame()
for (i in 1:nrow(sel)){
  table <- tryCatch(read_sav(sel$full[i], encoding='utf-8'),
                    error=function(x){read_sav(sel$full[i], encoding='latin1')})
  
 
  fulldicttmp <- data.frame(codes=tolower(codes), labels=tolower(labels), 
                            nas=sapply(table, 
                                       function(x){length(unique(x[!is.na(x)]))/nrow(table)}),
                            basename=sel$basename[i],
                            survey=basename(sel$dir[i]))
  
  fulldict <- bind_rows(fulldict, fulldicttmp)
}

fulldict$mylab <- NA

# Survey CMC (if available)
fulldict$mylab[grepl("cmc", fulldict$labels) & grepl('enquête|entrevista|interview', fulldict$labels)] <- 's.cmc'

# Survey date
fulldict$mylab[grepl("day|jour", fulldict$labels) & grepl('enquête|entrevista|interview', fulldict$labels)] <- 's.dd'

# Survey month
fulldict$mylab[grepl("mois|month", fulldict$labels) & grepl('enquête|entrevista|interview', fulldict$labels)] <- 's.mm'

# Survey year
fulldict$mylab[grepl("ann|year", fulldict$labels) & grepl('enquête|entrevista|interview', fulldict$labels)] <- 's.yy'

tab(fulldict$survey, fulldict$mylab)


for (i in 1:nrow(sel)){
  table <- tryCatch(read_sav(sel$full[i], encoding='utf-8'),
                    error=function(x){read_sav(sel$full[i], encoding='latin1')})

  print(i/nrow(sel))
  
  labels <- sapply(table, label_process) %>% unlist
  codes <- names(table)

  # Date of Survey
  s.cmc <- codes[grepl("Date de l'enquête \\(CMC\\)", labels)]
  
  # Date of birth
  b.dd <- codes[grepl("Jour de naissance", labels)]
  b.yy <- codes[grepl("Année de naissance", labels)]
  b.mm <- codes[grepl("Mois de naissance", labels)]
  b.cmc <- codes[grepl("Date de naissance de l'enfant \\(CMC\\)", labels)]
  b.cmc.flag <- codes[grepl("drapeu

  # Age
  age <- codes[grepl("Age de l'enfant", labels)]
  
  # Age at death
  death.age.units <- codes[grepl("Age au décès \\(Unité\\)", labels)]
  death.age.number <- codes[grepl("Age au décès \\(Nombre\\)", labels)]

  # Region
  regions <- codes[grepl("Milieu|territoriale", labels)]

  # Province
  # Area






}

