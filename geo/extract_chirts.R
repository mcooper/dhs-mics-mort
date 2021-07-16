#####################################################
#' gdal_calc is stupid and only takes
#' rasters labeled with capital letters "A", "B", "C", etc.
#' so it can only handle 26 rasters
#'
#' But changing the AlphaList variable on line 57 of `gdal_calc.py`
#' makes it accept more arguments.
#' https://gis.stackexchange.com/questions/147457/raster-caculator-qgis-2-6-v-s-gdal-calc-py-by-terminal/147492

library(raster)
library(tidyverse)
library(lubridate)

d <- seq(ymd('1983-01-01'), ymd('2016-12-31'), by='day')
ym <- unique(substr(d, 1, 7))

setwd('~/mortalityblob/tmp')

gdal_mean <- function(m, minmax){
  fs <- list.files(path=m, full.names=T, pattern=minmax)
  files <- paste0(paste0('-', c(LETTERS, letters)[1:length(fs)], ' ', fs), collapse=' ')
  com <- paste0('gdal_calc.py ', files, 
                ' --outfile=/home/mattcoop/mortalityblob/mortnew/chirts/', minmax, '.', 
                m, '.tif --calc="(', 
                paste0(c(LETTERS, letters)[1:length(fs)], collapse='+'),
                ')/', length(fs), '" --co="COMPRESS=LZW" --NoDataValue=-9999')
  system(com)
}

done <- unique(substr(list.files(pattern='Tmin', path='~/mortalityblob/mortnew/chirts'), 6, 12))
undone <- ym[!ym %in% done]
write.csv(undone, 'undone.csv', row.names=F)

while (length(list.files(path='~/mortalityblob/mortnew/chirts')) < 816){
  #Pick a month that hasnt been done yet
  undone <- read.csv('undone.csv')
  m <- sample(undone$x, 1)
  write.csv(undone[undone$x != m, , drop=FALSE], 'undone.csv', row.names=FALSE)

  dir.create(m)
  
  sel <- d[grepl(m, d)]
  
  for (s in as.character(sel)){
    y <- substr(s, 1, 4)
    mn <- substr(s, 6, 7)
    dy <- substr(s, 9, 10)
    system(paste0('wget -P ', m, ' http://data.chc.ucsb.edu/products/CHIRTSdaily/v1.0/global_tifs_p05/Tmin/', y, '/Tmin.', y, '.', mn, '.', dy, '.tif'))
    system(paste0('wget -P ', m, ' http://data.chc.ucsb.edu/products/CHIRTSdaily/v1.0/global_tifs_p05/Tmax/', y, '/Tmax.', y, '.', mn, '.', dy, '.tif'))
  }

  gdal_mean(m, 'Tmax')
  gdal_mean(m, 'Tmin')

  system(paste0('rm -rf ', m))
}

system('telegram DONE')

