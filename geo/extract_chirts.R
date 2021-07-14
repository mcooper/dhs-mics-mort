library(raster)
library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)

cl <- makeCluster(2, outfile = '')
registerDoParallel()

d <- seq(ymd('1983-01-01'), ymd('2016-12-31'), by='day')
ym <- unique(substr(d, 1, 7))

setwd('~/mortalityblob/tmp')

foreach (m=ym[16:length(ym)], .packages=c('raster', 'lubridate')) %dopar% {

  dir.create(m)
  
  sel <- d[grepl(m, d)]
  
  for (s in as.character(sel)){
    y <- substr(s, 1, 4)
    mn <- substr(s, 6, 7)
    dy <- substr(s, 9, 10)
    system(paste0('wget -P ', m, ' http://data.chc.ucsb.edu/products/CHIRTSdaily/v1.0/global_tifs_p05/Tmin/', y, '/Tmin.', y, '.', mn, '.', dy, '.tif'))
    system(paste0('wget -P ', m, ' http://data.chc.ucsb.edu/products/CHIRTSdaily/v1.0/global_tifs_p05/Tmax/', y, '/Tmax.', y, '.', mn, '.', dy, '.tif'))
  }
  
  tmx <- mean(stack(list.files(path=m, full.names=T, pattern='Tmax')))
  tmx[tmx == -9999] <- NA
  tmn <- mean(stack(list.files(path=m, full.names=T, pattern='Tmin')))
  tmn[tmn == -9999] <- NA
  
  writeRaster(tmx, paste0('~/mortalityblob/mortnew/chirts/Tmax.', m, '.tif'), format='GTiff')
  writeRaster(tmn, paste0('~/mortalityblob/mortnew/chirts/Tmin.', m, '.tif'), format='GTiff')
  
  system(paste0('rm -rf ', m))
}

system('telegram Donzo')



