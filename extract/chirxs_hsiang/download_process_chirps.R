#####################################################
#' gdal_calc is stupid and only takes
#' rasters labeled with capital letters "A", "B", "C", etc.
#' so it can only handle 26 rasters
#'
#' But changing the AlphaList variable on line 57 of `gdal_calc.py`
#' makes it accept more arguments.
#' https://gis.stackexchange.com/questions/147457/raster-caculator-qgis-2-6-v-s-gdal-calc-py-by-terminal/147492

library(tidyverse)
library(lubridate)

setwd('/mnt')

gdal_sum <- function(m, p){
  fs <- list.files(path=m, full.names=T, pattern=paste0('p', p, '.tif'))
  files <- paste0(paste0('-', c(LETTERS, letters)[1:length(fs)], ' ', fs), collapse=' ')
  com <- paste0('gdal_calc.py ', files, 
                ' --outfile=/home/mattcoop/mortalityblob/mortnew/chirxs_hsiang/chirps.', m, '.', 
                p, '.tif --calc="(', 
                paste0(c(LETTERS, letters)[1:length(fs)], collapse='+'),
                ')" --co="COMPRESS=LZW"')
  system(com)
}

gdal_poly <- function(f, p){
  com <- paste0('gdal_calc.py -A ', f, 
                ' --outfile=', gsub('.tif', paste0('.p', p, '.tif'), f), ' --calc="(', 
                paste0(rep('A', p), collapse='*'),
                ')" --co="COMPRESS=LZW" --NoDataValue=-9999')
  system(com)
}

d <- seq(ymd('1981-01-01'), ymd('2020-12-31'), by='day')
ym <- unique(substr(d, 1, 7))

for (m in ym){
  start <- Sys.time()
  dir.create(m)

  sel <- d[grepl(m ,d)]
  
  #Download all files
  for (s in as.character(sel)){
    y <- substr(s, 1, 4)
    mn <- substr(s, 6, 7)
    dy <- substr(s, 9, 10)
    system(paste0('wget -P ', m, ' -c https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/', y, '/chirps-v2.0.', y, '.', mn, '.', dy, '.tif.gz'))
  }

  system(paste0('gunzip ', m, '/*'))

  #Get polynomials
  for (f in list.files(path=m, full.names=T)){
    gdal_poly(f, 2)
    gdal_poly(f, 3)
    gdal_poly(f, 4)
    system(paste0('mv ', f, ' ', gsub('.tif', '.p1.tif', f)))
  }

  #Sum to output directory
  gdal_sum(m, 1)
  gdal_sum(m, 2)
  gdal_sum(m, 3)
  gdal_sum(m, 4)

  system(paste0('rm -rf ', m))
  end <- Sys.time()
}
