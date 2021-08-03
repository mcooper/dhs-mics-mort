library(raster)
library(lubridate)

setwd('/mnt')

for (year in 1980:2016){
  system(paste0('wget http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/prcp_daily_', year, '-', year, '.nc'))
  system(paste0('wget http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/tas_daily_', year, '-', year, '.nc'))

  dates <- seq(ymd(paste0(year, '-01-01')), ymd(paste0(year, '-12-31')), by='day')

  prcp <- stack(paste0('prcp_daily_', year, '-', year, '.nc'))
  temp <- stack(paste0('tas_daily_', year, '-', year, '.nc'))

  for (i in 1:length(dates)){
    print(dates[i])
    writeRaster(prcp[[i]], paste0('~/mortalityblob/princeton/prcp_', dates[i]), format='GTiff')
    writeRaster(temp[[i]], paste0('~/mortalityblob/princeton/temp_', dates[i]), format='GTiff')
  }

  rm(prcp, temp)

  system('rm *')
}

system('telegram "Donezo!"')
system('sudo poweroff')



