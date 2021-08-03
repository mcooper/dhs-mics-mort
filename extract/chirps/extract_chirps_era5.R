library(gdalUtils)
library(dplyr)
library(SPEI)
library(lubridate)
library(data.table)
library(doParallel)
library(raster)
library(zoo)

aa <- fread('~/mortalityblob/mortnew/chirps/admin_areas_matching.csv')
aa[is.na(aa)] <- 0

#Read in precipitation data and create a gdal VRT file
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files('~/mortalityblob/chirps', 
                           pattern='.*tif$',
                           full.names=T)
gdalbuildvrt(precip_files, precip_vrt_file, separate=TRUE, verbose=T,
             overwrite=TRUE, a_srs='EPSG:4326')

#Make cluster to parallelize the extraction
cl <- makeCluster(detectCores(), outfile = '')
registerDoParallel(cl)

#Figure these scale functions out with trial and error using gdallocationinfo
scale_x <- function(x){
  x[x < 0] <- x[x < 0] + 360
  x*4
}

scale_y <- function(x){
  x <- -x + 90
  x*5
}

scale_t <- function(t){
  t*0.00176319797961702 + 257.7036700977876 - 273.15
}

aa <- aa[order(aa$uuid), ]

uus <- unique(aa$uuid)
uus <- uus[!uus %in% list.files('~/mortalityblob/mortnew/uuidtmp/')]

res <- foreach(uu=unique(aa$uuid), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo', 'lubridate', 'data.table'), .combine=bind_rows) %dopar% {

  sel <- aa %>%
    filter(uuid == uu)

  # If all weights are equal to 0, set to 1
  if(all(sel$ag == 0)){
    sel$ag <- 1
  }
  if(all(sel$pop == 0)){
    sel$pop <- 1
  }

  #Get sum of weights
  popw <- sum(sel$pop)
  agw <- sum(sel$ag)

  res <- data.frame(date=substr(seq(ymd("1981-01-01"), ymd("2020-12-01"), by='month'), 1, 7), wb_pop=0, wb_ag=0)

  cat(min(which(aa$uuid == uu))/nrow(aa), '\n')
  for (i in 1:nrow(sel)){
    #Extract time series of precipitation data
    precip <- as.numeric(gdallocationinfo(precip_vrt_file, sel$x[i], sel$y[i],
                                          wgs84=TRUE, valonly=TRUE))

    if (any(precip == -9999)){
      #Remove weight value from weight total
      popw <- popw - sel$pop[i]
      agw <- agw - sel$ag[i]
      next
    }

    #Extract temperature data
    temp <- scale_t(as.numeric(gdallocationinfo('NETCDF:"/home/mattcoop/mortalityblob/era5/era5-temp.nc":t2m', scale_x(sel$x[i]), scale_y(sel$y[i]), valonly=TRUE)))

    wb <- as.numeric(precip - thornthwaite(temp, sel$y[i]))

    res$wb_pop <- res$wb_pop + wb*sel$pop[i]
    res$wb_ag <- res$wb_ag + wb*sel$ag[i]
  }

  res$wb_pop <- res$wb_pop/popw
  res$wb_ag <- res$wb_ag/agw

  res$spei01=round(as.numeric(spei(res$wb_pop, 1, na.rm=TRUE)$fitted), 3)
  res$spei02=round(as.numeric(spei(res$wb_pop, 2, na.rm=TRUE)$fitted), 3)
  res$spei03=round(as.numeric(spei(res$wb_pop, 3, na.rm=TRUE)$fitted), 3)
  res$spei06=round(as.numeric(spei(res$wb_ag, 6, na.rm=TRUE)$fitted), 3)
  res$spei12=round(as.numeric(spei(res$wb_ag, 12, na.rm=TRUE)$fitted), 3)
  res$spei24=round(as.numeric(spei(res$wb_ag, 24, na.rm=TRUE)$fitted), 3)
  res$spei36=round(as.numeric(spei(res$wb_ag, 36, na.rm=TRUE)$fitted), 3)
  res$spei48=round(as.numeric(spei(res$wb_ag, 48, na.rm=TRUE)$fitted), 3)

  res$wb_pop <- round(res$wb_pop/popw, 3)
  res$wb_ag <- round(res$wb_ag/agw, 3)

  res$uuid <- uu

  fwrite(res, paste0('~/mortalityblob/mortnew/uuidtmp/', uu))

}

system('telegram "Donezo!"')

cd ~/mortalityblob/mortnew/uuidtmp/
cat uu0001 | head -1 > ../chirps_spei_uuids.csv
cat * | grep -a -v wb_pop >> ../chirps_spei_uuids.csv




