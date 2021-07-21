library(gdalUtils)
library(dplyr)
library(SPEI)
library(raster)
library(lubridate)
library(zoo)
library(pbapply)
library(doParallel)

aa <- read.csv('~/mortalityblob/mortnew/chirps/admin_areas_matching.csv')
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

res <- foreach(uu=unique(aa$uuid), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {

  sel <- aa %>%
    filter(uuid == uu)

  # If all weights are equal to 0, set to 1
  if(all(sel$ag == 0)){
    sel$ag <- 1
  }
  if(all(sel$pop == 0)){
    sel$pop <- 1
  }

  sel$pop <- sel$pop/sum(sel$pop)
  sel$ag <- sel$ag/sum(sel$ag)

  res <- data.frame(date=substr(seq(ymd("1981-01-01"), ymd("2020-12-01"), by='month'), 1, 7), wb_pop=0, wb_ag=0)

  cat(min(which(aa$uuid == uu))/nrow(aa), '\n')
  for (i in 1:nrow(sel)){
    print(i/nrow(sel))

    #Extract time series of precipitation data
    precip <- as.numeric(gdallocationinfo(precip_vrt_file, sel$x[i], sel$y[i],
                                          wgs84=TRUE, valonly=TRUE))

    temp <- scale_t(as.numeric(gdallocationinfo('NETCDF:"/home/mattcoop/mortalityblob/era5/era5-temp.nc":t2m', scale_x(sel$x[i]), scale_y(sel$y[i]), valonly=TRUE)))

    precip[precip == -9999] <- NA
    temp[temp < -70] <- NA

    wb <- as.numeric(precip - thornthwaite(temp, sel$y[i]))

    res$wb_pop <- res$wb_pop + wb*sel$pop[i]
    res$wb_ag <- res$wb_ag + wb*sel$ag[i]
  }



  #Run summary stats using the time series, like getting z-scores, percentiles, and SPI score
  precip_zscore <- (precip - mean(precip, na.rm=T))/sd(precip, na.rm=T)
  precip_percentile <- ecdf(precip)(precip)*100
  precip_spi12 <- as.numeric(spi(precip, 12, na.rm=TRUE)$fitted)
  
  prp_df <- date.frame(date=seq(ymd("1981-01-01"), ymd("2020-01-01"), by='month'), #Make a date column
                                                                                   #Should matches dates of tiff files
                                                                                   #and format of DATE column of mydata.csv
                       precip_zscore,
                       precip_percentile,
                       precip_spi12)
  
  #Subset point data to the code we are iterating on 
  spsel <- sp[sp$tmpcode==rll$tmpcode[n], ]
  
  comb <- merge(spsel, prp_df, all.x=T, all.y=F) %>%
	  dplyr::select(-tmpcode)

  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  
  comb
}

write.csv(result, '~/datadir/mydata_climate_metrics.csv', row.names=F)



