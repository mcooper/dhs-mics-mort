#############################################################################################################
# This is a script to extract climate data at lat-long points in R
# It is useful when you need to work with a time series to get metrics based on long-term norms, 
#    such as percentiles, Z-scores, or SPI scores.
# It uses an obscure spatial library (gdalUtils) but is faster than any other approach I know of
# I learned about this approach from Alex Zvoleff, who likely get it from the Climate Hazards Group at UCSB
############################################################################################################

library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(pbapply)
library(doParallel)

library(ncdf4)

setwd('~/mortalityblob/cru_ts/')
# cdo mergetime *.nc cru_ts_all.nc 
gdallocationinfo('NETCDF:"cru_ts_all.nc":tmn', x=0, y=0, valonly=TRUE)

# Faster as vrt
setwd('~/mortalityblob/chirps/')
start <- Sys.time()
gdallocationinfo('NETCDF:"chirps-v2.0.monthly.nc":precip', x=10, y=10, valonly=TRUE)
end <- Sys.time()
end - start


#Directory with TIFFs, each at the same time step and with the same extent and resolution
setwd('~/tiffdir')

#Dataframe with columns X, Y, ID, and DATE
data <- read.csv('~/datadir/mydata.csv')
sp <- SpatialPointsDataFrame(coords=data[ c('X', 'Y')], data = data)

#Get example raster for extent and resolution, assign each cell a code
r <- raster(list.files()[1])
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[is.na(r)] <- NA #Set NA values (in some datasets, like CHRIPS, NAs might be -9999), so use codes[r==-9999] <- NA
sp@data$tmpcode <- extract(codes, sp)

#If you have terrestrial climate data and points near a coast, some of them may come up NA.  Match them to the nearest non-na grid cell.
if (sum(is.na(sp@data$tmpcode)) > 0){
  #Make code raster NA everywhere except coasts
  codes <- focal(codes, w=matrix(rep(1, 9), ncol=3), fun=function(x){ifelse(any(is.na(x)), x[5], NA)})
  spna <- sp[is.na(sp@data$tmpcode) , ]
  spna$tmpcode <- NULL
  badcoords <- spna@coords
  tmpcode <- pbapply(X = badcoords, MARGIN = 1, FUN = function(xy) codes@data@values[which.min(replace(distanceFromPoints(codes, xy), is.na(codes), NA))])
  badcoords <- cbind.data.frame(badcoords, tmpcode)
  spna <- merge(spna@data, badcoords)
  sp <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ])
}

#Get dataframe with codes of all grid cells that contain at least one point
rll <- sp@data %>% 
  group_by(tmpcode) %>%
  summarize(X=mean(X), Y=mean(Y)) 	

#Read in precipitation data and create a gdal VRT file
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files('.', pattern='^precip.*tif$') #If you have multiple variables in tiffs, like precip and temp,
                                                         #use regex to get only the tiffs for one variable type per VRT
gdalbuildvrt(precip_files, precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

#Make cluster to parallelize the extraction
cl <- makeCluster(8, outfile = '')
registerDoParallel(cl)

res <- foreach(n=1:nrow(rll), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {
  
  #Extract time series of precipitation data
  precip <- as.numeric(gdallocationinfo(precip_vrt_file, rll$X[n], rll$Y[n], wgs84=TRUE, valonly=TRUE))

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



