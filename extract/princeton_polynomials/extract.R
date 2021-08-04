library(data.table)
library(tidyverse)
library(gdalUtils)
library(raster)
library(lubridate)
library(doParallel)

setwd('~/mortalityblob/mortnew')

pts <- fread('princeton_polynomials//uuids_matching.csv')
pts$x2 <- pts$x
pts$x2[pts$x2 < 0] <- pts$x2[pts$x2 < 0] + 360

setwd('~/mortalityblob/princeton')

# # gdalbuildvrt() is failing so build them in bash
# cd ~/mortalityblob/princeton
# ls | grep temp.\*tif > tempvrtlist
# ls | grep prcp.\*tif > prcpvrtlist
# gdalbuildvrt -separate -input_file_list tempvrtlist temp.vrt
# gdalbuildvrt -separate -input_file_list prcpvrtlist prcp.vrt

ts <- seq(ymd('1980-01-01'), ymd('2016-12-31'), by='day')
tsm <- substr(ts, 1, 7)

cl <- makeCluster(detectCores(), outfile = '')
registerDoParallel(cl)

foreach(i=1:nrow(pts), .packages=c('data.table', 'gdalUtils')) %dopar% {
  cat(i/nrow(pts), '\n')

  p <- as.numeric(gdallocationinfo('prcp.vrt', pts$x2[i], pts$y[i], valonly=TRUE))
  t <- as.numeric(gdallocationinfo('temp.vrt', pts$x2[i], pts$y[i], valonly=TRUE))

  # Convert to C and mm/day
  p1 <- p*86400
  t1 <- t-273.15

  # Get polynomials
  p2 <- p1^2
  t2 <- t1^2
  p3 <- p1^3
  t3 <- t1^3
  p4 <- p1^4
  t4 <- t1^4

  dt <- data.table(tsm, p1, p2, p3, p4, t1, t2, t3, t4)
  dt <- dt[ , .(p1=sum(p1), p2=sum(p2), p3=sum(p3), p4=sum(p4),
                t1=sum(t1), t2=sum(t2), t3=sum(t3), t4=sum(t4)), tsm]

  dt$x <- pts$x[i]
  dt$y <- pts$y[i]

  fwrite(dt, paste0('~/mortalityblob/mortnew/tmp/', i), col.names=F)

}

system('telegram "Donezo!"')
system('sudo poweroff')


