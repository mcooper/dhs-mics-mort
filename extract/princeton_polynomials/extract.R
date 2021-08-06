library(data.table)
library(tidyverse)
library(gdalUtils)
library(raster)
library(lubridate)
library(doParallel)
library(zoo)

setwd('~/mortalityblob/mortnew')

pts <- fread('princeton_polynomials//uuids_matching.csv')

convert_x <- function(x){
  x[x < 0] <- x[x < 0] + 360
  x
}

pts$x2 <- convert_x(pts$x)

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

foreach(i=1:nrow(pts), .packages=c('data.table', 'gdalUtils', 'zoo')) %dopar% {
  cat(i/nrow(pts), '\n')

  p <- as.numeric(gdallocationinfo('prcp.vrt', pts$x2[i], pts$y[i], valonly=TRUE, wgs84=TRUE))
  t <- as.numeric(gdallocationinfo('temp.vrt', pts$x2[i], pts$y[i], valonly=TRUE, wgs84=TRUE))

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

  dt$p1.y = rollapply(dt$p1, width=12, sum, align='right', fill=NA)
  dt$p2.y = rollapply(dt$p2, width=12, sum, align='right', fill=NA)
  dt$p3.y = rollapply(dt$p3, width=12, sum, align='right', fill=NA)
  dt$p4.y = rollapply(dt$p4, width=12, sum, align='right', fill=NA)
  dt$t1.y = rollapply(dt$t1, width=12, sum, align='right', fill=NA)
  dt$t2.y = rollapply(dt$t2, width=12, sum, align='right', fill=NA)
  dt$t3.y = rollapply(dt$t3, width=12, sum, align='right', fill=NA)
  dt$t4.y = rollapply(dt$t4, width=12, sum, align='right', fill=NA)

  dt$x <- pts$x[i]
  dt$y <- pts$y[i]

  dt <- dt[!is.na(dt$p1.y), ]

  fwrite(dt, paste0('~/mortalityblob/mortnew/tmp/', i), col.names=F)

}

system('telegram "Donezo!"')
system('sudo poweroff')

cd ~/mortalityblob/mortnew/tmp2
find . -type f -exec cat {} + > ../princeton_polynomials/monthly_polynomials.csv

