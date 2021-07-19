library(data.table)

setwd('~/mortalityblob/mortnew/')

uuids <- fread('chirps/admin_areas_matching.csv')
uuids$layer <- NULL
uuids$x <- round(uuids$x, 3)
uuids$y <- round(uuids$y, 3)

############################################################
# Process monthly files to have only points in admin areas
############################################################
for (f in list.files(path='chirts', full.names=T)){
  print(f)
  d <- fread(f, col.names=c('date', 'x', 'y', 'tmax'))
  d <- d[d$x %in% uuids$x & d$y %in% uuids$y, ]
  fwrite(d, f, col.names=FALSE)
}

############################################################
# Read and combine
############################################################

tmx <- rbindlist(lapply(list.files(path='chirts', pattern="Tmax", full.names=T), fread))
tmn <- rbindlist(lapply(list.files(path='chirts', pattern="Tmin", full.names=T), fread))
names(tmx) <- c('date', 'x', 'y', 'tmax')
names(tmn) <- c('date', 'x', 'y', 'tmin')

setkeyv(tmx, cols=c('x', 'y', 'date'))
setkeyv(tmn, cols=c('x', 'y', 'date'))

tmp <- merge(tmx, tmn)
rm(tmx, tmn)
gc()

prp <- fread('chirps/chirps.xyz')

setkeyv(prp, c('x', 'y', 'date'))

gc()

all <- merge(tmp, prp, all.x=T, all.y=F)

fwrite(all, 'chirxs.csv')

rm(tmp, prp)

gc()

#####################################
# Split it all up for Parallelization
#####################################

# Bash
cd ~/mortalityblob/mortnew/
sudo chown mattcoop /mnt 
mkdir /mnt/split
tail -n +2 chirxs.csv | split -l 408 - /mnt/split/

##############################
# Calculate evapotranspiration
##############################

setwd('/mnt/split')

fs <- list.files()

library(SPEI)
library(foreach)
library(doParallel)

cl <- makeCluster(detectCores(), outfile='')
registerDoParallel(cl)

foreach(f=fs, .packages=c('SPEI', 'data.table')) %dopar% {
  cat(which(fs==f)/length(fs), '\n')

  s <- fread(f, col.names=c('x', 'y', 'date', 'tmax', 'tmin', 'prp'))
  
  # water balance
  s$et0 <- hargreaves(lat = s$y[1], Tmin = s$tmin, Tmax=s$tmax, Pre = s$prp)
  s$wb <- s$prp - s$et0

  s$spei01 <- round(as.numeric(spei(s$wb, 1, na.rm=TRUE)$fitted), 3)
  s$spei02 <- round(as.numeric(spei(s$wb, 2, na.rm=TRUE)$fitted), 3)
  s$spei03 <- round(as.numeric(spei(s$wb, 3, na.rm=TRUE)$fitted), 3)
  s$spei06 <- round(as.numeric(spei(s$wb, 6, na.rm=TRUE)$fitted), 3)
  s$spei12 <- round(as.numeric(spei(s$wb, 12, na.rm=TRUE)$fitted), 3)
  s$spei24 <- round(as.numeric(spei(s$wb, 24, na.rm=TRUE)$fitted), 3)
  s$spei36 <- round(as.numeric(spei(s$wb, 36, na.rm=TRUE)$fitted), 3)
  s$spei48 <- round(as.numeric(spei(s$wb, 48, na.rm=TRUE)$fitted), 3)

  s <- s[ , c('x', 'y', 'date', "spei01", "spei02", "spei03",
              "spei06", "spei12", "spei24", "spei36", "spei48")]

  fwrite(s, f)
}

