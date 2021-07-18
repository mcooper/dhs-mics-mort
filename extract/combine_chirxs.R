library(data.table)

setDTthreads(32)

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

##############################
# Calculate evapotranspiration
##############################

library(SPEI)

d <- unique(all[ , c('x', 'y')])

all$spei1 <- NA
all$spei2 <- NA
all$spei3 <- NA
all$spei6 <- NA
all$spei12 <- NA
all$spei24 <- NA
all$spei36 <- NA
all$spei48 <- NA

for (i in 1:nrow(d)){
  cat(i/nrow(d))

  ix <- all$x == d$x[i] & all$y == d$y[i]

  s <- all[ix,]
  
  # water balance
  s$et0 <- hargreaves(lat = s$y[1], Tmin = s$tmin, Tmax=s$tmax, Pre = s$prp)
  s$wb <- s$prp - s$et0

  all$spei1[ix] <- round(as.numeric(spei(s$wb, 1, na.rm=TRUE)$fitted), 3)
  all$spei2[ix] <- round(as.numeric(spei(s$wb, 2, na.rm=TRUE)$fitted), 3)
  all$spei3[ix] <- round(as.numeric(spei(s$wb, 3, na.rm=TRUE)$fitted), 3)
  all$spei6[ix] <- round(as.numeric(spei(s$wb, 6, na.rm=TRUE)$fitted), 3)
  all$spei12[ix] <- round(as.numeric(spei(s$wb, 12, na.rm=TRUE)$fitted), 3)
  all$spei24[ix] <- round(as.numeric(spei(s$wb, 24, na.rm=TRUE)$fitted), 3)
  all$spei36[ix] <- round(as.numeric(spei(s$wb, 36, na.rm=TRUE)$fitted), 3)
  all$spei48[ix] <- round(as.numeric(spei(s$wb, 48, na.rm=TRUE)$fitted), 3)
}

fwrite(all, 'chirxs.csv')
