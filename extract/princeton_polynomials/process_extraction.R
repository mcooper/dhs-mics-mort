library(data.table)
library(zoo)

setwd('~/mortalityblob/mortnew')

pol <- fread('princeton_polynomials/monthly_polynomials.csv',
             col.names=c('month', 'p1', 'p2', 'p3', 'p4', 't1', 't2', 't3', 't4', 
                         'p1.y', 'p2.y', 'p3.y', 'p4.y', 't1.y', 't2.y', 't3.y', 't4.y',
                         'x', 'y'))

pol$x <- round(pol$x, 3); pol$y <- round(pol$y, 3)
setkey(pol, x, y, month)

pop <- fread('princeton_polynomials/pop.csv')
pop$x <- round(pop$x, 3); pop$y <- round(pop$y, 3)
pop$x[pop$x > 180] <- pop$x[pop$x > 180] - 360

uuids <- fread('princeton_polynomials/uuids_matching.csv')
uuids$x <- round(uuids$x, 3); uuids$y <- round(uuids$y, 3)

dates <- fread('princeton_polynomials//uuid_dates.csv')

from_cmc <- function(cmc){
  y = 1900 + floor((cmc - 1)/12)
  m = substr(100 + (cmc - 12 * (y - 1900)), 2, 3)
  return(paste0(y, '-', m))
}

uuid_means <- data.frame()
for (id in unique(uuids$uuid)){
  print(id)

  sel <- uuids[uuid == id, ]
  sel <- merge(sel, pop, all.x=T, all.y=F)

  #Normalize weights
  if (all(sel$pop == 0) | any(is.na(sel$pop))){
    sel$pop <- 1/nrow(sel)
  }else{
    sel$pop <- sel$pop/sum(sel$pop)
  }

  dmax <- from_cmc(dates$max_date[dates$uuid == id])
  dmin <- from_cmc(dates$min_date[dates$uuid == id])

  polsel <- merge(sel, pol, all.x=T, all.y=F)
  polsel <- polsel[polsel$month > dmin & polsel$month < dmax, ]

  res <- polsel[ , .(p1=sum(p1*pop), p2=sum(p2*pop), p3=sum(p3*pop), p4=sum(p4*pop),
              t1=sum(t1*pop), t2=sum(t2*pop), t3=sum(t3*pop), t4=sum(t4*pop),
              p1.y=sum(p1.y*pop), p2.y=sum(p2.y*pop), p3.y=sum(p3.y*pop), p4.y=sum(p4.y*pop),
              t1.y=sum(t1.y*pop), t2.y=sum(t2.y*pop), t3.y=sum(t3.y*pop), t4.y=sum(t4.y*pop)),
            .(month, uuid)]

  uuid_means <- rbind(uuid_means, res)
}

fwrite(uuid_means, 'princeton_polynomials/monthly_polynomials_wave.csv')


