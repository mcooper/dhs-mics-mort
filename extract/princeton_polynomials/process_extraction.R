library(data.table)
library(zoo)

setwd('~/mortalityblob/mortnew')

pol <- fread('princeton_polynomials/monthly_polynomials.csv',
             col.names=c('month', 'p1', 'p2', 'p3', 'p4', 't1', 't2', 't3', 't4', 'x', 'y',
                         'p1.y', 'p2.y', 'p3.y', 'p4.y', 't1.y', 't2.y', 't3.y', 't4.y'))
pol$x <- round(pol$x, 3); pol$y <- round(pol$y, 3)
setkey(pol, x, y, month)

tmp <- pol[ , .(p1=mean(is.na(p1))), .(x, y)]

ggplot(tmp) + 
  geom_point(aes(x, y, color=tmp$p1))

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

for (id %in% unique(uuids$uuid)){
  sel <- uuids[uuid == id, ]
  sel <- merge(sel, pop, all.x=T, all.y=F)

  #Normalize weights
  if (all(sel$pop == 0)){
    sel$pop <- 1
  }else{
    sel$pop <- sel$pop/sum(sel$pop)
  }

  dmax <- from_cmc(dates$max_date[dates$uuid == id])
  dmin <- from_cmc(dates$min_date[dates$uuid == id])

  polsel <- merge(sel, pol, all.x=T, all.y=F)
  polsel <- polsel[polsel$month > dmin & polsel$month < dmax, ]

  polsel[ , .(p1=mean(p1*pop), p2=mean(p2*pop), p3=mean(p3*pop), p4=mean(p4*pop),














}


