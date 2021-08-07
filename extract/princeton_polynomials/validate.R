library(tidyverse)

#Select a random location

setwd('/home/mattcoop/mortalityblob/mortnew/princeton_polynomials/month_mod')

all <- readRDS('all.RDS')
two <- readRDS('2yr.RDS')

setwd('~/mortalityblob/princeton/')

#Lets get a time series from Kissa
t <- tail(as.numeric(system('gdallocationinfo temp.vrt -wgs84 -valonly 10 -6', intern=T)), 31) - 273.15
p <- tail(as.numeric(system('gdallocationinfo prcp.vrt -wgs84 -valonly 10 -6', intern=T)), 31)*86400

pred.poly <- function(mod, t, p, td=0, pd=0){
  t1 <- sum(t + td)
  p1 <- sum(p + pd)

  # Get polynomials
  p2 <- p1^2
  t2 <- t1^2
  p3 <- p1^3
  t3 <- t1^3
  p4 <- p1^4
  t4 <- t1^4

  pred <- (p1*mod['p1'] + p2*mod['p2'] + p3*mod['p3'] + p4*mod['p4'] + 
   t1*mod['t1'] + t2*mod['t2'] + t3*mod['t3'] + t4*mod['t4'])

  pred
}

l <- data.frame()
for (td in -10:10){
  l <- bind_rows(l, data.frame(delt=td, pred=pred.poly(all, t, p, td=td)))
}

l$diff <- l$pred - l$pred[l$delt == 0]

plot(l$delt, l$diff)


l2 <- data.frame()
for (td in -10:10){
  l2 <- bind_rows(l2, data.frame(delt=td, pred=pred.poly(two, t, p, td=td)))
}

l2$diff <- l2$pred - l2$pred[l2$delt == 0]

plot(l2$delt, l2$diff)
