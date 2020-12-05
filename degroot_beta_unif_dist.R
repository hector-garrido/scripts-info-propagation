library(dplyr)

dist <- rbeta(1000,2,5)

dist %>% hist(breaks=20,main='distribution t=0')
lines(rep(median(dist),2),c(-1,150),col='blue')
lines(rep(mean(dist),2),c(-1,150),col='red')
text(median(dist)-.04,120,labels='median',col='blue')
text(mean(dist)+.04,120,labels='mean',col='red')

hist(rep(mean(dist),1000),xlim=c(0,1),main='distribution t=inf',xlab = '')
lines(rep(mean(dist)-.002,2),c(-1,1000),col='red')
lines(rep(mean(dist)+.002,2),c(-1,1000),col='blue')
text(mean(dist)+.04,140,labels='mean',col='red')
text(mean(dist)-.04,140,labels='median',col='blue')

hist(c(.1,.4,.6,.8,1),xlim=c(0,1),main='uniform distribution',xlab = '')
lines(rep(.5-.002,2),c(-1,1000),col='red')
lines(rep(.5+.002,2),c(-1,1000),col='blue')
text(.5+.04,.7,labels='mean',col='red')
text(.5-.04,.7,labels='median',col='blue')

hist(c(.2,.4,.6,.8,1),xlim=c(0,1),main='uniform distribution after treatment',xlab = '')
lines(rep(.54-.002,2),c(-1,1000),col='red')
lines(rep(.5+.002,2),c(-1,1000),col='blue')
text(.54+.04,.7,labels='mean',col='red')
text(.5-.04,.7,labels='median',col='blue')

hist(c(0,.2,.3,.4,.5,.8,.9,.8,.9,1),xlim=c(0,1),main='uniform distribution after treatment',xlab = '',breaks=10)
lines(rep(.54-.002,2),c(-1,1000),col='red')
lines(rep(.7+.002,2),c(-1,1000),col='blue')
text(.54+.04,.7,labels='mean',col='red')
text(.7+.04,.7,labels='median',col='blue')
