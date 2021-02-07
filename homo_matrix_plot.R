
# PLOTS DEL DOC DE OVERLEAF SOBRE LAMBDA 2 Y REDES HOMOFILICAS

evec_H <- eigen(H/5)$vectors
evec_H[1:10,1:10j]

cae_diag <- numeric(100)
for(i in 1:30){
  cae_diag[i] <- ((H7/7)%^%i)[400,400]
}

dist_0 <- seq(0,1,.001)
dist_0 <- dist_0[1:1000]

dist_1 <- dist_0
dist_1[380:530] <- dist_1[380:530] + .1

par(mfrow=c(1,1))
plot(density(dist_1,n=64,kernel='gaussian',bw=.01))
plot(density(((H7/7)%^%10)%*%dist_1,n=64,kernel='gaussian',bw=.01),xlim=c(-.2,1.2))
plot(density(((H7/7)%^%100)%*%dist_1,n=64,kernel='gaussian',bw=.01),xlim=c(-.2,1.2))
plot(density(((H7/7)%^%500)%*%dist_1,n=64,kernel='gaussian',bw=.01),xlim=c(-.2,1.2))
plot(density(((H7/7)%^%1000)%*%dist_1,n=64,kernel='gaussian',bw=.01),xlim=c(-.2,1.2))
plot(density(((H7/7)%^%2000)%*%dist_1,n=64,kernel='gaussian',bw=.01),xlim=c(-.2,1.2))
plot(density(((H7/7)%^%3000)%*%dist_1,n=64,kernel='gaussian',bw=.01),xlim=c(-.2,1.2))
plot(density(((H7/7)%^%40000)%*%dist_1,n=64,kernel='gaussian',bw=.01),xlim=c(-.2,1.2))

plot(density(((H7/7)%^%2)%*%dist_1))
plot(density(((H7/7)%^%3)%*%dist_1))
plot(density(((H7/7)%^%4)%*%dist_1))
plot(density(((H7/7)%^%5)%*%dist_1))
plot(density(((H7/7)%^%6)%*%dist_1))
plot(density(((H7/7)%^%7)%*%dist_1))
plot(density(((H7/7)%^%8)%*%dist_1))
plot(density(((H7/7)%^%9)%*%dist_1))

plot(dist_0,rep(0,1000),col=paste('grey',round(dist_0*100)),cex=.5)
plot(dist_0,rep(0,1000),col=paste('grey',round((((H7/7)%^%1)%*%dist_0)[,1]*100)),cex=.5)
plot(dist_0,rep(0,1000),col=paste('grey',round((((H7/7)%^%2)%*%dist_0)[,1]*100)),cex=.5)
plot(dist_0,rep(0,1000),col=paste('grey',round((((H7/7)%^%3)%*%dist_0)[,1]*100)),cex=.5)
plot(dist_0,rep(0,1000),col=paste('grey',round((((H7/7)%^%4)%*%dist_0)[,1]*100)),cex=.5)
plot(dist_0,rep(0,1000),col=paste('grey',round((((H7/7)%^%5)%*%dist_0)[,1]*100)),cex=.5)
plot(dist_0,rep(0,1000),col=paste('grey',round((((H7/7)%^%6)%*%dist_0)[,1]*100)),cex=.5)
plot(dist_0,rep(0,1000),col=paste('grey',round((((H7/7)%^%7)%*%dist_0)[,1]*100)),cex=.5)
plot(dist_0,rep(0,1000),col=paste('grey',round((((H7/7)%^%8)%*%dist_0)[,1]*100)),cex=.5)
plot(dist_0,rep(0,1000),col=paste('grey',round((((H7/7)%^%9)%*%dist_0)[,1]*100)),cex=.5)

par(mfrow=c(3,2))
for(i in 1:6){
  plot( ((H7/7)%^%(100*(i-1+1)))[300,] ,
        main=paste('t = ',100*(i-1+1)),ylab='',xlab='',ylim=c(0,.02),pch='.')
}

par(mfrow=c(2,2))
plot(((H7/7)%^%1)[1:20,1:20],main='A^1')
plot(((H7/7)%^%2)[1:20,1:20],main='A^2')
plot(((H7/7)%^%3)[1:20,1:20],main='A^3')
plot(((H7/7)%^%4)[1:20,1:20],main='A^4')
