
# install.packages('igraph')
# 
# adjm <- matrix(sample(0:1, 100, replace=TRUE, prob=c(0.9,0.1)), nc=10)
# g1 <- graph_from_adjacency_matrix( adjm ,mode='undirected')
# adjm <- matrix(sample(0:5, 100, replace=TRUE,
#                       prob=c(0.9,0.02,0.02,0.02,0.02,0.02)), nc=10)

library(igraph)

################################################################################

total_nodos <- 10000
vecinos <- 9
porc_sat <- 0.2

#g1 <- graph_from_adjacency_matrix( adjm ,mode='undirected')
g_reg <- sample_k_regular(total_nodos, vecinos, directed = FALSE, multiple = FALSE)
g_reg_mat <- as_adjacency_matrix(g_reg) %>% matrix(ncol=total_nodos)
g_reg_mat <- g_reg_mat + diag(total_nodos)

infectados_0 <- numeric(total_nodos)
infectados_0[1:(total_nodos*porc_sat)] <- 1
#infectados_0

#plot.igraph(g_reg,vertex.color=infectados_0)
#plot.igraph(g_reg,vertex.color=c(rep(0,25),rep(1,25)))

################################################################################

# multiplicacion
infectados_1 <- g_reg_mat%*%infectados_0
# vemos la ubicacion de los infectados
aux <- which((infectados_1-infectados_0)==1)
# tomamos la mitad de los infectados y los "perdonamos"
aux <- aux[which(as.numeric(runif(length(aux))>0.5)==1)]
auxx <- which(infectados_0==1)
infectados_1 <- numeric(total_nodos)
infectados_1[c(auxx,aux)] <- 1

# multiplicacion
infectados_2 <- g_reg_mat%*%infectados_1
# vemos la ubicacion de los infectados
aux <- which((infectados_2-infectados_1)==1)
# tomamos la mitad de los infectados y los "perdonamos"
aux <- aux[which(as.numeric(runif(length(aux))>0.5)==1)]
auxx <- which(infectados_1==1)
infectados_2 <- numeric(total_nodos)
infectados_2[c(auxx,aux)] <- 1

# multiplicacion
infectados_3 <- g_reg_mat%*%infectados_2
# vemos la ubicacion de los infectados
aux <- which((infectados_3-infectados_2)==1)
# tomamos la mitad de los infectados y los "perdonamos"
aux <- aux[which(as.numeric(runif(length(aux))>0.5)==1)]
auxx <- which(infectados_2==1)
infectados_3 <- numeric(total_nodos)
infectados_3[c(auxx,aux)] <- 1

# multiplicacion
infectados_4 <- g_reg_mat%*%infectados_3
# vemos la ubicacion de los infectados
aux <- which((infectados_4-infectados_3)==1)
# tomamos la mitad de los infectados y los "perdonamos"
aux <- aux[which(as.numeric(runif(length(aux))>0.5)==1)]
auxx <- which(infectados_3==1)
infectados_4 <- numeric(total_nodos)
infectados_4[c(auxx,aux)] <- 1

# multiplicacion
infectados_5 <- g_reg_mat%*%infectados_4
# vemos la ubicacion de los infectados
aux <- which((infectados_5-infectados_4)==1)
# tomamos la mitad de los infectados y los "perdonamos"
aux <- aux[which(as.numeric(runif(length(aux))>0.5)==1)]
auxx <- which(infectados_4==1)
infectados_5 <- numeric(total_nodos)
infectados_5[c(auxx,aux)] <- 1

# multiplicacion
infectados_6 <- g_reg_mat%*%infectados_5
# vemos la ubicacion de los infectados
aux <- which((infectados_6-infectados_5)==1)
# tomamos la mitad de los infectados y los "perdonamos"
aux <- aux[which(as.numeric(runif(length(aux))>0.5)==1)]
auxx <- which(infectados_5==1)
infectados_6 <- numeric(total_nodos)
infectados_6[c(auxx,aux)] <- 1

# multiplicacion
infectados_7 <- g_reg_mat%*%infectados_6
# vemos la ubicacion de los infectados
aux <- which((infectados_7-infectados_6)==1)
# tomamos la mitad de los infectados y los "perdonamos"
aux <- aux[which(as.numeric(runif(length(aux))>0.5)==1)]
auxx <- which(infectados_6==1)
infectados_7 <- numeric(total_nodos)
infectados_7[c(auxx,aux)] <- 1

# multiplicacion
infectados_8 <- g_reg_mat%*%infectados_7
# vemos la ubicacion de los infectados
aux <- which((infectados_8-infectados_7)==1)
# tomamos la mitad de los infectados y los "perdonamos"
aux <- aux[which(as.numeric(runif(length(aux))>0.5)==1)]
auxx <- which(infectados_7==1)
infectados_8 <- numeric(total_nodos)
infectados_8[c(auxx,aux)] <- 1

# infectados_2 <- g_reg_mat%*%infectados_1
# infectados_3 <- g_reg_mat%*%infectados_2
# infectados_4 <- g_reg_mat%*%infectados_3
# infectados_5 <- g_reg_mat%*%infectados_4

################################################################################

igraph_options(labels = NA)
par(mfrow=c(2,3))
par(mar = c(.2, .2, .2, .2))
#plot(g_reg,vertex.color=infectados_0,vertex.label=NA,vertex.size=6)
#plot(g_reg,vertex.color=ceiling(infectados_1/10000),vertex.label=NA,vertex.size=6)
#plot(g_reg,vertex.color=ceiling(infectados_2/10000),vertex.label=NA,vertex.size=6)
#plot(g_reg,vertex.color=ceiling(infectados_3/10000),vertex.label=NA,vertex.size=6)
#plot(g_reg,vertex.color=ceiling(infectados_4/10000),vertex.label=NA,vertex.size=6)
#plot(g_reg,vertex.color=ceiling(infectados_5/10000),vertex.label=NA,vertex.size=6)


################################################################################

par(mfrow=c(1,1))
par(mar = c(2, 2, 2, 2))

#plot(c(-10,10),c(0.5,0.5),lty=2,type='l',
#           ylim=c(0,1),xlim=c(-1,9))
#lines(c(-10,10),c(0.25,0.25),lty=2)
#lines(c(-10,10),c(0.75,0.75),lty=2)

# plot(0,sum(infectados_0)/length(infectados_0),ylim=c(0,1),xlim=c(-1,9),col=vecinos)
# #points(0,sum(infectados_0)/length(infectados_0),col=vecinos)
# points(1,sum(infectados_1)/length(infectados_1),col=vecinos)
# points(2,sum(infectados_2)/total_nodos,col=vecinos)
# points(3,sum(infectados_3)/total_nodos,col=vecinos)
# points(4,sum(infectados_4)/total_nodos,col=vecinos)
# points(5,sum(infectados_5)/total_nodos,col=vecinos)
# points(6,sum(infectados_6)/total_nodos,col=vecinos)
# points(7,sum(infectados_7)/total_nodos,col=vecinos)
# points(8,sum(infectados_8)/total_nodos,col=vecinos)

lines(0:8,
      c(sum(infectados_0),sum(infectados_1),sum(infectados_2),sum(infectados_3),
        sum(infectados_4),sum(infectados_5),sum(infectados_6),sum(infectados_7),
        sum(infectados_8))/total_nodos,type='o',col=vecinos)

sum(infectados_8)/total_nodos
