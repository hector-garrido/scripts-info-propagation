
# install.packages('igraph')
# 
# adjm <- matrix(sample(0:1, 100, replace=TRUE, prob=c(0.9,0.1)), nc=10)
# g1 <- graph_from_adjacency_matrix( adjm ,mode='undirected')
# adjm <- matrix(sample(0:5, 100, replace=TRUE,
#                       prob=c(0.9,0.02,0.02,0.02,0.02,0.02)), nc=10)

library(igraph)

################################################################################

total_nodos <- 300
vecinos <- 4
porc_sat <- 0.2
beta <- 0.1

#g1 <- graph_from_adjacency_matrix( adjm ,mode='undirected')
g_reg <- sample_k_regular(total_nodos, vecinos, directed = FALSE, multiple = FALSE)
g_reg_mat <- as_adjacency_matrix(g_reg) %>% matrix(ncol=total_nodos)
g_reg_mat <-t(g_reg_mat + diag(total_nodos))/(vecinos+1)


#infectados_0 <- numeric(total_nodos)
#infectados_0[1:(total_nodos*porc_sat)] <- 1
infectados_0 <- runif(total_nodos)

infectados_1 <- g_reg_mat%*%infectados_0
infectados_2 <- g_reg_mat%*%infectados_1
infectados_3 <- g_reg_mat%*%infectados_2
infectados_4 <- g_reg_mat%*%infectados_3
infectados_5 <- g_reg_mat%*%infectados_4

################################################################################

igraph_options(labels = NA)
par(mfrow=c(3,2))
par(mar = c(.2, .2, .2, .2))
plot(g_reg,
     vertex.color=paste0('grey',100-round(100*infectados_0)),vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=paste0('grey',100-round(100*infectados_1)),vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=paste0('grey',100-round(100*infectados_2)),vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=paste0('grey',100-round(100*infectados_3)),vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=paste0('grey',100-round(100*infectados_4)),vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=paste0('grey',100-round(100*infectados_5)),vertex.label=NA,vertex.size=6)

################################################################################

colores <- c('red4','red2','white','green2','green4')

igraph_options(labels = NA)
par(mfrow=c(3,2))
par(mar = c(.2, .2, .2, .2))
plot(g_reg,
     vertex.color=colores[1+round(4*infectados_0)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(4*infectados_1)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(4*infectados_2)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(4*infectados_3)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(4*infectados_4)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(4*infectados_5)],vertex.label=NA,vertex.size=6)


colores <- c('red4','red2','green2','green4')

igraph_options(labels = NA)
par(mfrow=c(3,2))
par(mar = c(.2, .2, .2, .2))
plot(g_reg,
     vertex.color=colores[1+round(3*infectados_0)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(3*infectados_1)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(3*infectados_2)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(3*infectados_3)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(3*infectados_4)],vertex.label=NA,vertex.size=6)
plot(g_reg,
     vertex.color=colores[1+round(3*infectados_5)],vertex.label=NA,vertex.size=6)
