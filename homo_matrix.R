
#   CONSTRUCCION DE MATRICES DE ADYACENCIA DE REDES HOMOFILICAS

################################################################################

library(dplyr)

G <- read.csv('red_hemofilia.csv',header=F) %>% 
  as.matrix()

H <- read.csv('red_homfilia_2.csv',header=F) %>% 
  as.matrix()

H[is.na(H)] <- 0

eval_red <- eigen(H) 

plot(eval_red$values)
plot(eigen(G/5)$values)

HH <- graph_from_adjacency_matrix(H, mode = 'undirected', weighted = NULL,
                                  diag = TRUE, add.colnames = NULL, add.rownames = NA)

plot(HH,
     vertex.color=paste0('grey',100-round(100*infectados_0)),vertex.label=NA,vertex.size=6)

# H5 1000 vertices #####################################################################

H5 <- diag(1000)
for(i in 3:998){
  H5[i,(i-2):(i+2)] <- 1
}
H5[1,c(1:3,999,1000)] <- 1
H5[2,c(1:4,1000)] <- 1
H5[999,c(1,997:1000)] <- 1
H5[1000,c(1,2,998:1000)] <- 1
plot(H[1:20,1:20])
plot(H5[980:1000,980:1000])
plot(sort(eigen(H/5)$values))

plot(graph_from_adjacency_matrix(H5, mode = 'undirected', weighted = NULL,
                                 diag = TRUE, add.colnames = NULL, add.rownames = NA),
     vertex.label=NA,vertex.size=6)

H55 <- (H/5)%^%70
H55[H55==0] <- NA
plot(H55[20,],type='o')

# H5 100 vertices ##############################################################################

H5 <- diag(100)
for(i in 3:98){
  H5[i,(i-2):(i+2)] <- 1
}
H5[1,c(1:3,99,100)] <- 1
H5[2,c(1:4,100)] <- 1
H5[99,c(1,97:100)] <- 1
H5[100,c(1,2,98:100)] <- 1

plot(graph_from_adjacency_matrix(H5, mode = 'undirected', weighted = NULL,
                                 diag = TRUE, add.colnames = NULL, add.rownames = NA),
     vertex.label=NA,vertex.size=6)
plot(sort(eigen(H5/5)$values))
plot(H5/5)

# H7 1000 vertices ###############################################################################

aux <- read.csv('red_grado_7.csv',header=F) %>% 
  as.matrix()

H7 <- diag(1000)
for(i in 5:996){
  H7[i,(i-3):(i+3)] <- 1
}
H7[1:10,1:10] <- aux[1:10,1:10]
H7[991:1000,991:1000] <- aux[11:20,11:20]

plot(H7[1:20,1:20])
plot(H7[980:1000,980:1000])
plot(sort(eigen(H7/7)$values))

plot(e72,border=NA)

# H7 - matrices de sus eigenvectores ###############################################################################

eigen_7 <- eigen(H7/7)$vectors
eigen_7[1:10,1:10]

t(eigen_7)[2,]%*%eigen_7[,2]

e74 <- diag(1000)
for(i in 1:1000){
  for(j in 1:1000){
    e74[i,j] <- eigen_7[,4][i]*eigen_7[,4][j]
  }
}

e75 <- diag(1000)
e76 <- diag(1000)
e77 <- diag(1000)
e71 <- diag(1000)
e78 <- diag(1000)
for(i in 1:1000){
  for(j in 1:1000){
    e71[i,j] <- 0.001
    e75[i,j] <- eigen_7[,5][i]*eigen_7[,5][j]
    e76[i,j] <- eigen_7[,6][i]*eigen_7[,6][j]
    e77[i,j] <- eigen_7[,7][i]*eigen_7[,7][j]
    e78[i,j] <- eigen_7[,8][i]*eigen_7[,8][j]
  }
}

eigenval_7 <- eigen(H7/7)$values
e7_aprox <- (e71*eigenval_7[1])+(e72*eigenval_7[2])+
  (e73*eigenval_7[3])+(e74*eigenval_7[4])+
  (e75*eigenval_7[5])+(e76*eigenval_7[6])+
  (e77*eigenval_7[7])+(e78*eigenval_7[8])


write.csv(e7_aprox,'e7_aprox.csv')


# construccion de matriz P 500 - es una mat diag en bloques de 2x2 con un comp aleatorio ###############################################################################

P <- diag(500)
P[1:250,1:250] <- .5
P[251:500,251:500] <- .5
ruido <- rbeta(500*500,2,2)/2
for(i in 1:500){
  for(j in 1:500){
    P[i,j] <- P[i,j] + ruido[(i-1)*500 + j]
  }
}
eigenval_P <- eigen(P)$values
eigenvec_P <- eigen(P)$vectors
plot(Re(eigenval_P))
Re(eigenval_P[7])
eigenvec_P
str(eigenvec_P)
hist(Re(eigenvec_P[,1]))

# construccion de matriz V 50 - es una mat diag en bloques de 2x2 ###############################################################################

V <- diag(50)
V[1:25,1:25] <- 1/25
V[26:50,26:50] <- 1/25
plot(V)
eval_V <- eigen(V)$values
evec_V <- eigen(V)$vectors
plot(eval_V)
eval_V
V3 <- diag(50)
for(i in 1:50){
  for(j in 1:50){
    V3[i,j] <- evec_V[,3][i]*evec_V[,3][j]
  }
}
plot(V3)
plot(V2)
