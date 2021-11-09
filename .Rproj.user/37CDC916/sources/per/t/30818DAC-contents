set.seed(1)

K=6
lambda=3.5

MatN=matrix(data = rpois(K*K,lambda),nrow=K,ncol=K);MatN

coordX=NULL
coordY=NULL
for (i in 1:K){
  for (j in 1:K){
    coordX=c(coordX,runif(MatN[i,j],i-1,i))
    coordY=c(coordY,runif(MatN[i,j],j-1,j))
  }
}

plot(coordX,coordY)

#install.packages("spatstat")
library(spatstat)
rectN=rpoispp(3.5,win=owin(c(0,K),c(0,K)))
plot(rectN)

#Calcul Ripley
Surf=K*K
N=length(coordX)
D=as.matrix(dist(c(coordX,coordY))) #sym et diagonale
image(D)
rayon=0.5
sort(D[1,])
sum(D[1,] < rayon)

S=0
for (i in 1:N){
  S=S+sum(D[i,]<rayon)
}
S
(Surf/(N*(N-1)))*S


Resu=NULL
for (rayon in seq(0.01,2,0.01)){
  S=0
  for (i in 1:N){
    S=S+sum(D[i,] < rayon)
  }
  Resu=c(Resu,(Surf/(N*(N-1)))*S)
}
plot(seq(0.01,2,0.01),Resu,pch=".")
lines(seq(0.01,2,0.01), pi*seq(0.01,2,0.01)^2,pch="+")
