##### 1. Calculez plusieurs matrices de voisinage. Detaillez les calculs.

df=read.csv("MontpellierE.csv")
summary(df)
plot(df$Long,df$Lat,main="MontpellierE")

DD=dist(cbind(df$Long,df$Lat))

par(mfrow=c(1,3))
N=dim(df)[1];N
length(DD)

M=as.matrix(DD,nrow(df),nrow(df))
image(M,main="Image of M")
hist(DD)

#W : Matrice de pondération
rayon=quantile(DD,0.05)
image(M<rayon,main="Image of M<rayon")
W=M<rayon
diag(W)=0
W=W/sum(W);sum(W)


##### 2. Calculez le coefficient de Geary et de Moran en utilisant vos matrices  
##### de voisinages et vos fonctions ecrites en R.

Geary = function(X,Y,Z,ray){
  n=length(Z)
  DD=dist(cbind(X,Y))
  M=as.matrix(DD,n,n)
  rayon=quantile(DD,ray)
  W=M<rayon
  
  D=matrix(rep(0,n*n),n,n)
  diag(D)=apply(W,1,sum)
  
  diag(W)=0
  W=W/sum(W)
  Y=(Z-mean(Z))/(sd(Z)*sqrt((n-1)/n))
  t(Y) %*% (D-W) %*% Y / sum(D-W)
}

Geary(df$Long,df$Lat,df$LDEN,ray=0.05)

###

Moran = function(X,Y,Z,ray){
  n=length(Z)
  DD=dist(cbind(X,Y))
  M=as.matrix(DD,n,n)
  rayon=quantile(DD,ray)
  W=M<rayon
  diag(W)=0
  W=W/sum(W)
  Y=(Z-mean(Z))/(sd(Z)*sqrt((n-1)/n))
  t(Y) %*% W %*% Y
}

Moran(df$Long,df$Lat,df$LDEN,ray=0.05)

##### 3. Effectuez un test de validite : H0 independance spatiale.
par(mfrow=c(1,1))

indiceMoran=function(W,Z){
  n=dim(W)[1]
  Z=(Z-mean(Z))/sd(Z)*sqrt((n-1)/n)
  
  t(Z) %*% W %*% Z
}

set.seed(123)
#Permutations
resuMoran=NULL
for (k in 1:1000){
  LDENP=sample(df$LDEN)
  resuMoran=c(resuMoran,indiceMoran(W,LDENP))
}
hist(resuMoran)
mean(resuMoran)

library(spdep)
# ?moran.test
# ?mat2listw
Mlistw=mat2listw(M, row.names = NULL, style="M")
moran.test(df$LDEN,Mlistw)

#La moyenne calculée par boucle aléatoire correspond à l'attente de la fonction
#du package spdep
