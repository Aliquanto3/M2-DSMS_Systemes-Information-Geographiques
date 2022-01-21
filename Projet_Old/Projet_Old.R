##############################
#5e JDD pour le projet
dfE=read.csv("Projet/MontpellierE.csv")
summary(dfE)
plot(dfE$Long,dfE$Lat,main="MontpellierE")

df=dfE
DD=dist(cbind(df$Long,df$Lat))

par(mfrow=c(1,3))
N=dim(df)[1];N
length(DD)
#2. Transformez votre vecteur distances en matrice M.
M=as.matrix(DD,nrow(df),nrow(df))
image(M,main="Image of M")
hist(DD)
# 3. Fixez un seuil ( prendre au debut le quantile a 2.5 %. Mettez le resultat 
#dans une matrice W. Affectez 0 a la diagonale
rayon=quantile(DD,0.05)
image(M<rayon,main="Image of M<rayon")
W=M<rayon
diag(W)=0
#4. Transformez votre matrice M en W qui somme a 1.
W=W/sum(W);sum(W)

NV=10 #nombre de voisins fixé
#mettre 1 sur les NV plus petites distances
#0 sur les autres
N=dim(M)[1];N
NV=10
#ici dim(M)[1]=dim(M)[2]
for (i in 1:dim(M)[2]){
  V=M[i,]
  M[i,order(V)[1:NV]]=1
  M[i,order(V)[(NV+1):N]]=0
}


apply(M,1,sum)

par(mfrow=c(1,1))
image(M<rayon,main="Image of M<rayon")
W=M<rayon
diag(W)=0
for (i in 1:N){
  W[i,]=W[i,]/sum(W[i,])
}

apply(W,1,sum)
plot(table(apply(W,1,sum))) 
#il peut y avoir des 0 si des points n'ont pas de voisin


##########################
#4. Transformez votre matrice M en W qui somme a 1.
W=W/sum(W);sum(W)

# 5. Calculez l'expression donnee par Saporta pour le coefficient de Moran et 
# comparez a la valeur attendue sous l'hypothese H0

#I = t(Y)*W*Y / (t(Y)*N*Y)

#W = matrice de pondération nxn
#Les données Y sont les données Z centrées

Z=df$LDEN;head(Z)

#En revenant à la formule théorique
n=dim(W)[1];n
Y=(Z-mean(Z))/(sd(Z)*sqrt((n-1)/n));Y

I = t(Y) %*% W %*% Y;I

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

library(ape)
Moran.I(df$LDEN,W)

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

indiceMoran=function(W,Z){
  n=dim(W)[1]
  Z=(Z-mean(Z))/sd(Z)*sqrt((n-1)/n)
  
  t(Z) %*% W %*% Z
}
#Permutations
resuMoran=NULL
for (k in 1:1000){
  dfP=sample(df$LDEN)
  resuMoran=c(resuMoran,indiceMoran(W,dfP))
}
hist(resuMoran)

indiceGeary=function(Y,W,N){
  t(Y)*(N-W)*Y/(t(Y)*N*Y)
}
#Permutations
resuGeary=NULL
NN=diag(W)/(1/N)
for (k in 1:1000){
  dfP=sample(df$LDEN)
  resuGeary=c(resuGeary,indiceGeary(df$Lat,W,NN))
}
hist(resuGeary)