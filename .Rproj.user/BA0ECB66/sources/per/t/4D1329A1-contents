# 1. Sous R , transformez ces coordonnees en dataset : Points. Puis calculez 
#les distances au moyen de la fonction dist.
NO2=read.csv("TD7/NO2.csv")
plot(NO2$X,NO2$Y)
#View(NO2)
DD=dist(cbind(NO2$X,NO2$Y))
length(DD)
#2. Transformez votre vecteur distances en matrice M.
M=as.matrix(DD,nrow(NO2),nrow(NO2));M
image(M)
hist(DD)
# 3. Fixez un seuil ( prendre au debut le quantile a 2.5 %. Mettez le resultat 
#dans une matrice W. Affectez 0 a la diagonale
rayon=quantile(DD,0.05)
image(M<rayon)
W=M<rayon
diag(W)=0
#4. Transformez votre matrice M en W qui somme a 1.
W=W/sum(W);sum(W)
#plot(table(apply(W,1,sum)))

# 5. Calculez l'expression donnee par Saporta pour le coefficient de Moran et 
# comparez a la valeur attendue sous l'hypothese H0

#I = t(Y)*W*Y / (t(Y)*N*Y)

#W = matrice de pondération nxn
#Les données Y sont les données Z centrées

Z=NO2$NO2;head(Z)

##Fonction de base pour centrer réduire
# ?scale #divise par n-1 au lieu de n
# Y=scale(Z);head(Y)

# #On peut aussi utiliser une PCA pour centrer en divisant par n
# library(FactoMineR)
# Y=PCA(Z, scale.unit = TRUE, ncp=2)$ind$coord;head(Y)

#En revenant à la formule théorique
n=dim(W)[1];n
Y=(Z-mean(Z))/(sd(Z)*sqrt((n-1)/n));Y

I = t(Y) %*% W %*% Y;I
#(t(Y)*N*Y) correspond à la variance de Y.
#Si on prend scale qui centre et réduit, la variance est de 1, donc 
#inutile de rajouter cette division.

Moran = function(X,Y,Z,rayon){
  n=length(Z)
  DD=dist(cbind(X,Y))
  M=as.matrix(DD,n,n)
  W=M<rayon
  diag(W)=0
  W=W/sum(W)
  Y=(Z-mean(Z))/(sd(Z)*sqrt((n-1)/n))
  t(Y) %*% W %*% Y
}
Moran(NO2$X,NO2$Y,NO2$NO2,rayon)

resu=NULL
for (k in 1:1000){
  NO2P=sample(NO2$NO2)
  resu=c(resu,Moran(NO2$X,NO2$Y,NO2P,rayon))
}
hist(resu)

#install.packages("ape")
library(ape)
Moran.I(NO2$NO2,W)

#Essayer autres matrices de voisinage, surtout pour le projet