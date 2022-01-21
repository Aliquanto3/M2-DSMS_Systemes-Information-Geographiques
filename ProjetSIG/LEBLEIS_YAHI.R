##############################################
### Mise en evidence d'une structure spatiale
##############################################

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

#######################################################
### Interpolation par methode probabiliste ou krigeage
#######################################################

# Libraries
library(sp)
library(gstat)

##### IDW
# Data exported from QGIS after interpolations and conversion to centroids,
# from raster to vector to centroid, and X/Y coordinates added
IDW=read.csv("IDW_Centroid.csv")
names(IDW)

# Variogram model
BruitIDW=na.omit(IDW)
coordinates(BruitIDW)=c("X_Coord","Y_Coord")
bubble(BruitIDW,"DN")

VarioIDW=variogram(DN~1,BruitIDW)
plot(VarioIDW)

vgmIDW=fit.variogram(VarioIDW,vgm("Sph"))
geoIDW=gstat(formula = DN~1,locations=BruitIDW,model = vgmIDW)

VariModelIDW=vgm(psill=var(BruitIDW$DN),model="Sph",range=0.01,nugget = 1)
plot(VarioIDW,VariModelIDW,plot.numbers=T)

# Create sampling grid
x.rangeIDW=range(IDW$X_Coord,na.rm = T)
y.rangeIDW=range(IDW$Y_Coord,na.rm = T)
resolutionIDW=0.01
grdIDW=expand.grid(x = seq(from = x.rangeIDW[1], to = x.rangeIDW[2], by = resolutionIDW),
                   y = seq(from = y.rangeIDW[1],to = y.rangeIDW[2], by = resolutionIDW))
coordinates(grdIDW)=~x + y
gridded(grdIDW)=TRUE

# Krige
krigeIDW=krige(DN~1,BruitIDW,grdIDW,VariModelIDW)
krigeIDW
summary(krigeIDW)

##### PPV - Plus Proche Voisin
# Data exported from QGIS after interpolations and conversion to centroids,
# from raster to vector to centroid, and X/Y coordinates added
PPV=read.csv("PPV_Centroid.csv")
names(PPV)

# Variogram model
BruitPPV=na.omit(PPV)
coordinates(BruitPPV)=c("X_Coord","Y_Coord")
bubble(BruitPPV,"DN")

VarioPPV=variogram(DN~1,BruitPPV)
plot(VarioPPV)

vgmPPV=fit.variogram(VarioPPV,vgm("Sph"))
geoPPV=gstat(formula = DN~1,locations=BruitPPV,model = vgmPPV)

VariModelPPV=vgm(psill=var(BruitPPV$DN),model="Sph",range=0.01,nugget = 1)
plot(VarioPPV,VariModelPPV,plot.numbers=T)

# Create sampling grid
x.rangePPV=range(PPV$X_Coord,na.rm = T)
y.rangePPV=range(PPV$Y_Coord,na.rm = T)
resolutionPPV=0.01
grdPPV=expand.grid(x = seq(from = x.rangePPV[1], to = x.rangePPV[2], by = resolutionPPV),
                   y = seq(from = y.rangePPV[1],to = y.rangePPV[2], by = resolutionPPV))
coordinates(grdPPV)=~x + y
gridded(grdPPV)=TRUE

# Krige
krigePPV=krige(DN~1,BruitPPV,grdPPV,VariModelPPV)
krigePPV
summary(krigePPV)

plot(krigePPV, main="PPV")

##### TIN
# Data exported from QGIS after interpolations and conversion to centroids,
# from raster to vector to centroid, and X/Y coordinates added
TIN=read.csv("TIN_Centroid.csv")
names(TIN)

# Variogram model
BruitTIN=na.omit(TIN)
coordinates(BruitTIN)=c("X_Coord","Y_Coord")
bubble(BruitTIN,"DN")

VarioTIN=variogram(DN~1,BruitTIN)
plot(VarioTIN)

vgmTIN=fit.variogram(VarioTIN,vgm("Sph"))
geoTIN=gstat(formula = DN~1,locations=BruitTIN,model = vgmTIN)

VariModelTIN=vgm(psill=var(BruitTIN$DN),model="Sph",range=0.01,nugget = 1)
plot(VarioTIN,VariModelTIN,plot.numbers=T)

# Create sampling grid
x.rangeTIN=range(TIN$X_Coord,na.rm = T)
y.rangeTIN=range(TIN$Y_Coord,na.rm = T)
resolutionTIN=0.01
grdTIN=expand.grid(x = seq(from = x.rangeTIN[1], to = x.rangeTIN[2], by = resolutionTIN),
                   y = seq(from = y.rangeTIN[1],to = y.rangeTIN[2], by = resolutionTIN))
coordinates(grdTIN)=~x + y
gridded(grdTIN)=TRUE

# Krige
krigeTIN=krige(DN~1,BruitTIN,grdTIN,VariModelTIN)
krigeTIN
summary(krigeTIN)

plot(krigeTIN, main="TIN")
