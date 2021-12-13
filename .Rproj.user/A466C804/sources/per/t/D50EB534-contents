file = "Interpolation/OUTPUT_Interpole_IDW_high_def.csv"
df = read.csv(file, header = T, sep = " ")
names(df)=c("X","Y","DB")
Cond=df$X<quantile(df$X,0.25) & df$Y<quantile(df$Y,0.25)
IDWQ1=df[Cond,1:2]


file2 = "Interpolation/PointSup.csv"
df2 = read.csv(file2, header = T, sep = ",")
df2$layer=NULL
df2$path=NULL
names(df2)
#To be updated based on the data names
coordNames=c("CoordX","CoordY")
names(df2)[names(df2) == coordNames[1]] <- 'X'
names(df2)[names(df2) == coordNames[2]] <- 'Y'
Cond2=df2$X<quantile(df2$X,0.25) & df2$Y<quantile(df2$Y,0.25)
PtSupQ1=df2[Cond2,3:4]
names(PtSupQ1)=c("X","Y")

DD=as.matrix(dist(rbind(IDWQ1,PtSupQ1)))
image(DD)

plot(IDWQ1$X,IDWQ1$Y,col=2,pch="+")
points(PtSupQ1$X,PtSupQ1$Y,col="blue",pch="+")
#On trouverait un seul point avec un quantile à 0.15 pour ces données ?

file3 = "Interpolation/MontpellierE.csv"
Bruit = read.csv(file3, header = T, sep = ",")
summary(Bruit)
hist(Bruit$LDEN)
var(Bruit$LDEN)

DBM=(1.2)^Bruit$LDEN

library(sp)


BruitM=as.data.frame(cbind(Bruit,DBM))
coordinates(Bruit)=c("Long","Lat")
coordinates(BruitM)=c("Long","Lat")
bubble(Bruit,"LDEN")

library(gstat)

MonVarioExp=variogram(LDEN~1,BruitM,alpha=c(0,45,90,135))
plot(MonVarioExp)

vgmX=fit.variogram(MonVarioExp,vgm("Sph"))
geoX=gstat(formula = LDEN~1,locations=BruitM,model = vgmX)

MonVarioExp=variogram(LDEN~1,Bruit)
VariModel=vgm(psill=var(Bruit$LDEN),model="Sph",range=0.03,nugget = 20)
plot(MonVarioExp,VariModel,plot.numbers=T)


