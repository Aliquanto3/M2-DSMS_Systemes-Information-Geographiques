ComptePoints=read.csv(file = "TD6/ComptePoints.csv",sep=";")
#View(ComptePoints)
names(ComptePoints)

X=ComptePoint$NUMPOINTS

mean(X)
sqrt(var(X))/mean(X)

Obs=data.frame(table(X))
str(Obs)
#View(Obs)

Obs$X=as.numeric(Obs$X)
for (i in 0:max(X)){
  if (!i %in% Obs$X){
    Obs[nrow(Obs) + 1,] = c(i,0)
  }
}
Obs=Obs[order(Obs$X),]
#View(Obs)
Obs

J=length(Obs)
N=sum(table(X))

Theo = dpois(0:max(X),mean(X)) * N;Theo

Chi2=sum((Theo-Obs$Freq)^2/Theo);Chi2

qchisq(0.95,J)

1-pchisq(Chi2,J)
