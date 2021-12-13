file = "Interpolation/PointSup.csv"
df = read.csv(file, header = T, sep = ",")
df$layer=NULL
df$path=NULL

names(df)
#To be updated based on the data names
coordNames=c("CoordX","CoordY")

names(df)[names(df) == coordNames[1]] <- 'X'
names(df)[names(df) == coordNames[2]] <- 'Y'

# Pour des besoins de calculs: pour chaque carr? on fait la moyenne des coordonn?es 
B1 = mean(df$X); B2 = mean(df$Y);B1;B2

cond = (df$X < B1) & (df$Y <B2);cond
