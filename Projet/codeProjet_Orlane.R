file = "H:/SIG/Projet/InterComp.csv"
inter = read.csv(file, header = F, dec = '.', sep = " ")
colnames(inter) <- c('X','Y',"DB")

file = "H:/SIG/Projet/PtsSupp.csv"
supp = read.csv(file, header = T, sep = ",")
supp$layer=NULL
supp$path=NULL



# On a trop de distance on réduit le nombre de point de l'interpolation
DD = dist(rbind(inter[1:100,1:2],supp[,2:3]))
M = as.matrix(DD)

# Pour des besoins de calculs: pour chaque carré on fait la moyenne des coordonnées 
B1 = mean(inter$X); B2 = mean(inter$Y)

cond = (inter$X < B1) & (inter$Y <B2)

as.matrix(dist(rbind(inter[cond,1:2],supp[cond,3:4])))
